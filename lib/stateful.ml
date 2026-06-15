(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

open! Core

module Variables = struct
  type 'a t =
    { tc : Client.test_case
    ; pool_id : int
    ; values : (int, 'a) Hashtbl.t
    ; sexp_of : ('a -> Sexp.t) option
    }

  let create ?sexp_of tc =
    let pool_id = Client.new_pool tc in
    { tc; pool_id; values = Hashtbl.create (module Int); sexp_of }
  ;;

  let add t value =
    let variable_id = Client.pool_add t.tc ~pool_id:t.pool_id in
    Hashtbl.set t.values ~key:variable_id ~data:value
  ;;

  let size t = Hashtbl.length t.values
  let is_empty t = Hashtbl.is_empty t.values

  (* Resolve a drawn variable id against the local table, removing it when
     [consume]. The [None] case is a server-contract violation (the engine
     returned an id we never registered) and is unreachable through the normal
     engine-driven path, so it is split out here to be unit-testable. *)
  let resolve_drawn values ~consume variable_id =
    match Hashtbl.find values variable_id with
    | Some v ->
      if consume then Hashtbl.remove values variable_id;
      v
    | None ->
      (* State diverged between the engine and the client, or a bug in the
         variables bookkeeping. *)
      raise Client.Flaky_strategy
  ;;

  let pick t ~consume =
    Client.assume t.tc (not (is_empty t));
    let variable_id = Client.pool_generate t.tc ~pool_id:t.pool_id ~consume () in
    let value = resolve_drawn t.values ~consume variable_id in
    Option.iter t.sexp_of ~f:(fun f ->
      Client.note t.tc (Printf.sprintf "v%d = %s" variable_id (Sexp.to_string (f value))));
    value
  ;;

  let draw t = pick t ~consume:false
  let consume t = pick t ~consume:true
end

module Rule = struct
  type 'state t =
    { name : string
    ; step : Client.test_case -> 'state -> 'state
    }

  let create ~name ~step = { name; step }
  let name t = t.name
end

let run ~init ~rules ?(invariants = []) tc =
  match rules with
  | [] -> invalid_arg "Cannot run a state machine with no rules."
  | _ ->
    let is_single =
      match tc.Client.mode with
      | Client.Single_test_case -> true
      | Test_run -> false
    in
    let rule_array = Array.of_list rules in
    let invariant_names =
      List.mapi invariants ~f:(fun i _ -> Printf.sprintf "invariant_%d" i)
    in
    let state_machine_id =
      Client.new_state_machine
        tc
        ~rule_names:(List.map rules ~f:Rule.name)
        ~invariant_names
    in
    let run_invariants state = List.iter invariants ~f:(fun inv -> inv state) in
    let max_steps = if is_single then Int.max_value else tc.Client.stateful_step_count in
    (* We basically always want to run the maximum number of steps, but leave a
       small probability of terminating early so the shrinker can reduce the
       step count once a failing case is found: stop with probability 2^-16
       during normal operation, and force a stop once enough steps have run. *)
    let rec loop ~state ~num_steps_succeeded ~steps_run =
      Client.start_span ~label:Generators.Labels.stateful_rule tc;
      let p_stop = 2.0 ** -16.0 in
      let must_stop =
        if is_single
        then Some false
        else if steps_run >= max_steps
        then Some true
        else if steps_run <= 0
        then Some false
        else None
      in
      (* Stop: mirror Hypothesis, which breaks out of the loop leaving this span
         open (the engine freezes it on completion), so the terminating stop
         boolean stays recorded inside its own span. *)
      if Client.primitive_boolean tc p_stop must_stop
      then (if num_steps_succeeded = 0 then Client.assume tc false)
      else (
        let next_state, num_steps_succeeded =
          try
            let rule = rule_array.(Client.state_machine_next_rule tc ~state_machine_id) in
            Client.note tc (Printf.sprintf "Step %d: %s" (steps_run + 1) rule.Rule.name);
            let new_state = rule.Rule.step tc state in
            run_invariants new_state;
            Client.stop_span tc;
            new_state, num_steps_succeeded + 1
          with
          | Client.Assume_rejected ->
            Client.note tc "Rule stopped early due to violated assumption.";
            Client.stop_span ~discard:true tc;
            state, num_steps_succeeded
          | e ->
            Client.stop_span tc;
            raise e
        in
        loop ~state:next_state ~num_steps_succeeded ~steps_run:(steps_run + 1))
    in
    loop ~state:init ~num_steps_succeeded:0 ~steps_run:0
;;
