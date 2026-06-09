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
    if t.tc.is_final || t.tc.verbose
    then
      Option.iter t.sexp_of ~f:(fun f ->
        Client.note
          t.tc
          (Printf.sprintf "v%d = %s" variable_id (Sexp.to_string (f value))));
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
    let rule_generator = Generators.sampled_from rules in
    let run_invariants state = List.iter invariants ~f:(fun inv -> inv state) in
    run_invariants init;
    let step_cap =
      (* [draw_silent]: the step count is engine bookkeeping, not a useful part
         of the printed counterexample. *)
      if is_single
      then Int.max_value
      else Generators.draw_silent tc (Generators.integers ~min_value:1 ~max_value:50 ())
    in
    let should_continue ~num_steps_succeeded ~num_steps =
      is_single
      || (num_steps_succeeded < step_cap
          && (num_steps < 10 * step_cap || (num_steps_succeeded = 0 && num_steps < 1000))
         )
    in
    let try_step ~state ~num_steps =
      Client.start_span ~label:Generators.Labels.stateful_rule tc;
      try
        let rule = Generators.draw_silent tc rule_generator in
        if tc.is_final || tc.verbose
        then Client.note tc (Printf.sprintf "Step %d: %s" (num_steps + 1) rule.Rule.name);
        let new_state = rule.Rule.step tc state in
        run_invariants new_state;
        Client.stop_span tc;
        `Stepped new_state
      with
      | Client.Assume_rejected ->
        Client.note tc "Rule stopped early due to violated assumption.";
        Client.stop_span ~discard:true tc;
        `Rejected
      | e ->
        Client.stop_span tc;
        raise e
    in
    let rec loop ~state ~num_steps_succeeded ~num_steps =
      if should_continue ~num_steps_succeeded ~num_steps
      then (
        let next_state, num_steps_succeeded =
          match try_step ~state ~num_steps with
          | `Stepped new_state -> new_state, num_steps_succeeded + 1
          | `Rejected -> state, num_steps_succeeded
        in
        loop ~state:next_state ~num_steps_succeeded ~num_steps:(num_steps + 1))
      else if num_steps_succeeded = 0
      then Client.assume tc false
    in
    loop ~state:init ~num_steps_succeeded:0 ~num_steps:0
;;
