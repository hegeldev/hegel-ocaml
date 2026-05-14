(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

open! Core

module Variables = struct
  type 'a t = {
    tc : Client.test_case;
    pool_id : int;
    values : (int, 'a) Hashtbl.t;
  }

  let create tc =
    let pool_id = Client.new_pool tc in
    { tc; pool_id; values = Hashtbl.create (module Int) }

  let add t value =
    let variable_id = Client.pool_add t.tc ~pool_id:t.pool_id in
    Hashtbl.set t.values ~key:variable_id ~data:value

  let size t = Hashtbl.length t.values
  let is_empty t = Hashtbl.is_empty t.values

  let pick t ~consume =
    Client.assume t.tc (not (is_empty t));
    let variable_id =
      Client.pool_generate t.tc ~pool_id:t.pool_id ~consume ()
    in
    match Hashtbl.find t.values variable_id with
    | Some v ->
        if consume then Hashtbl.remove t.values variable_id;
        v
    | None ->
        (* Server returned an id we don't know about. This indicates either a
           flaky strategy (state diverged between the engine and the client)
           or a bug in the variables bookkeeping. *)
        raise Client.Flaky_strategy

  let draw t = pick t ~consume:false
  let consume t = pick t ~consume:true
end

module Rule = struct
  type 'state t = { name : string; step : Client.test_case -> 'state -> 'state }

  let create ~name ~step = { name; step }
  let name t = t.name
end

module Settings = struct
  type t = { max_steps : int }

  let default = { max_steps = 50 }
end

let run ?(settings = Settings.default) ~init ~rules ?(invariants = []) tc =
  match rules with
  | [] -> invalid_arg "Cannot run a state machine with no rules."
  | _ ->
      let rule_generator = Generators.sampled_from rules in
      let run_invariants state =
        List.iter invariants ~f:(fun inv -> inv state)
      in
      run_invariants init;
      let step_cap =
        Generators.draw tc
          (Generators.integers ~min_value:1
             ~max_value:settings.Settings.max_steps ())
      in
      let should_continue ~num_steps_succeeded ~num_steps =
        num_steps_succeeded < step_cap
        && (num_steps < 10 * step_cap
           || (num_steps_succeeded = 0 && num_steps < 1000))
      in
      let try_step ~state ~num_steps =
        Client.start_span ~label:Generators.Labels.stateful_rule tc;
        try
          let rule = Generators.draw tc rule_generator in
          if tc.Client.is_final then
            Client.note tc
              (Printf.sprintf "    Step %d: %s" (num_steps + 1) rule.Rule.name);
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
        if should_continue ~num_steps_succeeded ~num_steps then
          let next_state, num_steps_succeeded =
            match try_step ~state ~num_steps with
            | `Stepped new_state -> (new_state, num_steps_succeeded + 1)
            | `Rejected -> (state, num_steps_succeeded)
          in
          loop ~state:next_state ~num_steps_succeeded ~num_steps:(num_steps + 1)
        else if num_steps_succeeded = 0 then Client.assume tc false
      in
      loop ~state:init ~num_steps_succeeded:0 ~num_steps:0
