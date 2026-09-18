module Pool_gen = Generators.Make_pool (Generators.Int_table)

module Pool = struct
  type 'a t =
    { tc : Internal.test_case
    ; data : 'a Pool_gen.t
    }

  (* A rule body's [tc] is a block freed when the step ends,
     and a pool created there must keep working in later steps. The clone is
     owned by the test case and lives until it completes. *)
  let create ?(clone = Fun.id) tc =
    let tc = Internal.clone tc in
    { tc; data = Pool_gen.create tc ~clone ~lock:None }
  ;;

  let add t value = Pool_gen.add t.data t.tc value
  let size t = Pool_gen.size t.data
  let values_reusable t = Pool_gen.pool_values t.data ~consume:false
  let values_consumed t = Pool_gen.pool_values t.data ~consume:true
end

module Rule = struct
  type 'state t =
    { name : string
    ; group : string
    ; step : Internal.test_case -> 'state -> 'state
    }

  let create ~name ~step = { name; group = "<anonymous>"; step }
  let name t = t.name
end

let run tc ~state_machine ~init ~run_round ~check_invariants =
  let rec loop ~state ~steps_attempted =
    Internal.start_span ~label:Generators.Private.Labels.stateful_rule tc;
    if Internal.state_machine_next_round tc ~state_machine
    then (
      let state, steps_attempted, rejected =
        try run_round ~steps_attempted state with
        | e ->
          Internal.stop_span tc;
          raise e
      in
      Internal.stop_span ~discard:rejected tc;
      check_invariants
        ~where:(Printf.sprintf "after step %d" steps_attempted)
        ~sample:true
        state;
      loop ~state ~steps_attempted)
    else (
      Internal.stop_span tc;
      state)
  in
  loop ~state:init ~steps_attempted:0
;;
