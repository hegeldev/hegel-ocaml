(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

module Int_table = Generators.Int_table
module Pool_gen = Generators.Make_pool (Int_table)

module Pool = struct
  type 'a t =
    { tc : Internal.test_case
    ; pool : Internal.pool
    ; values : 'a Int_table.t
    }

  let create tc =
    let pool = Internal.new_pool tc in
    { tc; pool; values = Int_table.create 16 }
  ;;

  let add t value =
    let variable_id = Internal.pool_add t.tc ~pool:t.pool in
    Int_table.replace t.values variable_id value
  ;;

  let size t = Int_table.length t.values
  let values_consumed t = Pool_gen.pool_values ~pool:t.pool ~values:t.values ~consume:true

  let values_reusable t =
    Pool_gen.pool_values ~pool:t.pool ~values:t.values ~consume:false
  ;;
end

module Rule = struct
  type 'state t =
    { name : string
    ; step : Internal.test_case -> 'state -> 'state
    }

  let create ~name ~step = { name; step }
  let name t = t.name
end

let run ~init ~rules ?(invariants = []) ?sexp_of_state tc =
  let rule_array = Array.of_list rules in
  let invariant_names =
    List.mapi (fun i _ -> Printf.sprintf "invariant_%d" i) invariants
  in
  let state_machine =
    Internal.new_state_machine tc ~rule_names:(List.map Rule.name rules) ~invariant_names
  in
  let print_state state =
    Option.iter
      (fun sexp_of ->
         Internal.note
           tc
           (Stdlib.Format.asprintf "state = %a" Sexplib0.Sexp.pp_hum (sexp_of state)))
      sexp_of_state
  in
  let check_invariants ~where ~sample state =
    List.iteri
      (fun i inv ->
         if
           (not sample)
           || Internal.state_machine_should_check_invariant
                tc
                ~state_machine
                ~invariant_index:i
         then (
           match inv state with
           | () -> ()
           | exception e ->
             Internal.note tc (Printf.sprintf "Invariant %d violated %s." i where);
             raise e))
      invariants
  in
  print_state init;
  check_invariants ~where:"in the initial state" ~sample:false init;
  let rec exec_round ~state ~steps_attempted ~rejected =
    match Internal.state_machine_next_rule tc ~state_machine with
    | None -> state, steps_attempted, rejected
    | Some rule_index ->
      let rule = rule_array.(rule_index) in
      let step_num = steps_attempted + 1 in
      Internal.note tc (Printf.sprintf "Step %d: %s" step_num rule.Rule.name);
      (match Internal.with_note_indent tc (fun () -> rule.Rule.step tc state) with
       | new_state ->
         print_state new_state;
         exec_round ~state:new_state ~steps_attempted:step_num ~rejected
       | exception Internal.Assume_rejected ->
         Internal.state_machine_rule_rejected tc ~state_machine;
         Internal.note tc "Rule stopped early due to violated assumption.";
         exec_round ~state ~steps_attempted:step_num ~rejected:true)
  in
  let rec loop ~state ~steps_attempted =
    Internal.start_span ~label:Generators.Ppx_internal.Labels.stateful_rule tc;
    if Internal.state_machine_next_round tc ~state_machine
    then (
      let state, steps_attempted, rejected =
        try exec_round ~state ~steps_attempted ~rejected:false with
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
  Fun.protect
    ~finally:(fun () -> Internal.state_machine_free tc ~state_machine)
    (fun () ->
       let final_state = loop ~state:init ~steps_attempted:0 in
       check_invariants ~where:"in the final state" ~sample:false final_state)
;;
