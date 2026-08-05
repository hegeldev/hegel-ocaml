(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

module Int_table = Generators.Int_table
module Pool_gen = Generators.Make_pool (Int_table)

module Pool = struct
  type 'a t =
    { tc : Internal.test_case
    ; pool_id : int
    ; values : 'a Int_table.t
    }

  let create tc =
    let pool_id = Internal.new_pool tc in
    { tc; pool_id; values = Int_table.create 16 }
  ;;

  let add t value =
    let variable_id = Internal.pool_add t.tc ~pool_id:t.pool_id in
    Int_table.replace t.values variable_id value
  ;;

  let size t = Int_table.length t.values

  let values_consumed t =
    Pool_gen.pool_values ~pool_id:t.pool_id ~values:t.values ~consume:true
  ;;

  let values_reusable t =
    Pool_gen.pool_values ~pool_id:t.pool_id ~values:t.values ~consume:false
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
  match rules with
  | [] -> invalid_arg "Cannot run a state machine with no rules."
  | _ ->
    let rule_array = Array.of_list rules in
    let invariant_names =
      List.mapi (fun i _ -> Printf.sprintf "invariant_%d" i) invariants
    in
    let state_machine_id =
      Internal.new_state_machine
        tc
        ~rule_names:(List.map Rule.name rules)
        ~invariant_names
    in
    let print_state state =
      Option.iter
        (fun sexp_of ->
           Internal.note
             tc
             (Stdlib.Format.asprintf "state = %a" Sexplib0.Sexp.pp_hum (sexp_of state)))
        sexp_of_state
    in
    let check_invariants ~where state =
      List.iteri
        (fun i inv ->
           match inv state with
           | () -> ()
           | exception e ->
             Internal.note tc (Printf.sprintf "Invariant %d violated %s." i where);
             raise e)
        invariants
    in
    print_state init;
    check_invariants ~where:"in the initial state" init;
    let rec loop ~state ~steps_attempted =
      Internal.start_span ~label:Generators.Ppx_internal.Labels.stateful_rule tc;
      match Internal.state_machine_next_rule tc ~state_machine_id with
      | None -> ()
      | Some rule_index ->
        let rule = rule_array.(rule_index) in
        let step_num = steps_attempted + 1 in
        Internal.note tc (Printf.sprintf "Step %d: %s" step_num rule.Rule.name);
        (match Internal.with_note_indent tc (fun () -> rule.Rule.step tc state) with
         | new_state ->
           Internal.stop_span tc;
           print_state new_state;
           check_invariants ~where:(Printf.sprintf "after step %d" step_num) new_state;
           loop ~state:new_state ~steps_attempted:step_num
         | exception Internal.Assume_rejected ->
           Internal.stop_span ~discard:true tc;
           Internal.note tc "Rule stopped early due to violated assumption.";
           loop ~state ~steps_attempted:step_num
         | exception e ->
           Internal.stop_span tc;
           raise e)
    in
    loop ~state:init ~steps_attempted:0
;;
