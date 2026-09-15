(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

module Pool = Stateful_seq.Pool
module Concurrent_pool = Stateful_concurrent.Pool
module Rule = Stateful_seq.Rule
module Concurrent_rule = Stateful_concurrent.Rule

module Invariant = struct
  type 'state t =
    { name : string
    ; inv : Internal.test_case -> 'state -> unit
    ; always_check : bool
    }

  let create ~name ~inv ?(always_check = false) () = { name; inv; always_check }
  let name invariant = invariant.name
end

(* [section tc f] runs a rule or invariant body. It keeps a block of [tc]
   alive only for the step. *)
let section tc f =
  if Internal.should_print tc then Internal.with_block tc ~indent:2 f else f tc
;;

let check_invariants tc ~state_machine ~invariants ~where ~sample state =
  Array.iteri
    (fun i invariant ->
       if
         (not sample)
         || Internal.state_machine_should_check_invariant
              tc
              ~state_machine
              ~invariant_index:i
       then (
         match section tc (fun tc -> invariant.Invariant.inv tc state) with
         | () -> ()
         | exception e ->
           Internal.note
             tc
             (Printf.sprintf "Invariant %s violated %s." (Invariant.name invariant) where);
           raise e))
    invariants
;;

let run_machine
      ~init
      ~rules
      ~concurrent
      ~min_concurrency
      ~max_concurrency
      ~invariants
      ?sexp_of_state
      ?(step_count = 50)
      tc
  =
  let rules = Array.of_list rules in
  let invariants = Array.of_list invariants in
  let rule_names = Array.map Rule.name rules in
  let rule_groups = Array.map Concurrent_rule.group rules in
  let group_names = Stateful_concurrent.group_names rule_groups in
  let group_idxs =
    if concurrent
    then Stateful_concurrent.group_ids rule_groups group_names
    else Array.map (fun _ -> 0) rules
  in
  let state_machine, num_workers =
    Internal.new_state_machine_with_concurrency
      tc
      ~rule_names
      ~rule_groups:group_idxs
      ~invariant_names:(Array.map Invariant.name invariants)
      ~invariants_always_check:
        (Array.map (fun inv -> inv.Invariant.always_check) invariants)
      ~step_count
      ~min_concurrency
      ~max_concurrency
  in
  let print_state state =
    Option.iter
      (fun sexp_of -> Internal.print_line tc [ Text "state = "; Value (sexp_of state) ])
      sexp_of_state
  in
  let run_round ?(worker_index = 0) ?(steps_attempted = 0) state tc =
    let rec loop state steps_attempted rejected =
      match
        Internal.state_machine_next_rule_for_worker tc ~state_machine ~worker_index
      with
      | None -> state, steps_attempted, rejected
      | Some rule_index ->
        let rule = rules.(rule_index) in
        let step_num = steps_attempted + 1 in
        let heading =
          if concurrent
          then Printf.sprintf "Rule: %s" rule.name
          else Printf.sprintf "Step %d: %s" step_num rule.name
        in
        Internal.note tc heading;
        (match section tc (fun tc -> rule.step tc state) with
         | new_state ->
           if not concurrent then print_state new_state;
           loop new_state step_num rejected
         | exception Internal.Assume_rejected ->
           Internal.state_machine_rule_rejected_for_worker tc ~state_machine ~worker_index;
           Internal.note tc "Rule stopped early due to violated assumption.";
           loop state step_num true)
    in
    loop state steps_attempted false
  in
  let check_invariants = check_invariants tc ~state_machine ~invariants in
  let announce_checks which =
    if Array.length invariants > 0
    then Internal.note tc (Printf.sprintf "Checking invariants on the %s state." which)
  in
  Fun.protect
    ~finally:(fun () -> Internal.state_machine_free tc ~state_machine)
    (fun () ->
       if num_workers > 1
       then Internal.note tc (Printf.sprintf "Concurrency level: %d" num_workers);
       print_state init;
       announce_checks "initial";
       check_invariants ~where:"in the initial state" ~sample:false init;
       let final_state =
         if concurrent
         then
           Stateful_concurrent.run
             tc
             ~state_machine
             ~num_workers
             ~group_names
             ~state:init
             ~run_round:(fun ~worker_index tc -> run_round ~worker_index init tc)
             ~print_state
             ~check_invariants
         else
           Stateful_seq.run
             tc
             ~state_machine
             ~init
             ~run_round:(fun ~steps_attempted state ->
               run_round ~steps_attempted state tc)
             ~check_invariants
       in
       announce_checks "final";
       check_invariants ~where:"in the final state" ~sample:false final_state)
;;

let run_internal ~init ~rules ~invariants ?sexp_of_state ?step_count tc =
  run_machine
    ~init
    ~rules
    ~concurrent:false
    ~min_concurrency:1
    ~max_concurrency:1
    ~invariants
    ?sexp_of_state
    ?step_count
    tc
;;

module type State_machine = sig
  type state

  val rules : state Rule.t list
  val invariants : state Invariant.t list
end

let run
      (type s)
      ?step_count
      ?sexp_of_state
      tc
      (module M : State_machine with type state = s)
      ~(init : s)
  =
  run_internal ~init ~rules:M.rules ~invariants:M.invariants ?sexp_of_state ?step_count tc
;;

module type Concurrent_state_machine = State_machine

let run_concurrent
      (type s)
      ?step_count
      ?sexp_of_state
      tc
      (module M : Concurrent_state_machine with type state = s)
      ~(init : s)
      ~min_concurrency
      ~max_concurrency
  =
  run_machine
    ~init
    ~rules:M.rules
    ~concurrent:true
    ~min_concurrency
    ~max_concurrency
    ~invariants:M.invariants
    ?sexp_of_state
    ?step_count
    tc
;;
