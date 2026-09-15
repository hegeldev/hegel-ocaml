(** Stateful property-based testing for Hegel. See [stateful.mli]. *)

module Int_table = Generators.Int_table
module Pool_gen = Generators.Make_pool (Int_table)

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

module Concurrent_pool = struct
  type 'a t = 'a Pool_gen.t

  let create ?(clone = Fun.id) tc =
    Pool_gen.create tc ~clone ~lock:(Some (Mutex.create ()))
  ;;

  let add = Pool_gen.add
  let is_empty = Pool_gen.is_empty
  let size = Pool_gen.size
  let values_reusable t = Pool_gen.pool_values t ~consume:false
  let values_consumed t = Pool_gen.pool_values t ~consume:true
end

module Rule = struct
  type 'state t =
    { name : string
    ; step : Internal.test_case -> 'state -> 'state
    }

  let create ~name ~step = { name; step }
  let name t = t.name
end

module Concurrent_rule = struct
  type 'state t =
    { name : string
    ; group : string
    ; step : Internal.test_case -> 'state -> unit
    }

  let create ?(group = "<anonymous>") ~name ~step () = { name; group; step }
  let name t = t.name
  let group t = t.group
end

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
  List.iteri
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

let run_sequential_steps tc ~state_machine ~rules ~init ~print_state ~check_invariants =
  let rec exec_round ~state ~steps_attempted ~rejected =
    match Internal.state_machine_next_rule tc ~state_machine with
    | None -> state, steps_attempted, rejected
    | Some rule_index ->
      let rule = rules.(rule_index) in
      let step_num = steps_attempted + 1 in
      Internal.note tc (Printf.sprintf "Step %d: %s" step_num rule.Rule.name);
      (match section tc (fun tc -> rule.Rule.step tc state) with
       | new_state ->
         print_state new_state;
         exec_round ~state:new_state ~steps_attempted:step_num ~rejected
       | exception Internal.Assume_rejected ->
         Internal.state_machine_rule_rejected tc ~state_machine;
         Internal.note tc "Rule stopped early due to violated assumption.";
         exec_round ~state ~steps_attempted:step_num ~rejected:true)
  in
  let rec loop ~state ~steps_attempted =
    Internal.start_span ~label:Generators.Private.Labels.stateful_rule tc;
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
  loop ~state:init ~steps_attempted:0

let run_worker_round
      ~worker_index
      ~tc
      ~state
      ~(rules : _ Concurrent_rule.t array)
      ~state_machine
  =
  let rec loop () =
    match Internal.state_machine_next_rule_for_worker tc ~state_machine ~worker_index with
    | None -> ()
    | Some rule_index ->
      let rule = rules.(rule_index) in
      Internal.note tc (Printf.sprintf "Rule: %s" rule.Concurrent_rule.name);
      (match section tc (fun tc -> rule.Concurrent_rule.step tc state) with
       | () -> loop ()
       | exception Internal.Assume_rejected ->
         Internal.state_machine_rule_rejected_for_worker tc ~state_machine ~worker_index;
         section tc (fun tc ->
           Internal.note tc "Rule stopped early due to violated assumption.");
         loop ())
  in
  loop ()
;;

let run_machine
      ~init
      ~rule_names
      ~rule_groups
      ~min_concurrency
      ~max_concurrency
      ~invariants
      ~invariant_message
      ~run_steps
      ?sexp_of_state
      ?(step_count = 50)
      tc
  =
  let state_machine, concurrency =
    Internal.new_state_machine_with_concurrency
      tc
      ~rule_names
      ~rule_groups
      ~invariant_names:(List.map Invariant.name invariants)
      ~invariants_always_check:
        (List.map (fun inv -> inv.Invariant.always_check) invariants)
      ~step_count
      ~min_concurrency
      ~max_concurrency
  in
  let print_state state =
    Option.iter
      (fun sexp_of -> Internal.print_line tc [ Text "state = "; Value (sexp_of state) ])
      sexp_of_state
  in
  let check_invariants = check_invariants tc ~state_machine ~invariants in
  let announce_checks which =
    if not (List.is_empty invariants) then Internal.note tc (invariant_message which)
  in
  Fun.protect
    ~finally:(fun () -> Internal.state_machine_free tc ~state_machine)
    (fun () ->
       if concurrency > 1
       then Internal.note tc (Printf.sprintf "Concurrency level: %d" concurrency);
       print_state init;
       announce_checks "initial";
       check_invariants ~where:"in the initial state" ~sample:false init;
       let final_state =
         run_steps ~state_machine ~concurrency ~print_state ~check_invariants
       in
       announce_checks "final";
       check_invariants ~where:"in the final state" ~sample:false final_state)
;;

let run_internal ~init ~rules ~invariants ?sexp_of_state ?step_count tc =
  run_machine
    ~init
    ~rule_names:(List.map Rule.name rules)
    ~rule_groups:(List.map (fun _ -> 0) rules)
    ~min_concurrency:1
    ~max_concurrency:1
    ~invariants
    ~invariant_message:(Printf.sprintf "Checking invariants on the %s state.")
    ~run_steps:(fun ~state_machine ~concurrency:_ ~print_state ~check_invariants ->
      run_sequential_steps
        tc
        ~state_machine
        ~rules:(Array.of_list rules)
        ~init
        ~print_state
        ~check_invariants)
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

module type Concurrent_state_machine = sig
  type state

  val rules : state Concurrent_rule.t list
  val invariants : state Invariant.t list
end

let run_concurrent
      (type s)
      ?step_count
      tc
      (module M : Concurrent_state_machine with type state = s)
      ~(init : s)
      ~min_concurrency
      ~max_concurrency
  =
  let rules = Array.of_list M.rules in
  let rule_groups, group_names = group_ids rules in
  run_machine
    ~init
    ~rule_names:(Array.to_list (Array.map Concurrent_rule.name rules))
    ~rule_groups
    ~min_concurrency
    ~max_concurrency
    ~invariants:M.invariants
    ~invariant_message:(fun which ->
      Printf.sprintf "%s invariant check." (String.capitalize_ascii which))
    ~run_steps:(fun ~state_machine ~concurrency ~print_state:_ ~check_invariants ->
      run_concurrent_steps
        tc
        ~state_machine
        ~concurrency
        ~rules
        ~group_names
        ~init
        ~check_invariants)
    ?step_count
    tc
;;
