open Core

(* Stateful failure test: the [push] rule pushes an int in [0, 100] onto a
   stack; the [pop] rule fails when the popped value is >= 50. Should shrink to
   [push 50; pop]. *)
let stateful_failure_test () =
  let module S = Hegel.Stateful in
  let last_pop = ref None in
  let push_rule =
    S.Rule.create ~name:"push" ~step:(fun tc stack ->
      let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:100 ()) in
      n :: stack)
  in
  let pop_rule =
    S.Rule.create ~name:"pop" ~step:(fun tc stack ->
      Hegel.assume tc (not (List.is_empty stack));
      match stack with
      | [] -> assert false
      | top :: rest ->
        last_pop := Some top;
        assert (top < 50);
        rest)
  in
  let module Stack = struct
    type state = int list

    let rules = [ push_rule; pop_rule ]
    let invariants = []
  end
  in
  (try
     Hegel.run_hegel_test ~settings:(Hegel.settings ~seed:0 ()) (fun tc ->
       S.run tc (module Stack) ~init:[]);
     failwith "expected property to fail"
   with
   | Assert_failure _ -> ());
  Alcotest.(check (option int)) "last pop value" (Some 50) !last_pop
;;

(* Stateful variables test: an [alloc]/[free] register allocator. [alloc] draws
   a fresh integer id, deposits it in the variables, and records it in a "live"
   set; [free] consumes an id from the variables and removes it from the set.
   Variables size must match the size of the live set. Empty-variables draws are
   rejected by [Pool.consume]'s internal [assume] call. *)

module Var_state = struct
  module S = Hegel.Stateful

  type t =
    { live : Int.Set.t
    ; variables : int S.Pool.t
    }
end

let var_next_id = ref 0

let var_alloc_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"alloc" ~step:(fun _tc state ->
    let id = !var_next_id in
    incr var_next_id;
    S.Pool.add state.Var_state.variables id;
    { state with Var_state.live = Set.add state.Var_state.live id })
;;

let var_free_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"free" ~step:(fun tc state ->
    let var_gen = S.Pool.values_consumed state.Var_state.variables in
    let id = Hegel.draw_silent tc var_gen in
    assert (Set.mem state.Var_state.live id);
    { state with Var_state.live = Set.remove state.Var_state.live id })
;;

let var_use_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"use" ~step:(fun tc state ->
    let var_gen = S.Pool.values_consumed state.Var_state.variables in
    let id = Hegel.draw_silent tc var_gen in
    assert (Set.mem state.Var_state.live id);
    state)
;;

let stateful_variables_test () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ~seed:0 ()) (fun tc ->
    let module S = Hegel.Stateful in
    var_next_id := 0;
    let module Allocator = struct
      type state = Var_state.t

      let rules = [ var_alloc_rule; var_free_rule ]

      let invariants =
        [ S.Invariant.create
            ~name:"pool_sz"
            ~inv:(fun _tc state ->
              assert (
                S.Pool.size state.Var_state.variables = Set.length state.Var_state.live))
            ()
        ]
      ;;
    end
    in
    S.run
      tc
      (module Allocator)
      ~init:{ Var_state.live = Int.Set.empty; variables = S.Pool.create tc })
;;

let stateful_variables_draw_test () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:5 ~seed:0 ()) (fun tc ->
    let module S = Hegel.Stateful in
    var_next_id := 0;
    let module Allocator = struct
      type state = Var_state.t

      let rules = [ var_alloc_rule; var_use_rule ]
      let invariants = []
    end
    in
    S.run
      tc
      (module Allocator)
      ~init:{ Var_state.live = Int.Set.empty; variables = S.Pool.create tc })
;;

let stateful_usage_error_test () =
  let module S = Hegel.Stateful in
  let attempts = ref 0 in
  let bad_rule =
    S.Rule.create ~name:"bad" ~step:(fun tc () ->
      incr attempts;
      ignore
        (Hegel.draw
           tc
           (Hegel.dates
              ~min_date:{ year = 2024; month = 1; day = 2 }
              ~max_date:{ year = 2024; month = 1; day = 1 }
              ())
         : string))
  in
  let module M = struct
    type state = unit

    let rules = [ bad_rule ]
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
      S.run tc (module M) ~init:())
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check bool)
      "engine diagnostic"
      true
      (String.is_substring msg ~substring:"generate_date requires min_value <= max_value");
    Alcotest.(check int) "rule attempted once" 1 !attempts
;;

let stateful_rule_name_test () =
  let module S = Hegel.Stateful in
  let rule = S.Rule.create ~name:"my_rule" ~step:(fun _tc s -> s) in
  Alcotest.(check string) "name" "my_rule" (S.Rule.name rule)
;;

let stateful_no_rules_test () =
  let module Empty = struct
    type state = unit

    let rules = []
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
      Hegel.Stateful.run tc (module Empty) ~init:())
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check string)
      "engine diagnostic"
      "cannot run a state machine with no rules"
      msg
;;

(* Pins the engine-side contract documented on [Internal.pool_generate]: drawing
   from an empty pool rejects the test case with [Assume_rejected], not
   [Data_exhausted]. *)
let empty_pool_draw_rejects_test () =
  match
    Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
      let pool = Hegel.Internal.new_pool tc in
      ignore (Hegel.Internal.pool_generate tc ~pool () : int))
  with
  | () -> Alcotest.fail "expected Unsatisfiable"
  | exception Failure msg ->
    Alcotest.(check bool)
      "failure msg"
      (String.is_substring_at msg ~pos:0 ~substring:"Unsatisfiable")
      true
;;

let stateful_step_count_forwarded_test () =
  let module S = Hegel.Stateful in
  let steps_this_case = ref 0 in
  let max_steps = ref 0 in
  let count_rule =
    S.Rule.create ~name:"count" ~step:(fun _tc () -> incr steps_this_case)
  in
  let module M = struct
    type state = unit

    let rules = [ count_rule ]
    let invariants = []
  end
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ~seed:0 ()) (fun tc ->
    steps_this_case := 0;
    S.run tc (module M) ~init:() ~step_count:5;
    max_steps := max !max_steps !steps_this_case);
  Alcotest.(check bool) "no case exceeded the configured cap" true (!max_steps <= 5)
;;

(* A step count below one is a usage error: the engine rejects it
   ([HEGEL_E_INVALID_ARG]) when the state machine is created, and the runner
   propagates it unshrunk, matching hegel-rust. *)
let stateful_step_count_below_one_test () =
  let module S = Hegel.Stateful in
  let module M = struct
    type state = unit

    let rules = [ S.Rule.create ~name:"noop" ~step:(fun _tc () -> ()) ]
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.settings ()) (fun tc ->
      S.run tc (module M) ~init:() ~step_count:0)
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check bool)
      "diagnostic names the constraint"
      true
      (String.is_substring msg ~substring:"step count must be at least 1")
;;

let test_stateful_bounded_steps () =
  let module S = Hegel.Stateful in
  let step_count = ref 0 in
  let step_rule =
    S.Rule.create ~name:"step" ~step:(fun _tc state ->
      incr step_count;
      if !step_count >= 10 then failwith "reached 10 steps";
      state)
  in
  let module M = struct
    type state = unit

    let rules = [ step_rule ]
    let invariants = []
  end
  in
  let raised_msg = ref "" in
  (try
     Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
       step_count := 0;
       S.run tc (module M) ~init:() ~step_count:10)
   with
   | e ->
     raised_msg := Exn.to_string e;
     Printf.printf "%s" !raised_msg);
  Alcotest.(check bool)
    "exception carries the original message"
    true
    (String.is_substring !raised_msg ~substring:"reached 10 steps");
  Alcotest.(check int) "ran exactly 10 steps" 10 !step_count
;;

let test_always_check_invariant () =
  let module S = Hegel.Stateful in
  let stateful_step_count = 10 in
  let always_inv_exec_count = ref 0 in
  let sampled_inv_exec_count = ref 0 in
  let noop = S.Rule.create ~name:"noop" ~step:(fun _tc _state -> ()) in
  let always_check_invariant =
    S.Invariant.create
      ~name:"always_check"
      ~inv:(fun _tc _ -> incr always_inv_exec_count)
      ~always_check:true
      ()
  in
  let sampled_invariant =
    S.Invariant.create
      ~name:"sampled_check"
      ~inv:(fun _tc _ -> incr sampled_inv_exec_count)
      ()
  in
  let module M = struct
    type state = unit

    let rules = [ noop ]
    let invariants = [ always_check_invariant; sampled_invariant ]
  end
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ~seed:1 ()) (fun tc ->
    S.run tc (module M) ~init:() ~step_count:stateful_step_count);
  Alcotest.(check int)
    "always exec count = executed steps plus endpoint checks"
    (stateful_step_count + 2)
    !always_inv_exec_count;
  Alcotest.(check bool)
    "sampled exec count < always exec count"
    (* not generally true but is true for the seed *)
    (!sampled_inv_exec_count < !always_inv_exec_count)
    true
;;

let test_swarm_long_single_rule_run () =
  let module S = Hegel.Stateful in
  (* Per-test-case state: the longest run of one identical rule choice in the
     current case. *)
  let case_longest = ref 0 in
  let last_rule = ref None in
  let current_run = ref 0 in
  let long_run_cases = ref 0 in
  let make i =
    S.Rule.create ~name:(Printf.sprintf "rule_%d" i) ~step:(fun _tc () ->
      (match !last_rule with
       | Some j when j = i -> incr current_run
       | _ -> current_run := 1);
      last_rule := Some i;
      if !current_run > !case_longest then case_longest := !current_run)
  in
  let module M = struct
    type state = unit

    let rules = List.init 11 ~f:make
    let invariants = []
  end
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:200 ~seed:0 ()) (fun tc ->
    (* Reset per test case so a run can't bleed across cases. *)
    last_rule := None;
    current_run := 0;
    case_longest := 0;
    S.run tc (module M) ~init:();
    if !case_longest >= 20 then incr long_run_cases);
  Alcotest.(check bool)
    "swarm produces a recurring long single-rule chain"
    true
    (!long_run_cases >= 5)
;;

let stateful_hand_written_machine_test () =
  let module S = Hegel.Stateful in
  let steps = ref 0 in
  let checks = ref 0 in
  let module Counter = struct
    type state = int

    let rules =
      [ S.Rule.create ~name:"bump" ~step:(fun _tc n ->
          incr steps;
          n + 1)
      ]
    ;;

    let invariants =
      [ S.Invariant.create
          ~name:"non_negative"
          ~inv:(fun _tc n ->
            incr checks;
            assert (n >= 0))
          ~always_check:true
          ()
      ]
    ;;
  end
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
    S.run tc (module Counter) ~init:0 ~step_count:5);
  Alcotest.(check bool) "ran at least one step" true (!steps >= 1);
  Alcotest.(check int)
    "always-check runs per step plus both endpoints"
    (!steps + 2)
    !checks
;;

let tests =
  [ Alcotest.test_case "stateful: failing property shrinks" `Quick stateful_failure_test
  ; Alcotest.test_case
      "stateful: run drives a hand-written State_machine"
      `Quick
      stateful_hand_written_machine_test
  ; Alcotest.test_case
      "stateful: variables add/consume round-trips"
      `Quick
      stateful_variables_test
  ; Alcotest.test_case
      "stateful: variables draw (non-consuming)"
      `Quick
      stateful_variables_draw_test
  ; Alcotest.test_case
      "stateful: usage error aborts the run"
      `Quick
      stateful_usage_error_test
  ; Alcotest.test_case "stateful: rule name accessor" `Quick stateful_rule_name_test
  ; Alcotest.test_case "stateful: empty rules raises" `Quick stateful_no_rules_test
  ; Alcotest.test_case
      "stateful: empty pool draw rejects"
      `Quick
      empty_pool_draw_rejects_test
  ; Alcotest.test_case
      "stateful: step count is forwarded to the engine"
      `Quick
      stateful_step_count_forwarded_test
  ; Alcotest.test_case
      "stateful: step count below one is rejected"
      `Quick
      stateful_step_count_below_one_test
  ; Alcotest.test_case
      "stateful: step_count bounds steps"
      `Quick
      test_stateful_bounded_steps
  ; Alcotest.test_case
      "stateful: always-check invariant runs after every step"
      `Quick
      test_always_check_invariant
  ; Alcotest.test_case
      "stateful: swarm yields a long single-rule chain"
      `Quick
      test_swarm_long_single_rule_run
  ]
;;
