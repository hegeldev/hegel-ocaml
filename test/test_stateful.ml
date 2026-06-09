open Core

(* Stateful failure test: the [push] rule pushes an int in [0, 100] onto a
   stack; the [pop] rule fails when the popped value is >= 50. Should shrink to [push 50; pop]. *)
let stateful_failure_test () =
  let module S = Hegel.Stateful in
  let last_pop = ref None in
  let push_rule =
    S.Rule.create ~name:"push" ~step:(fun tc stack ->
      let n = Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ()) in
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
  (try
     Hegel.run_hegel_test ~settings:(Hegel.settings ~seed:0 ()) (fun tc ->
       S.run ~init:[] ~rules:[ push_rule; pop_rule ] tc);
     failwith "expected property to fail"
   with
   | Assert_failure _ -> ());
  Alcotest.(check (option int)) "last pop value" (Some 50) !last_pop
;;

(* Stateful variables test: an [alloc]/[free] register allocator. [alloc]
   draws a fresh integer id, deposits it in the variables, and records it in a
   "live" set; [free] consumes an id from the variables and removes it from
   the set. Variables size must match the size of the live set. Empty-variables
   draws are rejected by [Variables.consume]'s internal [assume] call. *)

module Var_state = struct
  module S = Hegel.Stateful

  type t =
    { live : Int.Set.t
    ; variables : int S.Variables.t
    }
end

let var_next_id = ref 0

let var_alloc_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"alloc" ~step:(fun _tc state ->
    let id = !var_next_id in
    incr var_next_id;
    S.Variables.add state.Var_state.variables id;
    { state with Var_state.live = Set.add state.Var_state.live id })
;;

let var_free_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"free" ~step:(fun _tc state ->
    let id = S.Variables.consume state.Var_state.variables in
    assert (Set.mem state.Var_state.live id);
    { state with Var_state.live = Set.remove state.Var_state.live id })
;;

let var_use_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"use" ~step:(fun _tc state ->
    let id = S.Variables.draw state.Var_state.variables in
    assert (Set.mem state.Var_state.live id);
    state)
;;

let%hegel_test stateful_variables_test tc =
  let module S = Hegel.Stateful in
  var_next_id := 0;
  S.run
    ~init:{ Var_state.live = Int.Set.empty; variables = S.Variables.create tc }
    ~rules:[ var_alloc_rule; var_free_rule ]
    ~invariants:
      [ (fun state ->
          assert (
            S.Variables.size state.Var_state.variables = Set.length state.Var_state.live))
      ]
    tc
[@@settings Hegel.settings ~test_cases:10 ~seed:0 ()]
;;

let%hegel_test stateful_variables_draw_test tc =
  let module S = Hegel.Stateful in
  var_next_id := 0;
  S.run
    ~init:{ Var_state.live = Int.Set.empty; variables = S.Variables.create tc }
    ~rules:[ var_alloc_rule; var_use_rule ]
    tc
[@@settings Hegel.settings ~test_cases:5 ~seed:0 ()]
;;

let stateful_rule_name_test () =
  let module S = Hegel.Stateful in
  let rule = S.Rule.create ~name:"my_rule" ~step:(fun _tc s -> s) in
  Alcotest.(check string) "name" "my_rule" (S.Rule.name rule)
;;

let stateful_no_rules_test () =
  let raised_msg = ref "" in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
    try Hegel.Stateful.run ~init:() ~rules:[] tc with
    | Invalid_argument msg -> raised_msg := msg);
  Alcotest.(check bool)
    "has 'no rules' message"
    true
    (String.equal !raised_msg "Cannot run a state machine with no rules.")
;;

let always_reject_rule =
  let module S = Hegel.Stateful in
  S.Rule.create ~name:"reject" ~step:(fun tc s ->
    Hegel.assume tc false;
    s)
;;

let%hegel_test stateful_retry_budget_floor_test tc =
  Hegel.Stateful.run ~init:() ~rules:[ always_reject_rule ] tc
[@@settings
  Hegel.settings ~test_cases:1 ~seed:0 ()
  |> Hegel.Client.with_suppress_health_check [ Hegel.Client.Filter_too_much ]]
;;

(* Directly exercise [resolve_drawn]: the engine-unreachable [None] branch
   (unknown variable id), and the [Some] branch with and without [consume]. *)
let test_resolve_drawn () =
  let tbl = Hashtbl.create (module Int) in
  Hashtbl.set tbl ~key:7 ~data:"v";
  (* consume:false keeps the entry *)
  Alcotest.(check string)
    "draw"
    "v"
    (Hegel.Stateful.Variables.resolve_drawn tbl ~consume:false 7);
  Alcotest.(check int) "still present" 1 (Hashtbl.length tbl);
  (* consume:true removes it *)
  Alcotest.(check string)
    "consume"
    "v"
    (Hegel.Stateful.Variables.resolve_drawn tbl ~consume:true 7);
  Alcotest.(check int) "removed" 0 (Hashtbl.length tbl);
  (* unknown id raises Flaky_strategy *)
  let raised =
    try
      ignore (Hegel.Stateful.Variables.resolve_drawn tbl ~consume:false 99 : string);
      false
    with
    | Hegel.Client.Flaky_strategy -> true
  in
  Alcotest.(check bool) "unknown id raises Flaky_strategy" true raised
;;

let tests =
  [ Alcotest.test_case "stateful: resolve_drawn" `Quick test_resolve_drawn
  ; Alcotest.test_case "stateful: failing property shrinks" `Quick stateful_failure_test
  ; Alcotest.test_case
      "stateful: variables add/consume round-trips"
      `Quick
      stateful_variables_test
  ; Alcotest.test_case
      "stateful: variables draw (non-consuming)"
      `Quick
      stateful_variables_draw_test
  ; Alcotest.test_case "stateful: rule name accessor" `Quick stateful_rule_name_test
  ; Alcotest.test_case "stateful: empty rules raises" `Quick stateful_no_rules_test
  ; Alcotest.test_case
      "stateful: all-rejected test case is invalid"
      `Quick
      stateful_retry_budget_floor_test
  ]
;;
