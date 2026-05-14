open Core

(* Stateful failure test: the [push] rule pushes an int in [0, 100] onto a
   stack; the [pop] rule fails when the popped value is >= 50. Should shrink to [push 50; pop]. *)
let stateful_failure_test () =
  let module S = Hegel.Stateful in
  let last_pop = ref None in
  let push_rule =
    S.Rule.create ~name:"push" ~step:(fun tc stack ->
        let n =
          Hegel.draw tc
            (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
        in
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
   with Assert_failure _ -> ());
  Alcotest.(check (option int)) "last pop value" (Some 50) !last_pop

(* Stateful variables test: an [alloc]/[free] register allocator. [alloc]
   draws a fresh integer id, deposits it in the variables, and records it in a
   "live" set; [free] consumes an id from the variables and removes it from
   the set. Variables size must match the size of the live set. Empty-variables
   draws are rejected by [Variables.consume]'s internal [assume] call. *)

let stateful_variables_test () =
  let module S = Hegel.Stateful in
  let next_id = ref 0 in
  let module State = struct
    type t = { live : Int.Set.t; variables : int S.Variables.t }
  end in
  let alloc_rule =
    S.Rule.create ~name:"alloc" ~step:(fun _tc state ->
        let id = !next_id in
        incr next_id;
        S.Variables.add state.State.variables id;
        { state with State.live = Set.add state.State.live id })
  in
  let free_rule =
    S.Rule.create ~name:"free" ~step:(fun _tc state ->
        let id = S.Variables.consume state.State.variables in
        assert (Set.mem state.State.live id);
        { state with State.live = Set.remove state.State.live id })
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ~seed:0 ())
    (fun tc ->
      next_id := 0;
      S.run
        ~init:{ State.live = Int.Set.empty; variables = S.Variables.create tc }
        ~rules:[ alloc_rule; free_rule ]
        ~invariants:
          [
            (fun state ->
              assert (
                S.Variables.size state.State.variables
                = Set.length state.State.live));
          ]
        tc)

let stateful_variables_draw_test () =
  let module S = Hegel.Stateful in
  let next_id = ref 0 in
  let module State = struct
    type t = { live : Int.Set.t; variables : int S.Variables.t }
  end in
  let alloc_rule =
    S.Rule.create ~name:"alloc" ~step:(fun _tc state ->
        let id = !next_id in
        incr next_id;
        S.Variables.add state.State.variables id;
        { state with State.live = Set.add state.State.live id })
  in
  let use_rule =
    S.Rule.create ~name:"use" ~step:(fun _tc state ->
        let id = S.Variables.draw state.State.variables in
        assert (Set.mem state.State.live id);
        state)
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:5 ~seed:0 ())
    (fun tc ->
      next_id := 0;
      S.run
        ~init:{ State.live = Int.Set.empty; variables = S.Variables.create tc }
        ~rules:[ alloc_rule; use_rule ] tc)

let stateful_rule_name_test () =
  let module S = Hegel.Stateful in
  let rule = S.Rule.create ~name:"my_rule" ~step:(fun _tc s -> s) in
  Alcotest.(check string) "name" "my_rule" (S.Rule.name rule)

let stateful_no_rules_test () =
  let raised_msg = ref "" in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun tc ->
      try Hegel.Stateful.run ~init:() ~rules:[] tc
      with Invalid_argument msg -> raised_msg := msg);
  Alcotest.(check bool)
    "has 'no rules' message" true
    (String.equal !raised_msg "Cannot run a state machine with no rules.")

let stateful_retry_budget_floor_test () =
  let module S = Hegel.Stateful in
  let always_reject =
    S.Rule.create ~name:"reject" ~step:(fun tc s ->
        Hegel.assume tc false;
        s)
  in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ~seed:0 ())
    (fun tc -> S.run ~init:() ~rules:[ always_reject ] tc)

let tests =
  [
    Alcotest.test_case "stateful: failing property shrinks" `Quick
      stateful_failure_test;
    Alcotest.test_case "stateful: variables add/consume round-trips" `Quick
      stateful_variables_test;
    Alcotest.test_case "stateful: variables draw (non-consuming)" `Quick
      stateful_variables_draw_test;
    Alcotest.test_case "stateful: rule name accessor" `Quick
      stateful_rule_name_test;
    Alcotest.test_case "stateful: empty rules raises" `Quick
      stateful_no_rules_test;
    Alcotest.test_case "stateful: all-rejected test case is invalid" `Quick
      stateful_retry_budget_floor_test;
  ]
