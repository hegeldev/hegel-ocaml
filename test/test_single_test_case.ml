open! Core
open Hegel
open Client
module G = Hegel.Generators

let single_settings () =
  Client.default_settings () |> Client.with_mode Client.Single_test_case
;;

let test_runs_exactly_one_case () =
  let count = ref 0 in
  Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    let (_ : int) =
      Hegel.draw
        tc
        (G.integers
           ~min_value:Int32.(to_int_exn min_value)
           ~max_value:Int32.(to_int_exn max_value)
           ())
    in
    incr count);
  Alcotest.(check int) "ran exactly one case" 1 !count
;;

let test_passing () =
  Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    let (_ : bool) = Hegel.draw tc (G.booleans ()) in
    ())
;;

let test_failing_propagates () =
  let raised_msg = ref "" in
  (try
     Session.run_hegel_test ~settings:(single_settings ()) (fun _tc ->
       failwith "deliberate failure")
   with
   | e -> raised_msg := Exn.to_string e);
  Alcotest.(check bool)
    "exception carries the original message"
    true
    (String.is_substring !raised_msg ~substring:"deliberate failure")
;;

let test_no_shrinking () =
  let count = ref 0 in
  let raised = ref false in
  (try
     Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
       let (_ : int) =
         Hegel.draw
           tc
           (G.integers
              ~min_value:Int32.(to_int_exn min_value)
              ~max_value:Int32.(to_int_exn max_value)
              ())
       in
       incr count;
       failwith "always fails")
   with
   | _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  Alcotest.(check int) "ran exactly once (no shrinking)" 1 !count
;;

let test_with_seed_is_deterministic () =
  let values = ref [] in
  for _ = 1 to 3 do
    let value = ref 0 in
    Session.run_hegel_test
      ~settings:(single_settings () |> Client.with_seed (Some 42))
      (fun tc ->
         value
         := Hegel.draw
              tc
              (G.integers
                 ~min_value:Int32.(to_int_exn min_value)
                 ~max_value:Int32.(to_int_exn max_value)
                 ()));
    values := !value :: !values
  done;
  match !values with
  | [ a; b; c ] ->
    Alcotest.(check int) "seed 42 reproducible (run 2 == run 1)" a b;
    Alcotest.(check int) "seed 42 reproducible (run 3 == run 2)" b c
  | _ -> Alcotest.fail "expected 3 values"
;;

let test_assume_produces_invalid () =
  Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    let n = Hegel.draw tc (G.integers ~min_value:(-100) ~max_value:100 ()) in
    assume tc (n > 0))
;;

let test_generation_works () =
  Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    let v =
      Hegel.draw
        tc
        (G.lists
           (G.integers
              ~min_value:Int32.(to_int_exn min_value)
              ~max_value:Int32.(to_int_exn max_value)
              ())
           ())
    in
    let (_ : int) = List.length v in
    ())
;;

let test_debug_verbosity () =
  Session.run_hegel_test
    ~settings:(single_settings () |> Client.with_verbosity Client.Debug)
    (fun tc ->
       let (_ : bool) = Hegel.draw tc (G.booleans ()) in
       ())
;;

let test_stateful_single_mode_unbounded_steps () =
  let module S = Hegel.Stateful in
  let step_count = ref 0 in
  let step_rule =
    S.Rule.create ~name:"step" ~step:(fun _tc state ->
      incr step_count;
      if !step_count >= 200 then failwith "reached 200 steps";
      state)
  in
  let raised_msg = ref "" in
  (try
     Session.run_hegel_test ~settings:(single_settings ()) (fun tc ->
       S.run ~init:() ~rules:[ step_rule ] tc)
   with
   | e -> raised_msg := Exn.to_string e);
  Alcotest.(check bool)
    "exception carries the original message"
    true
    (String.is_substring !raised_msg ~substring:"reached 200 steps");
  Alcotest.(check int) "ran exactly 200 steps" 200 !step_count
;;

let tests =
  let open Alcotest in
  [ test_case "runs exactly one case" `Quick test_runs_exactly_one_case
  ; test_case "passing" `Quick test_passing
  ; test_case "failing propagates" `Quick test_failing_propagates
  ; test_case "no shrinking" `Quick test_no_shrinking
  ; test_case "with seed is deterministic" `Quick test_with_seed_is_deterministic
  ; test_case "assume produces invalid" `Quick test_assume_produces_invalid
  ; test_case "generation works" `Quick test_generation_works
  ; test_case "debug verbosity" `Quick test_debug_verbosity
  ; test_case
      "stateful single mode unbounded steps"
      `Quick
      test_stateful_single_mode_unbounded_steps
  ]
;;
