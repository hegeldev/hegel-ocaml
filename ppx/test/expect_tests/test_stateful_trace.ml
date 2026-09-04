open! Core
open Hegel

(* Deterministic, database-disabled run so the [Falsified after N] count and the
   failure blob are stable; swallow the failure so the expect block only sees the
   report. *)
let run_failing body =
  let settings =
    settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal |> with_database Disabled
  in
  try Hegel.run_hegel_test ~settings body with
  | _ -> ()
;;

let%expect_test "state trace; invariant marks the failing step" =
  let inc = Stateful.Rule.create ~name:"inc" ~step:(fun _tc n -> n + 1) in
  run_failing (fun tc ->
    Stateful.run
      ~init:0
      ~rules:[ inc ]
      ~invariants:[ (fun n -> assert (n <= 1)) ]
      ~sexp_of_state:Int.sexp_of_t
      tc);
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 2 test cases (0 discarded):

      state = 0
      Step 1: inc
      state = 1
      Step 2: inc
      state = 2
      Invariant 0 violated after step 2.

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

let%expect_test "invariant violated in the initial state" =
  let noop = Stateful.Rule.create ~name:"noop" ~step:(fun _tc () -> ()) in
  run_failing (fun tc ->
    Stateful.run ~init:() ~rules:[ noop ] ~invariants:[ (fun () -> assert false) ] tc);
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

      Invariant 0 violated in the initial state.

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

(* A multi-rule stack: [?sexp_of_state] traces the whole stack after every step
   so a [pop] that violates the property shows the state that led there. *)
let%expect_test "state trace across multiple rules" =
  let push =
    Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
      let n = Hegel.draw tc (integers ~min_value:0 ~max_value:100 ()) in
      n :: stack)
  in
  let pop =
    Stateful.Rule.create ~name:"pop" ~step:(fun tc stack ->
      Hegel.assume tc (not (List.is_empty stack));
      match stack with
      | [] -> assert false
      | top :: rest ->
        assert (top < 50);
        rest)
  in
  run_failing (fun tc ->
    Stateful.run
      ~init:[]
      ~rules:[ push; pop ]
      ~sexp_of_state:(sexp_of_list sexp_of_int)
      tc);
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 2 test cases (0 discarded):

      state = ()
      Step 1: push
        draw_1 = 50
      state = (50)
      Step 2: pop

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;
