(** Expect tests for Phase 1 draw-value printing.

    On the final replay of a failing test ([is_final = true]) an outermost
    [draw] prints its value through the [note] channel (stderr, captured here by
    ppx_expect). [draw_silent] never prints, and draws nested inside a span
    (depth > 0) are suppressed so only the outermost value shows. The printer is
    carried on primitive generators (and overridable per-draw via [~sexp_of]);
    combinators that hand the output type to user code ([map], [sampled_from])
    carry no printer. *)

open! Core
open Hegel
open Generators

(* ---- draw / draw_silent through the engine ---- *)

(* Quiet, deterministic run; swallow the failure the property raises so the
   expect block only sees what we printed. *)
let run_failing body =
  let settings = Client.(settings ~test_cases:20 ~seed:0 () |> with_verbosity Quiet) in
  try Hegel.run_hegel_test ~settings body with
  | _ -> ()
;;

let%expect_test "draw_silent returns the value and prints nothing" =
  Hegel.run_hegel_test
    ~settings:Client.(settings ~test_cases:1 () |> with_verbosity Quiet)
    (fun tc ->
       let v = Hegel.draw_silent tc (integers ~min_value:3 ~max_value:3 ()) in
       printf "got=%d" v);
  [%expect {| got=3 |}]
;;

let%expect_test "labeled draw prints name = value on final replay" =
  run_failing (fun tc ->
    let _ = Hegel.draw ~label:"x" tc (integers ~min_value:7 ~max_value:7 ()) in
    assert false);
  [%expect {| x = 7 |}]
;;

let%expect_test "unlabeled draw prints the bare value on final replay" =
  run_failing (fun tc ->
    let _ = Hegel.draw tc (integers ~min_value:123456 ~max_value:123456 ()) in
    assert false);
  [%expect {| 123456 |}]
;;

let%expect_test "explicit ~sexp_of overrides the carried printer" =
  run_failing (fun tc ->
    let _ =
      Hegel.draw
        ~label:"h"
        ~sexp_of:(fun n -> Sexp.Atom (sprintf "0x%x" n))
        tc
        (integers ~min_value:255 ~max_value:255 ())
    in
    assert false);
  [%expect {| h = 0xff |}]
;;

let%expect_test "draw nested in a span (depth > 0) is suppressed" =
  run_failing (fun tc ->
    let _ =
      group Labels.list tc (fun () ->
        Hegel.draw ~label:"n" tc (integers ~min_value:7 ~max_value:7 ()))
    in
    Hegel.note tc "ran";
    assert false);
  (* Only the outermost draw should print; the nested one is suppressed. *)
  [%expect {| ran |}]
;;

let%expect_test "a tuple draw prints as one sexp" =
  run_failing (fun tc ->
    let _ =
      Hegel.draw
        ~label:"pair"
        tc
        (tuples2
           (integers ~min_value:7 ~max_value:7 ())
           (integers ~min_value:8 ~max_value:8 ()))
    in
    assert false);
  [%expect {| pair = (7 8) |}]
;;

let%expect_test "a list draw prints as one sexp" =
  run_failing (fun tc ->
    let _ =
      Hegel.draw
        ~label:"xs"
        tc
        (lists (integers ~min_value:7 ~max_value:7 ()) ~min_size:2 ~max_size:2 ())
    in
    assert false);
  [%expect {| xs = (7 7) |}]
;;
