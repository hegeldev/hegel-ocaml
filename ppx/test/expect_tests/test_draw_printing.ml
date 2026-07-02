(** On the final replay of a failing test ([is_final = true]) an outermost
    [draw] prints its value through the [note] channel (stderr, captured here by
    ppx_expect). [draw_silent] never prints, and draws nested inside a span
    (depth > 0) are suppressed so only the outermost value shows. The printer is
    carried on primitive generators; combinators that hand the output type to
    user code ([map], [sampled_from]) carry none, and [with_printer] attaches
    one (and is what [draw] requires). *)

open! Core
open Hegel
open Generators

(* ---- draw / draw_silent through the engine ---- *)

(* Quiet, deterministic run; swallow the failure the property raises so the
   expect block only sees what we printed. *)
let run_failing
      ?(settings = settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal)
      body
  =
  try Hegel.run_hegel_test ~settings body with
  | _ -> ()
;;

let%expect_test "draw_silent returns the value and prints nothing" =
  Hegel.run_hegel_test
    ~settings:(settings ~test_cases:1 () |> with_verbosity Normal)
    (fun tc ->
       let v = Hegel.draw_silent tc (integers ~min_value:3 ~max_value:3 ()) in
       printf "got=%d" v);
  [%expect {| got=3 |}]
;;

let%expect_test "labeled draw prints name = value on final replay" =
  run_failing (fun tc ->
    let _ = Hegel.draw ~label:"x" tc (integers ~min_value:7 ~max_value:7 ()) in
    assert false);
  [%expect
    {|
    x = 7
    |}]
;;

let%expect_test "unlabeled draw is auto-named draw_N on final replay" =
  run_failing (fun tc ->
    let _ = Hegel.draw tc (integers ~min_value:123456 ~max_value:123456 ()) in
    assert false);
  [%expect
    {|
    draw_1 = 123456
    |}]
;;

let%expect_test "successive unlabeled draws number draw_1, draw_2" =
  run_failing (fun tc ->
    let _ = Hegel.draw tc (integers ~min_value:1 ~max_value:1 ()) in
    let _ = Hegel.draw tc (integers ~min_value:2 ~max_value:2 ()) in
    assert false);
  [%expect
    {|
    draw_1 = 1
    draw_2 = 2
    |}]
;;

let%expect_test "with_printer supplies the printer draw renders with" =
  run_failing (fun tc ->
    let _ =
      Hegel.draw
        ~label:"h"
        tc
        (with_printer
           (fun n -> Sexp.Atom (sprintf "0x%x" n))
           (integers ~min_value:255 ~max_value:255 ()))
    in
    assert false);
  [%expect
    {|
    h = 0xff
    |}]
;;

let%expect_test "with_printer makes an unprintable sampled_from drawable" =
  run_failing (fun tc ->
    let _ = Hegel.draw ~label:"c" tc (with_printer Int.sexp_of_t (sampled_from [ 9 ])) in
    assert false);
  [%expect
    {|
    c = 9
    |}]
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
  [%expect
    {|
    ran
    |}]
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
  [%expect
    {|
    pair = (7 8)
    |}]
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
  [%expect
    {|
    xs = (7 7)
    |}]
;;

let%expect_test "a stateful rule's args print; the step-cap draw stays silent" =
  let rule =
    Stateful.Rule.create ~name:"push" ~step:(fun tc _state ->
      let _ = Hegel.draw ~label:"n" tc (integers ~min_value:7 ~max_value:7 ()) in
      assert false)
  in
  run_failing (fun tc -> Stateful.run ~init:() ~rules:[ rule ] tc);
  [%expect
    {|
    Step 1: push
    n = 7 
    |}]
;;

let%hegel_test stateful_print tc =
  let rule =
    Stateful.Rule.create ~name:"push" ~step:(fun tc _state ->
      let _n = Hegel.draw tc (integers ~min_value:7 ~max_value:7 ()) in
      ())
  in
  let vars = Stateful.Pool.create tc in
  Stateful.Pool.add vars 42;
  let val_gen = with_printer sexp_of_int (Stateful.Pool.values_reusable vars) in
  let _x = Hegel.draw tc val_gen in
  Stateful.run ~init:() ~rules:[ rule ] tc
[@@settings
  settings ~test_cases:1 ~seed:0 ()
  |> with_verbosity Verbose
  |> with_phases [ Generate ]
  |> with_stateful_step_count 3]
;;

let%expect_test "stateful tests prints drawn data on passing test verbosity is verbose" =
  stateful_print ();
  [%expect
    {|
    Starting phase: Generate
    _x = 42
    Step 1: push
    _n_1 = 7
    Step 2: push
    _n_2 = 7
    Step 3: push
    _n_3 = 7
    Ending phase: Generate
    |}]
;;

(* indent inner draws in a step *)

(* [@@deriving hegel_generator] emits an unprintable generator value. Adding
   [@@deriving sexp_of] and drawing through [with_printer] prints the whole value
   as one sexp; a bare [@@deriving hegel_generator] drawn with [draw_silent] prints
   nothing. *)

type only = Only [@@deriving sexp_of, hegel_generator]
type wrap = { tag : only } [@@deriving sexp_of, hegel_generator]
type bare = Bare [@@deriving hegel_generator]

let%expect_test "a derived value prints as one sexp via with_printer" =
  run_failing (fun tc ->
    let _ = Hegel.draw tc (with_printer sexp_of_only only_generator) in
    assert false);
  [%expect
    {|
    draw_1 = Only
    |}]
;;

let%expect_test "a nested derived record prints as one sexp via with_printer" =
  run_failing (fun tc ->
    let _ = Hegel.draw tc (with_printer sexp_of_wrap wrap_generator) in
    assert false);
  [%expect
    {|
    draw_1 = ((tag Only))
    |}]
;;

let%expect_test
    "a bare [@@deriving hegel_generator] drawn with draw_silent prints nothing"
  =
  run_failing (fun tc ->
    let _ = Hegel.draw_silent tc bare_generator in
    assert false);
  (* Header only — no [@@deriving sexp_of], so nothing to print. *)
  [%expect {|  |}]
;;

(* ---- ppx_hegel_test label injection (end-to-end) ----

   Inside a [let%hegel_test] body, a draw bound to a simple variable —
   [let x = draw tc g] — is rewritten by the PPX to [draw ~label:"x" tc g], so
   the value prints as [x = value] (not a bare value) on the failing replay.
   This file enables the [ppx_hegel_test] rewriter, so the test below exercises
   a real expansion. *)

let%hegel_test label_injection_from_binding (tc : test_case) =
  let x = Hegel.draw tc (integers ~min_value:7 ~max_value:7 ()) in
  ignore (x : int);
  assert false
[@@settings settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "ppx injects ~label from the binding name" =
  (try label_injection_from_binding () with
   | _ -> ());
  [%expect
    {| 
    x = 7
    |}]
;;

(* The receiver decides what is rewritten, not the module path: a draw on [tc]
   qualified by any Hegel-exporting module (here [Generators]) is labeled, and
   the rewrite keeps that prefix (targeting [Generators.draw_named]). *)

let%hegel_test qualified_generators_draw (tc : test_case) =
  let g = Generators.draw tc (integers ~min_value:9 ~max_value:9 ()) in
  ignore (g : int);
  assert false
[@@settings settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a Generators-qualified draw is labeled (prefix preserved)" =
  (try qualified_generators_draw () with
   | _ -> ());
  [%expect
    {|   
    g = 9
    |}]
;;

(* A local function also named [draw] that is not Hegel's is left untouched,
   because the rewrite keys off the receiver (the draw must be applied to the
   test's own [tc]). Here [draw 5] is not applied to [tc], so it gets no
   label and the user's own [draw] runs; only the genuine [Hegel.draw tc] is
   labeled. *)

let%hegel_test local_draw_not_on_tc_untouched (tc : test_case) =
  let draw n = n + 100 in
  let y = draw 5 in
  ignore (y : int);
  let z = Hegel.draw tc (integers ~min_value:1 ~max_value:1 ()) in
  ignore (z : int);
  assert false
[@@settings settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a local non-Hegel draw (not on tc) is not rewritten" =
  (try local_draw_not_on_tc_untouched () with
   | _ -> ());
  (* No [y = …] line: [draw 5] was left alone; only [z], a real Hegel draw on
     [tc], is labeled. *)
  [%expect
    {|
    z = 1
    |}]
;;

(* When a binding name is drawn more than once, the PPX flags it repeatable, so
   every occurrence is numbered ([x_1], [x_2], …) — including the first — rather
   than the lone-binding bare [x]. *)

let%hegel_test repeated_binding_numbers (tc : test_case) =
  let x = Hegel.draw tc (integers ~min_value:1 ~max_value:1 ()) in
  ignore (x : int);
  let x = Hegel.draw tc (integers ~min_value:2 ~max_value:2 ()) in
  ignore (x : int);
  let x = Hegel.draw tc (integers ~min_value:3 ~max_value:3 ()) in
  ignore (x : int);
  assert false
[@@settings settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a reused binding name numbers x_1, x_2, x_3" =
  (try repeated_binding_numbers () with
   | _ -> ());
  [%expect
    {|
    x_1 = 1
    x_2 = 2
    x_3 = 3
    |}]
;;

(* A draw inside a loop (block depth > 0) is flagged repeatable even though the
   name appears once syntactically, so its per-iteration values are numbered. *)

let%hegel_test looped_binding_numbers (tc : test_case) =
  for i = 1 to 2 do
    let x = Hegel.draw tc (integers ~min_value:i ~max_value:i ()) in
    ignore (x : int)
  done;
  assert false
[@@settings settings ~test_cases:20 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a draw inside a loop numbers x_1, x_2" =
  (try looped_binding_numbers () with
   | _ -> ());
  [%expect
    {|
    x_1 = 1
    x_2 = 2
    |}]
;;

(* ---- verbose verbosity prints draws on a non-final case ----

   At [Normal]/[Quiet] verbosity a passing test prints nothing, since printing
   is gated on the failing final replay. A single case keeps the snapshot clean: 
   the engine emits its own [Running test case] lines between cases under verbose, 
   which a multi-case run would capture. *)

let%expect_test "verbose prints draws on a passing run" =
  Hegel.run_hegel_test
    ~settings:(settings ~test_cases:1 ~seed:0 () |> with_verbosity Verbose)
    (fun tc ->
       let _ = Hegel.draw tc (integers ~min_value:5 ~max_value:5 ()) in
       ());
  [%expect
    {|
    Starting phase: Generate
    draw_1 = 5
    Ending phase: Generate
    |}]
;;
