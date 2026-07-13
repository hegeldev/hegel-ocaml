open! Core
open Hegel
open Generators

(* swallow the failure the property raises so the expect block only sees what we printed. *)
let run_failing body =
  try
    Hegel.run_hegel_test
      ~settings:(settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal)
      body
  with
  | _ -> ()
;;

let%hegel_test binding_name tc =
  let f_gen =
    functions
      ~sexp_of_arg:sexp_of_int
      ~returns:(integers ~min_value:0 ~max_value:1000 ())
      ()
  in
  let f = draw_silent tc f_gen in
  assert (f 42 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a function is named from its binding" =
  (try binding_name () with
   | _ -> ());
  [%expect
    {|
    f 42 = 10
    |}]
;;

let%hegel_test binding_name_inline tc =
  let f =
    draw_silent
      tc
      (functions
         ~sexp_of_arg:sexp_of_int
         ~returns:(integers ~min_value:0 ~max_value:1000 ())
         ())
  in
  assert (f 42 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a function drawn inline is also named from its binding" =
  (try binding_name_inline () with
   | _ -> ());
  [%expect
    {|
    f 42 = 10
    |}]
;;

let%hegel_test explicit_name_beats_binding tc =
  let f =
    draw_silent
      tc
      (functions
         ~name:"chosen"
         ~sexp_of_arg:sexp_of_int
         ~returns:(integers ~min_value:0 ~max_value:1000 ())
         ())
  in
  assert (f 42 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "an explicit ~name wins over the draw-site binding name" =
  (try explicit_name_beats_binding () with
   | _ -> ());
  [%expect
    {|
    chosen 42 = 10
    |}]
;;

let%expect_test "without a binding the function falls back to its default name" =
  run_failing (fun tc ->
    let f =
      draw_silent
        tc
        (functions
           ~sexp_of_arg:sexp_of_int
           ~returns:(integers ~min_value:0 ~max_value:1000 ())
           ())
    in
    assert (f 42 < 10));
  [%expect
    {|
    function 42 = 10
    |}]
;;

let%expect_test "an explicit ~name is used when there is no binding name" =
  run_failing (fun tc ->
    let f =
      draw_silent
        tc
        (functions
           ~name:"myfun"
           ~sexp_of_arg:sexp_of_int
           ~returns:(integers ~min_value:0 ~max_value:1000 ())
           ())
    in
    assert (f 42 < 10));
  [%expect
    {|
    myfun 42 = 10
    |}]
;;

let%hegel_test functions2_binding tc =
  let g =
    draw_silent
      tc
      (functions2
         ~sexp_of_arg1:sexp_of_int
         ~sexp_of_arg2:sexp_of_bool
         ~returns:(integers ~min_value:0 ~max_value:1000 ())
         ())
  in
  assert (g 3 true < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "functions2 shows its table uncurried, named from its binding" =
  (try functions2_binding () with
   | _ -> ());
  [%expect
    {|
    g (3 true) = 10
    |}]
;;

let%hegel_test functions3_binding tc =
  let h =
    draw_silent
      tc
      (functions3
         ~sexp_of_arg1:sexp_of_int
         ~sexp_of_arg2:sexp_of_bool
         ~sexp_of_arg3:sexp_of_int
         ~returns:(integers ~min_value:0 ~max_value:1000 ())
         ())
  in
  assert (h 1 true 2 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "functions3 shows its table uncurried, named from its binding" =
  (try functions3_binding () with
   | _ -> ());
  [%expect
    {|
    h (1 true 2) = 10
    |}]
;;

(* The PPX injects [~name] into every [draw_silent] binding, but it is ignored
   for non-function generators, which stay silent. *)
let%hegel_test draw_silent_scalar_stays_silent tc =
  let n = draw_silent tc (integers ~min_value:5 ~max_value:5 ()) in
  ignore (n : int);
  assert false
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a scalar drawn with draw_silent prints nothing even when named" =
  (try draw_silent_scalar_stays_silent () with
   | _ -> ());
  [%expect {| |}]
;;

let%hegel_test printable_function_draw tc =
  let f =
    draw
      tc
      (with_printer
         (fun _ -> Sexp.Atom "<fun>")
         (functions
            ~sexp_of_arg:sexp_of_int
            ~returns:(integers ~min_value:0 ~max_value:1000 ())
            ()))
  in
  assert (f 42 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a printable function generator prints with its sexp_of" =
  (try printable_function_draw () with
   | _ -> ());
  [%expect
    {|
    f = <fun>
    f 42 = 10
    |}]
;;

(* Drawn nested (inside a span, draw depth > 0) a function still gets its label
   threaded into its pairs — here [inner], numbered because it is drawn inside a
   thunk — but its [<fun>] value line is suppressed like any nested draw. It is
   applied at the top level, so its pair still shows. *)
let%hegel_test printable_function_drawn_nested tc =
  let f =
    group Labels.list tc (fun () ->
      draw
        tc
        (with_printer
           (fun _ -> Sexp.Atom "<fun>")
           (functions
              ~name:"f"
              ~sexp_of_arg:sexp_of_int
              ~returns:(integers ~min_value:0 ~max_value:1000 ())
              ())))
  in
  assert (f 42 < 10)
[@@settings settings ~test_cases:100 ~seed:0 () |> with_verbosity Normal]
;;

let%expect_test "a function drawn nested keeps its label but suppresses its value line" =
  (try printable_function_drawn_nested () with
   | _ -> ());
  [%expect
    {|
    f 42 = 10
    |}]
;;

(* A function applied inside a span (draw depth > 0) does not print its pair,
   like any nested draw. Here [f] is applied only inside the [group], so nothing
   is shown even though the property fails. *)
let%hegel_test application_inside_span_is_suppressed tc =
  let f =
    draw_silent
      tc
      (functions
         ~sexp_of_arg:sexp_of_int
         ~returns:(integers ~min_value:0 ~max_value:1000 ())
         ())
  in
  let r = group Labels.list tc (fun () -> f 42) in
  ignore (r : int);
  assert false
[@@settings settings ~test_cases:100 ~seed:0 ()]
;;

let%expect_test "a function applied inside a span prints nothing" =
  (try application_inside_span_is_suppressed () with
   | _ -> ());
  [%expect {| |}]
;;

let%hegel_test partially_printable_args_and_ret tc =
  let f =
    draw_silent
      tc
      (functions
         ~sexp_of_arg:sexp_of_int
         ~returns:(functions ~returns:(integers ()) ())
         ())
  in
  let n = f 1 () in
  assert (n = 0)
[@@settings settings ~test_cases:100 ~seed:0 ()]
;;

let%expect_test "partially printable applications" =
  (try partially_printable_args_and_ret () with
   | _ -> ());
  [%expect
    {|
    f 1 = <opaque>
    function <opaque> = 1
    |}]
;;

let%hegel_test unprintable_args_and_ret tc =
  let f = draw_silent tc (functions ~returns:(functions ~returns:(integers ()) ()) ()) in
  let n = f () () in
  assert (n = 0)
[@@settings settings ~test_cases:100 ~seed:0 ()]
;;

let%expect_test "unprintable applications" =
  (try unprintable_args_and_ret () with
   | _ -> ());
  [%expect
    {|
    f <opaque> = <opaque>
    function <opaque> = 1
    |}]
;;
