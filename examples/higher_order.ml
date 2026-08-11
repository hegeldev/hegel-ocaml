(** Higher-order examples: generating functions as test inputs.

    Demonstrates: functions, functions2, functions3. A generated function draws
    each result lazily from its [returns] generator, memoized per argument, so it
    behaves as a genuine function within a test case. On a failing replay it
    prints only the argument/result pairs the property actually applied (e.g.
    [f 3 = 7]) rather than an opaque [<function>]. *)

open Hegel

let small_int = integers ~min_value:(-100) ~max_value:100 ()

(** Property (map fusion): mapping [f] then [g] over a list equals mapping their
    composition. Both [f] and [g] are generated [int -> int] functions, drawn
    with [draw_silent] (a function carries no printer). *)
let%hegel_test map_fusion tc =
  let int_fn () =
    draw_silent
      tc
      (functions ~sexp_of_arg:Sexplib0.Sexp_conv.sexp_of_int ~returns:small_int ())
  in
  let f = int_fn () in
  let g = int_fn () in
  let xs = draw tc (lists small_int ~max_size:10 ()) in
  assert (List.map g (List.map f xs) = List.map (fun x -> g (f x)) xs)
[@@settings settings ~test_cases:100 ()]
;;

(** Property (filter keeps matches): every element [List.filter p] keeps does
    satisfy the generated predicate [p], an [int -> bool] function. *)
let%hegel_test filter_keeps_matching tc =
  let p =
    draw_silent
      tc
      (functions ~sexp_of_arg:Sexplib0.Sexp_conv.sexp_of_int ~returns:(booleans ()) ())
  in
  let xs = draw tc (lists (integers ~min_value:0 ~max_value:20 ()) ~max_size:10 ()) in
  List.iter (fun x -> assert (p x)) (List.filter p xs)
[@@settings settings ~test_cases:100 ()]
;;

(** Property (flip is involutive): flipping a generated two-argument function
    twice recovers the original. Uses [functions2]. *)
let%hegel_test flip_flip_is_identity tc =
  let f =
    draw_silent
      tc
      (functions2
         ~sexp_of_arg1:Sexplib0.Sexp_conv.sexp_of_int
         ~sexp_of_arg2:Sexplib0.Sexp_conv.sexp_of_int
         ~returns:small_int
         ())
  in
  let a = draw tc small_int in
  let b = draw tc small_int in
  let flip g x y = g y x in
  assert (flip (flip f) a b = f a b)
[@@settings settings ~test_cases:100 ()]
;;

(** Property (a generated function is a genuine function): a generated
    three-argument function returns the same result for the same arguments. Uses
    [functions3]. *)
let%hegel_test functions_are_deterministic tc =
  let f =
    draw_silent
      tc
      (functions3
         ~sexp_of_arg1:Sexplib0.Sexp_conv.sexp_of_int
         ~sexp_of_arg2:Sexplib0.Sexp_conv.sexp_of_bool
         ~sexp_of_arg3:Sexplib0.Sexp_conv.sexp_of_int
         ~returns:small_int
         ())
  in
  let a = draw tc small_int in
  let b = draw tc (booleans ()) in
  let c = draw tc small_int in
  assert (f a b c = f a b c)
[@@settings settings ~test_cases:100 ()]
;;

let () =
  Printf.printf "Running higher-order (function generator) examples...\n%!";
  map_fusion ();
  Printf.printf "  map_fusion: OK\n%!";
  filter_keeps_matching ();
  Printf.printf "  filter_keeps_matching: OK\n%!";
  flip_flip_is_identity ();
  Printf.printf "  flip_flip_is_identity: OK\n%!";
  functions_are_deterministic ();
  Printf.printf "  functions_are_deterministic: OK\n%!";
  Printf.printf "All tests passed.\n%!"
;;
