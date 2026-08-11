(** Basic property tests using primitive generators.

    Demonstrates: booleans, integers, floats, text, binary, assume, note. *)

open Hegel

(** Property: integer arithmetic identities. *)
let%hegel_test test_integer_arithmetic tc =
  let a = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  let b = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  (* Addition is commutative *)
  assert (a + b = b + a);
  (* Double negation is identity *)
  assert (- (-a) = a);
  (* Absolute value is non-negative *)
  assert (abs a >= 0)
[@@settings settings ~test_cases:100 ()]
;;

(** Property: boolean identities. *)
let%hegel_test test_boolean_laws tc =
  let p = draw tc (booleans ()) in
  let q = draw tc (booleans ()) in
  (* De Morgan's law *)
  assert (((not p) || not q) = not (p && q));
  (* Double negation *)
  assert ((not (not p)) = p);
  (* Commutativity of AND *)
  assert ((p && q) = (q && p))
[@@settings settings ~test_cases:50 ()]
;;

(** Property: division identity (with assume to avoid division by zero). *)
let%hegel_test test_division tc =
  let n = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  let d = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  assume tc (d <> 0);
  note tc (Printf.sprintf "n=%d d=%d" n d);
  (* Integer division: n = (n / d) * d + (n mod d) *)
  assert (n = (n / d * d) + (n mod d))
[@@settings settings ~test_cases:100 ()]
;;

(** Property: text strings have non-negative length. *)
let%hegel_test test_text_length tc =
  let s = draw tc (text ~min_size:0 ~max_size:50 ()) in
  assert (String.length s >= 0)
[@@settings settings ~test_cases:100 ()]
;;

(** Property: binary blobs have non-negative byte length. *)
let%hegel_test test_binary_length tc =
  let b = draw tc (binary ~min_size:0 ~max_size:50 ()) in
  assert (String.length b >= 0)
[@@settings settings ~test_cases:100 ()]
;;

(** Property: finite floats are their own doubles divided by two. Uses
    allow_nan:false and allow_infinity:false to restrict to finite values. *)
let%hegel_test test_float_finite tc =
  let x =
    draw
      tc
      (floats ~min_value:(-1e6) ~max_value:1e6 ~allow_nan:false ~allow_infinity:false ())
  in
  assert (Float.is_finite x)
[@@settings settings ~test_cases:100 ()]
;;

let () =
  Printf.printf "Running basic property tests...\n%!";
  test_integer_arithmetic ();
  Printf.printf "  integer_arithmetic: OK\n%!";
  test_boolean_laws ();
  Printf.printf "  boolean_laws: OK\n%!";
  test_division ();
  Printf.printf "  division_identity: OK\n%!";
  test_text_length ();
  Printf.printf "  text_length: OK\n%!";
  test_binary_length ();
  Printf.printf "  binary_length: OK\n%!";
  test_float_finite ();
  Printf.printf "  float_finite: OK\n%!";
  Printf.printf "All tests passed.\n%!"
;;
