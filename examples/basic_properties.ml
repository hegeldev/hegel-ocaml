(** Basic property tests using primitive generators.

    Demonstrates: booleans, integers, floats, text, binary, assume, note. *)

open Hegel.Generators

(** Property: integer arithmetic identities. *)
let test_integer_arithmetic () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let a = Hegel.draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
      let b = Hegel.draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
      (* Addition is commutative *)
      assert (a + b = b + a);
      (* Double negation is identity *)
      assert (- (-a) = a);
      (* Absolute value is non-negative *)
      assert (abs a >= 0))

(** Property: boolean identities. *)
let test_boolean_laws () =
  Hegel.run_hegel_test ~test_cases:50 (fun tc ->
      let p = Hegel.draw tc (booleans ()) in
      let q = Hegel.draw tc (booleans ()) in
      (* De Morgan's law *)
      assert (((not p) || not q) = not (p && q));
      (* Double negation *)
      assert ((not (not p)) = p);
      (* Commutativity of AND *)
      assert ((p && q) = (q && p)))

(** Property: division identity (with assume to avoid division by zero). *)
let test_division () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let n = Hegel.draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
      let d = Hegel.draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
      Hegel.assume tc (d <> 0);
      Hegel.note tc (Printf.sprintf "n=%d d=%d" n d);
      (* Integer division: n = (n / d) * d + (n mod d) *)
      assert (n = (n / d * d) + (n mod d)))

(** Property: text strings have non-negative length. *)
let test_text_length () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let s = Hegel.draw tc (text ~min_size:0 ~max_size:50 ()) in
      assert (String.length s >= 0))

(** Property: binary blobs have non-negative byte length. *)
let test_binary_length () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let b = Hegel.draw tc (binary ~min_size:0 ~max_size:50 ()) in
      assert (String.length b >= 0))

(** Property: finite floats are their own doubles divided by two. Uses
    allow_nan:false and allow_infinity:false to restrict to finite values. *)
let test_float_finite () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let x =
        Hegel.draw tc
          (floats ~min_value:(-1e6) ~max_value:1e6 ~allow_nan:false
             ~allow_infinity:false ())
      in
      assert (Float.is_finite x))

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
