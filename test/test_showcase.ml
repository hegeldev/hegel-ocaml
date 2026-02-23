(** Showcase tests demonstrating idiomatic Hegel usage.

    These tests serve as examples of how a user would write property-based tests
    using the Hegel OCaml SDK. Each test generates values and verifies a real
    mathematical or logical property. *)

open Hegel.Client

(** Property: addition of integers is commutative. *)
let test_addition_commutative () =
  Hegel.Session.run_hegel_test ~name:"addition_commutative" ~test_cases:50
    (fun () ->
      let schema =
        `Map
          [
            (`Text "type", `Text "integer");
            (`Text "min_value", `Int (-1000));
            (`Text "max_value", `Int 1000);
          ]
      in
      let a = Hegel.Cbor_helpers.extract_int (generate_from_schema schema) in
      let b = Hegel.Cbor_helpers.extract_int (generate_from_schema schema) in
      assert (a + b = b + a))

(** Property: double negation of an integer is identity. *)
let test_double_negation () =
  Hegel.Session.run_hegel_test ~name:"double_negation" ~test_cases:50 (fun () ->
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int (-10000));
               (`Text "max_value", `Int 10000);
             ])
      in
      let x = Hegel.Cbor_helpers.extract_int v in
      assert (- (-x) = x))

(** Property: absolute value is always non-negative. *)
let test_abs_nonnegative () =
  Hegel.Session.run_hegel_test ~name:"abs_nonnegative" ~test_cases:50 (fun () ->
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int (-10000));
               (`Text "max_value", `Int 10000);
             ])
      in
      let x = Hegel.Cbor_helpers.extract_int v in
      assert (abs x >= 0))

let tests =
  [
    Alcotest.test_case "addition is commutative" `Quick
      test_addition_commutative;
    Alcotest.test_case "double negation is identity" `Quick test_double_negation;
    Alcotest.test_case "abs(x) >= 0" `Quick test_abs_nonnegative;
  ]
