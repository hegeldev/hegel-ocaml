(** Showcase tests demonstrating idiomatic Hegel usage.

    These tests serve as examples of how a user would write property-based tests
    using the Hegel OCaml SDK. Each test generates values and verifies a real
    mathematical or logical property. *)

open Hegel.Client
open Hegel.Generators

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

(** Property: filter(x > 50) on integers(0, 100) always yields x > 50.

    Every filtered value must satisfy the predicate. Also checks that the value
    is still within the original bounds. *)
let test_filter_greater_than () =
  Hegel.Session.run_hegel_test ~name:"filter_gt50" ~test_cases:50 (fun () ->
      let gen =
        filter
          (fun v -> Hegel.Cbor_helpers.extract_int v > 50)
          (integers ~min_value:0 ~max_value:100 ())
      in
      let x = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (x > 50);
      assert (x <= 100))

(** Property: filter(x % 2 = 0) on integers(0, 10) always yields even values.

    Every filtered value is even and within bounds. We additionally verify that
    even numbers are divisible by 2, i.e. that x / 2 * 2 = x. *)
let test_filter_even () =
  Hegel.Session.run_hegel_test ~name:"filter_even" ~test_cases:50 (fun () ->
      let gen =
        filter
          (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
          (integers ~min_value:0 ~max_value:10 ())
      in
      let x = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (x mod 2 = 0);
      assert (x / 2 * 2 = x);
      assert (x >= 0 && x <= 10))

let tests =
  [
    Alcotest.test_case "addition is commutative" `Quick
      test_addition_commutative;
    Alcotest.test_case "double negation is identity" `Quick test_double_negation;
    Alcotest.test_case "abs(x) >= 0" `Quick test_abs_nonnegative;
    Alcotest.test_case "filter(x > 50) yields values > 50" `Quick
      test_filter_greater_than;
    Alcotest.test_case "filter(even) yields even values" `Quick test_filter_even;
  ]
