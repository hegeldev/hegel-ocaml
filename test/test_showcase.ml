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

(** Property: reversing a list twice yields the original list.

    For any list of integers, [List.rev (List.rev xs) = xs]. This verifies that
    the list generator produces well-formed OCaml lists. *)
let test_list_reverse_involution () =
  Hegel.Session.run_hegel_test ~name:"list_rev_involution" ~test_cases:50
    (fun () ->
      let gen =
        lists (integers ~min_value:(-100) ~max_value:100 ()) ~max_size:10 ()
      in
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      let ints = List.map Hegel.Cbor_helpers.extract_int items in
      assert (List.rev (List.rev ints) = ints))

(** Property: sum of a non-negative integer list is non-negative.

    Every element is non-negative, so the sum must also be non-negative. Also
    verifies that every element individually satisfies the bound. *)
let test_list_sum_nonnegative () =
  Hegel.Session.run_hegel_test ~name:"list_sum_nonneg" ~test_cases:50 (fun () ->
      let gen =
        lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:10 ()
      in
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      let ints = List.map Hegel.Cbor_helpers.extract_int items in
      let sum = List.fold_left ( + ) 0 ints in
      assert (sum >= 0);
      List.iter (fun x -> assert (x >= 0 && x <= 100)) ints)

(** Property: filtering a list preserves all matching elements.

    After filtering integers for evenness from a lists() generator, every result
    must be even and within the original range. *)
let test_list_filter_preserves_predicate () =
  Hegel.Session.run_hegel_test ~name:"list_filter_preserves" ~test_cases:50
    (fun () ->
      let elem =
        filter
          (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
          (integers ~min_value:0 ~max_value:50 ())
      in
      let gen = lists elem ~max_size:5 () in
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      assert (List.length items <= 5);
      List.iter
        (fun item ->
          let x = Hegel.Cbor_helpers.extract_int item in
          assert (x mod 2 = 0);
          assert (x >= 0 && x <= 50))
        items)

let tests =
  [
    Alcotest.test_case "addition is commutative" `Quick
      test_addition_commutative;
    Alcotest.test_case "double negation is identity" `Quick test_double_negation;
    Alcotest.test_case "abs(x) >= 0" `Quick test_abs_nonnegative;
    Alcotest.test_case "filter(x > 50) yields values > 50" `Quick
      test_filter_greater_than;
    Alcotest.test_case "filter(even) yields even values" `Quick test_filter_even;
    Alcotest.test_case "list reverse is involution" `Quick
      test_list_reverse_involution;
    Alcotest.test_case "list sum of non-negatives is non-negative" `Quick
      test_list_sum_nonnegative;
    Alcotest.test_case "list filter preserves predicate" `Quick
      test_list_filter_preserves_predicate;
  ]
