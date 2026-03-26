(** Real-world scenario: sorted-merge property test.

    Demonstrates a realistic property-based test: verifying that a merge
    function for two sorted lists produces a sorted output with the same
    elements as the concatenation of the inputs. *)

open Hegel.Generators

(** [sorted lst] returns [lst] in non-decreasing order. *)
let sorted lst = List.sort compare lst

(** [merge_sorted a b] merges two sorted integer lists into a single sorted
    list. This is the function under test. *)
let rec merge_sorted a b =
  match (a, b) with
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs, y :: ys ->
      if x <= y then x :: merge_sorted xs (y :: ys)
      else y :: merge_sorted (x :: xs) ys

(** [is_sorted lst] checks that [lst] is in non-decreasing order. *)
let rec is_sorted = function
  | [] | [ _ ] -> true
  | x :: y :: rest -> x <= y && is_sorted (y :: rest)

(** [multiset_equal a b] checks that two lists contain the same elements with
    the same multiplicities (order-independent equality). *)
let multiset_equal a b = List.sort compare a = List.sort compare b

(** Property: merging two sorted lists gives a sorted list. *)
let test_merge_sorted_is_sorted () =
  Hegel.run_hegel_test ~test_cases:200 (fun tc ->
      let int_gen = integers ~min_value:(-1000) ~max_value:1000 () in
      let list_gen = lists int_gen ~min_size:0 ~max_size:20 () in
      let a = sorted (Hegel.draw tc list_gen) in
      let b = sorted (Hegel.draw tc list_gen) in
      let merged = merge_sorted a b in
      assert (is_sorted merged))

(** Property: merging two sorted lists preserves all elements (multiset equality
    between [a @ b] and the merged result). *)
let test_merge_preserves_elements () =
  Hegel.run_hegel_test ~test_cases:200 (fun tc ->
      let int_gen = integers ~min_value:(-100) ~max_value:100 () in
      let list_gen = lists int_gen ~min_size:0 ~max_size:15 () in
      let a = sorted (Hegel.draw tc list_gen) in
      let b = sorted (Hegel.draw tc list_gen) in
      let merged = merge_sorted a b in
      assert (multiset_equal merged (a @ b)))

(** Property: merging a list with itself preserves sorted order and doubles
    every element's count. *)
let test_merge_with_self () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let int_gen = integers ~min_value:(-500) ~max_value:500 () in
      let list_gen = lists int_gen ~min_size:0 ~max_size:10 () in
      let a = sorted (Hegel.draw tc list_gen) in
      let merged = merge_sorted a a in
      assert (is_sorted merged);
      assert (List.length merged = 2 * List.length a))

(** Property: merge is commutative up to element equality (same multiset). *)
let test_merge_commutative () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let int_gen = integers ~min_value:(-200) ~max_value:200 () in
      let list_gen = lists int_gen ~min_size:0 ~max_size:10 () in
      let a = sorted (Hegel.draw tc list_gen) in
      let b = sorted (Hegel.draw tc list_gen) in
      let ab = merge_sorted a b in
      let ba = merge_sorted b a in
      (* Both results should be the same sorted multiset *)
      assert (multiset_equal ab ba);
      assert (is_sorted ab);
      assert (is_sorted ba))

let () =
  Printf.printf "Running sorted-merge property tests...\n%!";
  test_merge_sorted_is_sorted ();
  Printf.printf "  merge_sorted_is_sorted: OK\n%!";
  test_merge_preserves_elements ();
  Printf.printf "  merge_preserves_elements: OK\n%!";
  test_merge_with_self ();
  Printf.printf "  merge_with_self: OK\n%!";
  test_merge_commutative ();
  Printf.printf "  merge_commutative: OK\n%!";
  Printf.printf "All tests passed.\n%!"
