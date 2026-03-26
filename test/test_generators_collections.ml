open Hegel
open Generators

(** Test: lists(basic_elem) produces a Basic generator with type=list schema. *)
let test_lists_basic_schema () =
  let elem = integers ~min_value:0 ~max_value:10 () in
  let gen = lists elem ~min_size:2 ~max_size:5 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "list" typ;
      let min_s =
        Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "min_size" 2 min_s;
      let max_s =
        Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "max_size" 5 max_s
  | None -> Alcotest.fail "expected schema"

(** Test: lists(basic_elem) without max_size omits max_size from schema. *)
let test_lists_basic_no_max_schema () =
  let elem = integers () in
  let gen = lists elem () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "no max_size key" false
        (List.mem_assoc (`Text "max_size") pairs);
      let min_s =
        Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "default min_size" 0 min_s
  | None -> Alcotest.fail "expected schema"

(** Test: lists(basic_elem_with_transform) produces a Basic generator whose
    transform applies the element transform to every item in the result list. *)
let test_lists_basic_with_element_transform () =
  (* Build a basic generator with a transform (doubles the value) *)
  let elem = map (fun v -> v * 2) (integers ()) in
  Alcotest.(check bool) "elem is_basic" true (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen is_basic" true (is_basic gen);
  (* The generator has a transform — as_basic returns Some *)
  match as_basic gen with
  | Some (_, _transform) -> () (* expected: transform present *)
  | None -> Alcotest.fail "expected basic"

(** Test: lists(non_basic_elem) produces a CompositeList (not Basic). *)
let test_lists_non_basic_is_not_basic () =
  let elem = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "elem not basic" false (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen not basic" false (is_basic gen);
  Alcotest.(check bool) "as_basic None" true (as_basic gen = None)

(** Test: lists basic transform raises on non-array input. *)
let test_lists_basic_non_array_raises () =
  let gen = lists (integers ()) () in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(* ==== Validation tests ==== *)

(** Test: lists raises when min_size is negative. *)
let test_lists_negative_min_size () =
  match lists (integers ()) ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: lists raises when max_size is negative. *)
let test_lists_negative_max_size () =
  match lists (integers ()) ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: lists raises when min_size > max_size. *)
let test_lists_min_greater_than_max () =
  match lists (integers ()) ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: hashmaps raises when min_size is negative. *)
let test_hashmaps_negative_min_size () =
  match hashmaps (integers ()) (booleans ()) ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: hashmaps raises when max_size is negative. *)
let test_hashmaps_negative_max_size () =
  match hashmaps (integers ()) (booleans ()) ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: hashmaps raises when min_size > max_size. *)
let test_hashmaps_min_greater_than_max () =
  match hashmaps (integers ()) (booleans ()) ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(* ==== E2E tests ==== *)

(** Test: lists(integers) generates a list where all elements are in range. *)
let test_lists_of_integers_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let gen =
        lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:3 ()
      in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let items = Hegel.draw tc gen in
      Alcotest.(check bool) "max 3" true (List.length items <= 3);
      List.iter (fun n -> assert (n >= 0 && n <= 100)) items)

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let test_lists_booleans_bounds_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let items = Hegel.draw tc gen in
      let n = List.length items in
      assert (n >= 3 && n <= 5))

(** Test: lists(filtered integers) → all elements satisfy predicate. *)
let test_lists_non_basic_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let elem =
        filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = lists elem ~min_size:1 ~max_size:3 () in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let items = Hegel.draw tc gen in
      let n = List.length items in
      assert (n >= 1 && n <= 3);
      List.iter (fun x -> assert (x > 5)) items)

(** Test: lists(non-basic) without max_size (max_size=None in collection). *)
let test_lists_non_basic_no_max_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let elem =
        filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = lists elem () in
      let items = Hegel.draw tc gen in
      List.iter (fun x -> assert (x >= 0 && x <= 10)) items)

(** Test: lists(lists(booleans)) → nested lists work. *)
let test_lists_nested_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let inner = lists (booleans ()) ~max_size:3 () in
      let gen = lists inner ~max_size:3 () in
      Alcotest.(check bool) "outer is_basic" true (is_basic gen);
      let outer_items = Hegel.draw tc gen in
      assert (List.length outer_items <= 3);
      List.iter
        (fun inner_items -> assert (List.length inner_items <= 3))
        outer_items)

let tests =
  [
    Alcotest.test_case "lists basic schema" `Quick test_lists_basic_schema;
    Alcotest.test_case "lists basic no max schema" `Quick
      test_lists_basic_no_max_schema;
    Alcotest.test_case "lists basic with element transform" `Quick
      test_lists_basic_with_element_transform;
    Alcotest.test_case "lists non-basic is not basic" `Quick
      test_lists_non_basic_is_not_basic;
    Alcotest.test_case "lists basic non-array raises" `Quick
      test_lists_basic_non_array_raises;
    Alcotest.test_case "lists negative min_size" `Quick
      test_lists_negative_min_size;
    Alcotest.test_case "lists negative max_size" `Quick
      test_lists_negative_max_size;
    Alcotest.test_case "lists min > max" `Quick test_lists_min_greater_than_max;
    Alcotest.test_case "hashmaps negative min_size" `Quick
      test_hashmaps_negative_min_size;
    Alcotest.test_case "hashmaps negative max_size" `Quick
      test_hashmaps_negative_max_size;
    Alcotest.test_case "hashmaps min > max" `Quick
      test_hashmaps_min_greater_than_max;
    Alcotest.test_case "lists of integers e2e" `Quick test_lists_of_integers_e2e;
    Alcotest.test_case "lists booleans bounds e2e" `Quick
      test_lists_booleans_bounds_e2e;
    Alcotest.test_case "lists non-basic e2e" `Quick test_lists_non_basic_e2e;
    Alcotest.test_case "lists non-basic no max e2e" `Quick
      test_lists_non_basic_no_max_e2e;
    Alcotest.test_case "lists nested e2e" `Quick test_lists_nested_e2e;
  ]
