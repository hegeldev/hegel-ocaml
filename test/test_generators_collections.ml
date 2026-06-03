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
    let min_s = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    Alcotest.(check int) "min_size" 2 min_s;
    let max_s = Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs) in
    Alcotest.(check int) "max_size" 5 max_s
  | None -> Alcotest.fail "expected schema"
;;

(** Test: lists(basic_elem) without max_size omits max_size from schema. *)
let test_lists_basic_no_max_schema () =
  let elem = integers () in
  let gen = lists elem () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    Alcotest.(check bool)
      "no max_size key"
      false
      (List.mem_assoc (`Text "max_size") pairs);
    let min_s = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    Alcotest.(check int) "default min_size" 0 min_s
  | None -> Alcotest.fail "expected schema"
;;

(** Test: lists(basic_elem_with_transform) produces a Basic generator whose
    transform applies the element transform to every item in the result list. *)
let test_lists_basic_with_element_transform () =
  (* Build a basic generator with a transform (doubles the value); [with_printer]
     makes the mapped element drawable/composable while preserving its core. *)
  let elem = with_printer Core.Int.sexp_of_t (map (fun v -> v * 2) (integers ())) in
  Alcotest.(check bool) "elem is_basic" true (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen is_basic" true (is_basic gen);
  (* The generator has a transform — as_basic returns Some *)
  match as_basic gen with
  | Some (_, _transform) -> () (* expected: transform present *)
  | None -> Alcotest.fail "expected basic"
;;

(** Test: lists(non_basic_elem) produces a CompositeList (not Basic). *)
let test_lists_non_basic_is_not_basic () =
  let elem = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "elem not basic" false (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen not basic" false (is_basic gen);
  Alcotest.(check bool) "as_basic None" true (as_basic gen = None)
;;

(** Test: lists basic transform raises on non-array input. *)
let test_lists_basic_non_array_raises () =
  let gen = lists (integers ()) () in
  match as_basic gen with
  | Some (_, transform) ->
    let raised = ref false in
    (try ignore (transform (`Int 42) : _) with
     | Failure _ -> raised := true);
    Alcotest.(check bool) "raised" true !raised
  | None -> Alcotest.fail "expected Basic"
;;

(* ==== Validation tests ==== *)

(** Test: lists raises when min_size is negative. *)
let test_lists_negative_min_size () =
  match lists (integers ()) ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: lists raises when max_size is negative. *)
let test_lists_negative_max_size () =
  match lists (integers ()) ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: lists raises when min_size > max_size. *)
let test_lists_min_greater_than_max () =
  match lists (integers ()) ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: hashmaps raises when min_size is negative. *)
let test_hashmaps_negative_min_size () =
  match hashmaps (integers ()) (booleans ()) ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: hashmaps raises when max_size is negative. *)
let test_hashmaps_negative_max_size () =
  match hashmaps (integers ()) (booleans ()) ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: hashmaps raises when min_size > max_size. *)
let test_hashmaps_min_greater_than_max () =
  match hashmaps (integers ()) (booleans ()) ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(* ==== E2E tests ==== *)

(** Test: lists(integers) generates a list where all elements are in range. *)
let%hegel_test test_lists_of_integers_e2e tc =
  let gen = lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:3 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  let items = Hegel.draw tc gen in
  Alcotest.(check bool) "max 3" true (List.length items <= 3);
  List.iter (fun n -> assert (n >= 0 && n <= 100)) items
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let%hegel_test test_lists_booleans_bounds_e2e tc =
  let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  let items = Hegel.draw tc gen in
  let n = List.length items in
  assert (n >= 3 && n <= 5)
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(filtered integers) → all elements satisfy predicate. *)
let%hegel_test test_lists_non_basic_e2e tc =
  let elem = filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ()) in
  let gen = lists elem ~min_size:1 ~max_size:3 () in
  Alcotest.(check bool) "not basic" false (is_basic gen);
  let items = Hegel.draw tc gen in
  let n = List.length items in
  assert (n >= 1 && n <= 3);
  List.iter (fun x -> assert (x > 5)) items
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(non-basic) without max_size (max_size=None in collection). *)
let%hegel_test test_lists_non_basic_no_max_e2e tc =
  let elem = filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ()) in
  let gen = lists elem () in
  let items = Hegel.draw tc gen in
  List.iter (fun x -> assert (x >= 0 && x <= 10)) items
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: lists(lists(booleans)) → nested lists work. *)
let%hegel_test test_lists_nested_e2e tc =
  let inner = lists (booleans ()) ~max_size:3 () in
  let gen = lists inner ~max_size:3 () in
  Alcotest.(check bool) "outer is_basic" true (is_basic gen);
  let outer_items = Hegel.draw tc gen in
  assert (List.length outer_items <= 3);
  List.iter (fun inner_items -> assert (List.length inner_items <= 3)) outer_items
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(basic, unique=true) has unique=true in schema. *)
let test_lists_unique_schema () =
  let gen = lists (integers ~min_value:0 ~max_value:100 ()) ~unique:true () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let u = Cbor_helpers.extract_bool (List.assoc (`Text "unique") pairs) in
    Alcotest.(check bool) "unique=true" true u
  | None -> Alcotest.fail "expected schema"
;;

(** Test: lists(basic, unique=false) has unique=false in schema. *)
let test_lists_unique_false_schema () =
  let gen = lists (integers ()) () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let u = Cbor_helpers.extract_bool (List.assoc (`Text "unique") pairs) in
    Alcotest.(check bool) "unique=false" false u
  | None -> Alcotest.fail "expected schema"
;;

(** Test: lists(basic, unique=true) E2E — elements are distinct. *)
let%hegel_test test_lists_unique_e2e tc =
  let gen =
    lists
      (integers ~min_value:0 ~max_value:1000 ())
      ~min_size:3
      ~max_size:10
      ~unique:true
      ()
  in
  let items = Hegel.draw tc gen in
  let n = List.length items in
  assert (n >= 3 && n <= 10);
  let uniq = List.sort_uniq compare items |> List.length in
  Alcotest.(check int) "all unique" n uniq
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(non-basic, unique=true) E2E — elements are distinct. *)
let%hegel_test test_lists_non_basic_unique_e2e tc =
  let elem = filter (fun v -> v >= 0) (integers ~min_value:0 ~max_value:1000 ()) in
  let gen = lists elem ~min_size:1 ~max_size:5 ~unique:true () in
  Alcotest.(check bool) "not basic" false (is_basic gen);
  let items = Hegel.draw tc gen in
  let n = List.length items in
  assert (n >= 1 && n <= 5);
  let uniq = List.sort_uniq compare items |> List.length in
  Alcotest.(check int) "all unique" n uniq
[@@settings Client.settings ~test_cases:50 ()]
;;

(** Test: lists(non-basic, unique=true) with impossible constraints terminates
    via the server's rejection limit instead of hanging. Uses
    min_value=max_value=0 so every second element is a guaranteed duplicate,
    which causes the server to send StopTest after its rejection threshold. *)
let%hegel_test test_lists_non_basic_unique_exhaustion_e2e tc =
  let elem = filter (fun _ -> true) (integers ~min_value:0 ~max_value:0 ()) in
  (* Asking for ≥2 unique elements from {0} — impossible. The server's
     many.reject() limit will fire and send StopTest, which
     collection_reject converts to Data_exhausted. *)
  let gen = lists elem ~min_size:2 ~unique:true () in
  ignore (Hegel.draw tc gen)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: hashmaps(non-basic keys) E2E — generates pairs. *)
let%hegel_test test_hashmaps_non_basic_keys_e2e tc =
  let key_gen = filter (fun _ -> true) (integers ~min_value:0 ~max_value:100 ()) in
  let val_gen = integers ~min_value:0 ~max_value:100 () in
  let gen = hashmaps key_gen val_gen ~min_size:0 ~max_size:5 () in
  Alcotest.(check bool) "not basic" false (is_basic gen);
  let pairs = Hegel.draw tc gen in
  assert (List.length pairs <= 5)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: hashmaps(non-basic values) E2E — generates pairs. *)
let%hegel_test test_hashmaps_non_basic_values_e2e tc =
  let key_gen = integers ~min_value:0 ~max_value:100 () in
  let val_gen = filter (fun _ -> true) (integers ~min_value:0 ~max_value:100 ()) in
  let gen = hashmaps key_gen val_gen ~min_size:0 ~max_size:5 () in
  let pairs = Hegel.draw tc gen in
  assert (List.length pairs <= 5)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Regression: [lists ~unique:true] over a [map] that collapses distinct raw
    values must not return duplicates post-transform. The server enforces
    uniqueness on raw values, so a non-injective [map] would yield duplicate
    OCaml values if we took the fast path. The fix routes [unique=true] to
    the dedup path when the element transform isn't known to preserve
    distinctness. *)
let%hegel_test test_lists_unique_under_map_e2e tc =
  let gen =
    lists
      (with_printer
         Core.Int.sexp_of_t
         (map (fun _ -> 0) (integers ~min_value:0 ~max_value:1 ())))
      ~min_size:2
      ~max_size:2
      ~unique:true
      ()
  in
  Alcotest.(check bool) "elem is_basic" true (is_basic (map (fun _ -> 0) (integers ())));
  Alcotest.(check bool) "list not basic" false (is_basic gen);
  let xs = Hegel.draw tc gen in
  let n = List.length xs in
  let uniq = List.sort_uniq compare xs |> List.length in
  Alcotest.(check int) "all unique" n uniq
[@@settings
  Client.settings ~test_cases:5 ()
  |> Client.with_suppress_health_check [ Client.Filter_too_much ]]
;;

(** Regression: [hashmaps] with a non-basic key generator must still enforce
    key uniqueness. With keys constrained to a single value, the dedup loop
    rejects every duplicate; the server's reject limit eventually fires
    StopTest, which is caught by the test runner and skips the case.*)
let%hegel_test test_hashmaps_unique_keys_under_filter_e2e tc =
  let gen =
    hashmaps
      (filter (fun _ -> true) (integers ~min_value:0 ~max_value:0 ()))
      (booleans ())
      ~min_size:2
      ~max_size:2
      ()
  in
  let pairs = Hegel.draw tc gen in
  let keys = List.map fst pairs in
  let uniq = List.sort_uniq compare keys |> List.length in
  Alcotest.(check int) "keys all unique" (List.length keys) uniq
[@@settings
  Client.settings ~test_cases:5 ()
  |> Client.with_suppress_health_check [ Client.Filter_too_much ]]
;;

let tests =
  [ Alcotest.test_case "lists basic schema" `Quick test_lists_basic_schema
  ; Alcotest.test_case "lists basic no max schema" `Quick test_lists_basic_no_max_schema
  ; Alcotest.test_case
      "lists basic with element transform"
      `Quick
      test_lists_basic_with_element_transform
  ; Alcotest.test_case
      "lists non-basic is not basic"
      `Quick
      test_lists_non_basic_is_not_basic
  ; Alcotest.test_case
      "lists basic non-array raises"
      `Quick
      test_lists_basic_non_array_raises
  ; Alcotest.test_case "lists unique schema" `Quick test_lists_unique_schema
  ; Alcotest.test_case "lists unique false schema" `Quick test_lists_unique_false_schema
  ; Alcotest.test_case "lists negative min_size" `Quick test_lists_negative_min_size
  ; Alcotest.test_case "lists negative max_size" `Quick test_lists_negative_max_size
  ; Alcotest.test_case "lists min > max" `Quick test_lists_min_greater_than_max
  ; Alcotest.test_case "hashmaps negative min_size" `Quick test_hashmaps_negative_min_size
  ; Alcotest.test_case "hashmaps negative max_size" `Quick test_hashmaps_negative_max_size
  ; Alcotest.test_case "hashmaps min > max" `Quick test_hashmaps_min_greater_than_max
  ; Alcotest.test_case "lists of integers e2e" `Quick test_lists_of_integers_e2e
  ; Alcotest.test_case "lists booleans bounds e2e" `Quick test_lists_booleans_bounds_e2e
  ; Alcotest.test_case "lists non-basic e2e" `Quick test_lists_non_basic_e2e
  ; Alcotest.test_case "lists non-basic no max e2e" `Quick test_lists_non_basic_no_max_e2e
  ; Alcotest.test_case "lists nested e2e" `Quick test_lists_nested_e2e
  ; Alcotest.test_case "lists unique e2e" `Quick test_lists_unique_e2e
  ; Alcotest.test_case "lists non-basic unique e2e" `Quick test_lists_non_basic_unique_e2e
  ; Alcotest.test_case
      "lists non-basic unique exhaustion e2e"
      `Quick
      test_lists_non_basic_unique_exhaustion_e2e
  ; Alcotest.test_case
      "hashmaps non-basic keys e2e"
      `Quick
      test_hashmaps_non_basic_keys_e2e
  ; Alcotest.test_case
      "hashmaps non-basic values e2e"
      `Quick
      test_hashmaps_non_basic_values_e2e
  ; Alcotest.test_case
      "lists unique under non-injective map e2e (regression)"
      `Quick
      test_lists_unique_under_map_e2e
  ; Alcotest.test_case
      "hashmaps unique keys under filter e2e (regression)"
      `Quick
      test_hashmaps_unique_keys_under_filter_e2e
  ]
;;
