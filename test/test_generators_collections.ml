open Hegel
open Generators

(* ==== Validation tests ==== *)

(** Test: lists raises when min_size is negative. *)
let test_lists_negative_min_size () =
  match lists (integers ()) ~min_size:(-1) () with
  | exception Hegel.Usage_error _ -> ()
  | _ -> Alcotest.fail "expected Usage_error"
;;

(** Test: lists raises when max_size is negative. *)
let test_lists_negative_max_size () =
  match lists (integers ()) ~max_size:(-1) () with
  | exception Hegel.Usage_error _ -> ()
  | _ -> Alcotest.fail "expected Usage_error"
;;

(** Test: lists raises when min_size > max_size. *)
let test_lists_min_greater_than_max () =
  Test_helpers.expect_usage_error
    (lists (integers ()) ~min_size:5 ~max_size:3 ())
    "hegel_new_collection requires min_size <= max_size"
;;

(** Test: assoc_lists raises when min_size is negative. *)
let test_assoc_lists_negative_min_size () =
  match assoc_lists (integers ()) (booleans ()) ~min_size:(-1) () with
  | exception Hegel.Usage_error _ -> ()
  | _ -> Alcotest.fail "expected Usage_error"
;;

(** Test: assoc_lists raises when max_size is negative. *)
let test_assoc_lists_negative_max_size () =
  match assoc_lists (integers ()) (booleans ()) ~max_size:(-1) () with
  | exception Hegel.Usage_error _ -> ()
  | _ -> Alcotest.fail "expected Usage_error"
;;

(** Test: assoc_lists raises when min_size > max_size. *)
let test_assoc_lists_min_greater_than_max () =
  Test_helpers.expect_usage_error
    (assoc_lists (integers ()) (booleans ()) ~min_size:5 ~max_size:3 ())
    "hegel_new_collection requires min_size <= max_size"
;;

(* ==== E2E tests ==== *)

(** Test: lists(integers) generates a list where all elements are in range. *)
let test_lists_of_integers_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen = lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:3 () in
    let items = Hegel.draw tc gen in
    Alcotest.(check bool) "max 3" true (List.length items <= 3);
    List.iter (fun n -> assert (n >= 0 && n <= 100)) items)
;;

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let test_lists_booleans_bounds_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
    let items = Hegel.draw tc gen in
    let n = List.length items in
    assert (n >= 3 && n <= 5))
;;

(** Test: lists(filtered integers) → all elements satisfy predicate. *)
let test_lists_non_basic_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let elem = filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = lists elem ~min_size:1 ~max_size:3 () in
    let items = Hegel.draw tc gen in
    let n = List.length items in
    assert (n >= 1 && n <= 3);
    List.iter (fun x -> assert (x > 5)) items)
;;

(** Test: lists(non-basic) without max_size (max_size=None in collection). *)
let test_lists_non_basic_no_max_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let elem = filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = lists elem () in
    let items = Hegel.draw tc gen in
    List.iter (fun x -> assert (x >= 0 && x <= 10)) items)
;;

(** Test: lists(lists(booleans)) → nested lists work. *)
let test_lists_nested_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let inner = lists (booleans ()) ~max_size:3 () in
    let gen = lists inner ~max_size:3 () in
    let outer_items = Hegel.draw tc gen in
    assert (List.length outer_items <= 3);
    List.iter (fun inner_items -> assert (List.length inner_items <= 3)) outer_items)
;;

(** Test: lists(basic, unique=true) E2E — elements are distinct. *)
let test_lists_unique_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
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
    Alcotest.(check int) "all unique" n uniq)
;;

(** Test: lists(non-basic, unique=true) E2E — elements are distinct. *)
let test_lists_non_basic_unique_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let elem = filter (fun v -> v >= 0) (integers ~min_value:0 ~max_value:1000 ()) in
    let gen = lists elem ~min_size:1 ~max_size:5 ~unique:true () in
    let items = Hegel.draw tc gen in
    let n = List.length items in
    assert (n >= 1 && n <= 5);
    let uniq = List.sort_uniq compare items |> List.length in
    Alcotest.(check int) "all unique" n uniq)
;;

(** Test: lists(non-basic, unique=true) with impossible constraints terminates
    via the engine's rejection limit instead of hanging. Uses
    min_value=max_value=0 so every second element is a guaranteed duplicate,
    which causes the engine to send StopTest after its rejection threshold. *)
let test_lists_non_basic_unique_exhaustion_e2e () =
  Hegel.run_hegel_test
    ~settings:
      (Hegel.settings ~test_cases:10 () |> with_suppress_health_check [ Filter_too_much ])
    (fun tc ->
       let elem = filter (fun _ -> true) (integers ~min_value:0 ~max_value:0 ()) in
       (* Asking for ≥2 unique elements from {0} — impossible. The engine's
          many.reject() limit will fire and send StopTest, which
          collection_reject converts to Data_exhausted. *)
       let gen = lists elem ~min_size:2 ~unique:true () in
       ignore (Hegel.draw tc gen))
;;

(** Test: assoc_lists(non-basic keys) E2E — generates pairs. *)
let test_assoc_lists_non_basic_keys_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let key_gen = filter (fun _ -> true) (integers ~min_value:0 ~max_value:100 ()) in
    let val_gen = integers ~min_value:0 ~max_value:100 () in
    let gen = assoc_lists key_gen val_gen ~min_size:0 ~max_size:5 () in
    let pairs = Hegel.draw tc gen in
    assert (List.length pairs <= 5))
;;

(** Test: assoc_lists(non-basic values) E2E — generates pairs. *)
let test_assoc_lists_non_basic_values_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let key_gen = integers ~min_value:0 ~max_value:100 () in
    let val_gen = filter (fun _ -> true) (integers ~min_value:0 ~max_value:100 ()) in
    let gen = assoc_lists key_gen val_gen ~min_size:0 ~max_size:5 () in
    let pairs = Hegel.draw tc gen in
    assert (List.length pairs <= 5))
;;

(** Regression: [lists ~unique:true] over a [map] that collapses distinct raw
    values must not return duplicates post-transform. The engine enforces
    uniqueness on raw values, so a non-injective [map] would yield duplicate
    OCaml values if we took the fast path. The fix routes [unique=true] to
    the dedup path when the element transform isn't known to preserve
    distinctness. *)
let test_lists_unique_under_map_e2e () =
  Hegel.run_hegel_test
    ~settings:
      (Hegel.settings ~test_cases:5 () |> with_suppress_health_check [ Filter_too_much ])
    (fun tc ->
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
       let xs = Hegel.draw tc gen in
       let n = List.length xs in
       let uniq = List.sort_uniq compare xs |> List.length in
       Alcotest.(check int) "all unique" n uniq)
;;

(** Test: hash_tables produces a [Hashtbl.t] within the size bounds, holding
    the generated entries. *)
let test_hash_tables_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen =
      hash_tables
        (integers ~min_value:0 ~max_value:100 ())
        (booleans ())
        ~min_size:1
        ~max_size:5
        ()
    in
    let table = Hegel.draw tc gen in
    let n = Stdlib.Hashtbl.length table in
    assert (n >= 1 && n <= 5);
    Stdlib.Hashtbl.iter (fun key _ -> assert (key >= 0 && key <= 100)) table)
;;

(** Test: hash_tables rejects crossed size bounds like assoc_lists. *)
let test_hash_tables_min_greater_than_max () =
  Test_helpers.expect_usage_error
    (hash_tables (integers ()) (booleans ()) ~min_size:5 ~max_size:3 ())
    "hegel_new_collection requires min_size <= max_size"
;;

(** Regression: [assoc_lists] with a non-basic key generator must still enforce
    key uniqueness. With keys constrained to a single value, the dedup loop
    rejects every duplicate; the engine's reject limit eventually fires
    StopTest, which is caught by the test runner and skips the case.*)
let test_assoc_lists_unique_keys_under_filter_e2e () =
  Hegel.run_hegel_test
    ~settings:
      (Hegel.settings ~test_cases:5 () |> with_suppress_health_check [ Filter_too_much ])
    (fun tc ->
       let gen =
         assoc_lists
           (filter (fun _ -> true) (integers ~min_value:0 ~max_value:0 ()))
           (booleans ())
           ~min_size:2
           ~max_size:2
           ()
       in
       let pairs = Hegel.draw tc gen in
       let keys = List.map fst pairs in
       let uniq = List.sort_uniq compare keys |> List.length in
       Alcotest.(check int) "keys all unique" (List.length keys) uniq)
;;

let tests =
  [ Alcotest.test_case "lists negative min_size" `Quick test_lists_negative_min_size
  ; Alcotest.test_case "lists negative max_size" `Quick test_lists_negative_max_size
  ; Alcotest.test_case "lists min > max" `Quick test_lists_min_greater_than_max
  ; Alcotest.test_case
      "assoc_lists negative min_size"
      `Quick
      test_assoc_lists_negative_min_size
  ; Alcotest.test_case
      "assoc_lists negative max_size"
      `Quick
      test_assoc_lists_negative_max_size
  ; Alcotest.test_case
      "assoc_lists min > max"
      `Quick
      test_assoc_lists_min_greater_than_max
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
  ; Alcotest.test_case "hash_tables e2e" `Quick test_hash_tables_e2e
  ; Alcotest.test_case
      "hash_tables min > max"
      `Quick
      test_hash_tables_min_greater_than_max
  ; Alcotest.test_case
      "assoc_lists non-basic keys e2e"
      `Quick
      test_assoc_lists_non_basic_keys_e2e
  ; Alcotest.test_case
      "assoc_lists non-basic values e2e"
      `Quick
      test_assoc_lists_non_basic_values_e2e
  ; Alcotest.test_case
      "lists unique under non-injective map e2e (regression)"
      `Quick
      test_lists_unique_under_map_e2e
  ; Alcotest.test_case
      "assoc_lists unique keys under filter e2e (regression)"
      `Quick
      test_assoc_lists_unique_keys_under_filter_e2e
  ]
;;
