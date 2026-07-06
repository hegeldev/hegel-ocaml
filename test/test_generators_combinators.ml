open Hegel
open Generators

(** Test: one_of with empty list raises. *)
let test_one_of_empty () =
  match one_of [] with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail "expected Failure"
;;

(** Test: one_of with a single generator is accepted. *)
let test_one_of_single_accepted () =
  ignore (one_of [ booleans () ] : (bool, printable) generator)
;;

(** Test: sampled_from raises when given an empty list. *)
let test_sampled_from_empty () =
  match sampled_from [] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: ip_addresses invalid version raises. *)
let test_ip_invalid_version () =
  let raised = ref false in
  (try ignore (ip_addresses ~version:3 ()) with
   | Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised
;;

(* ==== E2E tests ==== *)

(** Test: one_of with basic generators works e2e. *)
let test_one_of_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen =
      one_of
        [ integers ~min_value:0 ~max_value:10 ()
        ; with_printer Core.Int.sexp_of_t (just 99)
        ]
    in
    let v = Hegel.draw tc gen in
    assert ((v >= 0 && v <= 10) || v = 99))
;;

(** Test: one_of with non-basic generators works e2e. *)
let test_one_of_non_basic_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = one_of [ filtered; integers ~min_value:100 ~max_value:200 () ] in
    let v = Hegel.draw tc gen in
    assert ((v > 5 && v <= 10) || (v >= 100 && v <= 200)))
;;

(** Test: optional produces None or Some e2e. *)
let test_optional_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen = optional (integers ~min_value:1 ~max_value:100 ()) in
    match Hegel.draw tc gen with
    | Some v ->
      saw_some := true;
      assert (v >= 1 && v <= 100)
    | None -> saw_none := true);
  (* At least one of each should have occurred in 50 test cases *)
  Alcotest.(check bool) "saw Some" true !saw_some;
  Alcotest.(check bool) "saw None" true !saw_none
;;

(** Test: ip_addresses generates valid IPs e2e. *)
let test_ip_addresses_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let v4 = Hegel.draw tc (ip_addresses ~version:4 ()) in
    assert (String.contains v4 '.');
    let v6 = Hegel.draw tc (ip_addresses ~version:6 ()) in
    assert (String.contains v6 ':'))
;;

(** Test: ip_addresses default generates either v4 or v6 e2e. *)
let test_ip_both_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let v = Hegel.draw tc (ip_addresses ()) in
    assert (String.contains v '.' || String.contains v ':'))
;;

(** Test: tuples2 basic e2e. *)
let test_tuples2_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let gen = tuples2 (integers ~min_value:0 ~max_value:10 ()) (booleans ()) in
    let a, _b = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10))
;;

(** Test: tuples2 composite e2e. *)
let test_tuples2_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = tuples2 filtered (booleans ()) in
    let a, _b = Hegel.draw tc gen in
    assert (a > 5 && a <= 10))
;;

(** Test: tuples3 basic e2e. *)
let test_tuples3_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let gen =
      tuples3
        (integers ~min_value:0 ~max_value:10 ())
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
    in
    let a, _b, c = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10);
    assert (c >= 100 && c <= 200))
;;

(** Test: tuples3 composite e2e. *)
let test_tuples3_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen =
      tuples3 filtered (booleans ()) (integers ~min_value:100 ~max_value:200 ())
    in
    let a, _b, c = Hegel.draw tc gen in
    assert (a > 5 && a <= 10);
    assert (c >= 100 && c <= 200))
;;

(** Test: tuples4 basic e2e. *)
let test_tuples4_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let gen =
      tuples4
        (integers ~min_value:0 ~max_value:10 ())
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
        (floats ~min_value:0.0 ~max_value:1.0 ())
    in
    let a, _b, c, d = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10);
    assert (c >= 100 && c <= 200);
    assert (d >= 0.0 && d <= 1.0))
;;

(** Test: tuples4 composite e2e. *)
let test_tuples4_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen =
      tuples4
        filtered
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
        (floats ~min_value:0.0 ~max_value:1.0 ())
    in
    let a, _b, c, d = Hegel.draw tc gen in
    assert (a > 5 && a <= 10);
    assert (c >= 100 && c <= 200);
    assert (d >= 0.0 && d <= 1.0))
;;

let tests =
  [ Alcotest.test_case "sampled_from empty" `Quick test_sampled_from_empty
  ; Alcotest.test_case "one_of empty" `Quick test_one_of_empty
  ; Alcotest.test_case "one_of single accepted" `Quick test_one_of_single_accepted
  ; Alcotest.test_case "ip_addresses invalid" `Quick test_ip_invalid_version
  ; Alcotest.test_case "one_of e2e" `Quick test_one_of_e2e
  ; Alcotest.test_case "one_of non-basic e2e" `Quick test_one_of_non_basic_e2e
  ; Alcotest.test_case "optional e2e" `Quick test_optional_e2e
  ; Alcotest.test_case "ip_addresses e2e" `Quick test_ip_addresses_e2e
  ; Alcotest.test_case "ip_addresses both e2e" `Quick test_ip_both_e2e
  ; Alcotest.test_case "tuples2 e2e" `Quick test_tuples2_e2e
  ; Alcotest.test_case "tuples2 composite e2e" `Quick test_tuples2_composite_e2e
  ; Alcotest.test_case "tuples3 e2e" `Quick test_tuples3_e2e
  ; Alcotest.test_case "tuples3 composite e2e" `Quick test_tuples3_composite_e2e
  ; Alcotest.test_case "tuples4 e2e" `Quick test_tuples4_e2e
  ; Alcotest.test_case "tuples4 composite e2e" `Quick test_tuples4_composite_e2e
  ]
;;
