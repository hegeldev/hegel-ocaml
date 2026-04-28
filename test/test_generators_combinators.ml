open Hegel
open Generators

(** Test: one_of with empty list raises. *)
let test_one_of_empty () =
  match one_of [] with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail "expected Failure"

(** Test: one_of with a single generator is accepted. *)
let test_one_of_single_accepted () = ignore (one_of [ booleans () ])

(** Test: sampled_from raises when given an empty list. *)
let test_sampled_from_empty () =
  match sampled_from [] with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: one_of all basic emits a one_of schema whose children are the raw
    child schemas — not wrapped in tagged tuples. Uses heterogeneous element
    schemas so any partial tuple-wrapping regression would be visible. *)
let test_one_of_basic_schema () =
  let gen =
    one_of
      [
        integers ~min_value:0 ~max_value:10 ();
        map (fun _ -> 0) (booleans ());
      ]
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      Alcotest.(check string)
        "type is one_of" "one_of"
        (Cbor_helpers.extract_string (List.assoc (`Text "type") pairs));
      let children =
        Cbor_helpers.extract_list (List.assoc (`Text "generators") pairs)
      in
      let child_types =
        List.map
          (fun child ->
            let child_pairs = Cbor_helpers.extract_dict child in
            Cbor_helpers.extract_string
              (List.assoc (`Text "type") child_pairs))
          children
      in
      (* Children must be the raw child schemas — never the old
         [{"type": "tuple", "elements": [{"type": "constant", ...}, child]}]
         workaround. *)
      Alcotest.(check (list string))
        "child types in order" [ "integer"; "boolean" ] child_types
  | None -> Alcotest.fail "expected schema"

(** Test: one_of with non-basic generators is not basic. *)
let test_one_of_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = one_of [ integers (); filtered ] in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: one_of [index, value] dispatch transform. *)
let test_one_of_dispatch () =
  let gen =
    one_of
      [
        map (fun x -> x * 2) (integers ()); map (fun x -> x + 100) (integers ());
      ]
  in
  match gen with
  | Basic { transform; _ } ->
      (* index 0 → first branch, value 5 → 5 * 2 = 10 *)
      let result = transform (`Array [ `Int 0; `Int 5 ]) in
      Alcotest.(check int) "dispatched first" 10 result;
      (* index 1 → second branch, value 7 → 7 + 100 = 107 *)
      let result2 = transform (`Array [ `Int 1; `Int 7 ]) in
      Alcotest.(check int) "dispatched second" 107 result2
  | _ -> Alcotest.fail "expected Basic"

(** Test: one_of dispatch bad format raises. *)
let test_one_of_dispatch_bad_format () =
  let gen =
    one_of
      [
        integers ~min_value:0 ~max_value:10 ();
        integers ~min_value:100 ~max_value:200 ();
      ]
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "bad format raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: optional creates one_of-based generator. *)
let test_optional_basic () =
  let gen = optional (integers ()) in
  Alcotest.(check bool) "is_basic" true (is_basic gen)

(** Test: ip_addresses IPv4. *)
let test_ip_v4_schema () =
  let gen = ip_addresses ~version:4 () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "ipv4" typ
  | None -> Alcotest.fail "expected schema"

(** Test: ip_addresses IPv6. *)
let test_ip_v6_schema () =
  let gen = ip_addresses ~version:6 () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "ipv6" typ
  | None -> Alcotest.fail "expected schema"

(** Test: ip_addresses default (both) is a one_of. *)
let test_ip_both () =
  let gen = ip_addresses () in
  Alcotest.(check bool) "is_basic (one_of)" true (is_basic gen)

(** Test: ip_addresses invalid version raises. *)
let test_ip_invalid_version () =
  let raised = ref false in
  (try ignore (ip_addresses ~version:3 ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: tuples2 basic schema. *)
let test_tuples2_basic_schema () =
  let gen = tuples2 (integers ()) (booleans ()) in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "tuple" typ
  | None -> Alcotest.fail "expected schema"

(** Test: tuples2 basic transform. *)
let test_tuples2_basic_transform () =
  let gen = tuples2 (integers ()) (booleans ()) in
  match gen with
  | Basic { transform; _ } ->
      let a, b = transform (`Array [ `Int 5; `Bool true ]) in
      Alcotest.(check int) "first" 5 a;
      Alcotest.(check bool) "second" true b
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples2 basic transform bad format raises. *)
let test_tuples2_bad_format () =
  let gen = tuples2 (integers ()) (booleans ()) in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples2 non-basic is not basic. *)
let test_tuples2_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples2 filtered (booleans ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: tuples3 basic schema. *)
let test_tuples3_basic_schema () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "tuple" typ
  | None -> Alcotest.fail "expected schema"

(** Test: tuples3 basic transform. *)
let test_tuples3_basic_transform () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  match gen with
  | Basic { transform; _ } ->
      let a, b, c = transform (`Array [ `Int 1; `Bool false; `Int 3 ]) in
      Alcotest.(check int) "first" 1 a;
      Alcotest.(check bool) "second" false b;
      Alcotest.(check int) "third" 3 c
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples3 bad format raises. *)
let test_tuples3_bad_format () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples3 non-basic is not basic. *)
let test_tuples3_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples3 filtered (booleans ()) (integers ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: tuples4 basic schema. *)
let test_tuples4_basic_schema () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen)

(** Test: tuples4 basic transform. *)
let test_tuples4_basic_transform () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  match gen with
  | Basic { transform; _ } ->
      let a, b, c, d =
        transform (`Array [ `Int 1; `Bool true; `Int 3; `Float 3.14 ])
      in
      Alcotest.(check int) "first" 1 a;
      Alcotest.(check bool) "second" true b;
      Alcotest.(check int) "third" 3 c;
      Alcotest.(check (float 0.01)) "fourth" 3.14 d
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples4 bad format raises. *)
let test_tuples4_bad_format () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples4 non-basic is not basic. *)
let test_tuples4_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples4 filtered (booleans ()) (integers ()) (floats ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(* ==== E2E tests ==== *)

(** Test: one_of with basic generators works e2e. *)
let test_one_of_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let gen = one_of [ integers ~min_value:0 ~max_value:10 (); just 99 ] in
      let v = Hegel.draw tc gen in
      assert ((v >= 0 && v <= 10) || v = 99))

(** Test: one_of with non-basic generators works e2e. *)
let test_one_of_non_basic_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        one_of [ filtered; integers ~min_value:100 ~max_value:200 () ]
      in
      let v = Hegel.draw tc gen in
      assert ((v > 5 && v <= 10) || (v >= 100 && v <= 200)))

(** Test: optional produces None or Some e2e. *)
let test_optional_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:50 ())
    (fun tc ->
      let gen = optional (integers ~min_value:1 ~max_value:100 ()) in
      match Hegel.draw tc gen with
      | Some v ->
          saw_some := true;
          assert (v >= 1 && v <= 100)
      | None -> saw_none := true);
  (* At least one of each should have occurred in 50 test cases *)
  Alcotest.(check bool) "saw Some" true !saw_some;
  Alcotest.(check bool) "saw None" true !saw_none

(** Test: ip_addresses generates valid IPs e2e. *)
let test_ip_addresses_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let v4 = Hegel.draw tc (ip_addresses ~version:4 ()) in
      assert (String.contains v4 '.');
      let v6 = Hegel.draw tc (ip_addresses ~version:6 ()) in
      assert (String.contains v6 ':'))

(** Test: ip_addresses default generates either v4 or v6 e2e. *)
let test_ip_both_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let v = Hegel.draw tc (ip_addresses ()) in
      assert (String.contains v '.' || String.contains v ':'))

(** Test: tuples2 basic e2e. *)
let test_tuples2_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let gen =
        tuples2 (integers ~min_value:0 ~max_value:10 ()) (booleans ())
      in
      let a, _b = Hegel.draw tc gen in
      assert (a >= 0 && a <= 10))

(** Test: tuples2 composite e2e. *)
let test_tuples2_composite_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = tuples2 filtered (booleans ()) in
      let a, _b = Hegel.draw tc gen in
      assert (a > 5 && a <= 10))

(** Test: tuples3 basic e2e. *)
let test_tuples3_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let gen =
        tuples3
          (integers ~min_value:0 ~max_value:10 ())
          (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
      in
      let a, _b, c = Hegel.draw tc gen in
      assert (a >= 0 && a <= 10);
      assert (c >= 100 && c <= 200))

(** Test: tuples3 composite e2e. *)
let test_tuples3_composite_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        tuples3 filtered (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
      in
      let a, _b, c = Hegel.draw tc gen in
      assert (a > 5 && a <= 10);
      assert (c >= 100 && c <= 200))

(** Test: tuples4 basic e2e. *)
let test_tuples4_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
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

(** Test: tuples4 composite e2e. *)
let test_tuples4_composite_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:20 ())
    (fun tc ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        tuples4 filtered (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
          (floats ~min_value:0.0 ~max_value:1.0 ())
      in
      let a, _b, c, d = Hegel.draw tc gen in
      assert (a > 5 && a <= 10);
      assert (c >= 100 && c <= 200);
      assert (d >= 0.0 && d <= 1.0))

let tests =
  [
    Alcotest.test_case "sampled_from empty" `Quick test_sampled_from_empty;
    Alcotest.test_case "one_of empty" `Quick test_one_of_empty;
    Alcotest.test_case "one_of single accepted" `Quick
      test_one_of_single_accepted;
    Alcotest.test_case "one_of basic schema" `Quick test_one_of_basic_schema;
    Alcotest.test_case "one_of non-basic" `Quick test_one_of_non_basic;
    Alcotest.test_case "one_of dispatch" `Quick test_one_of_dispatch;
    Alcotest.test_case "one_of dispatch bad format" `Quick
      test_one_of_dispatch_bad_format;
    Alcotest.test_case "optional basic" `Quick test_optional_basic;
    Alcotest.test_case "ip_addresses v4" `Quick test_ip_v4_schema;
    Alcotest.test_case "ip_addresses v6" `Quick test_ip_v6_schema;
    Alcotest.test_case "ip_addresses both" `Quick test_ip_both;
    Alcotest.test_case "ip_addresses invalid" `Quick test_ip_invalid_version;
    Alcotest.test_case "tuples2 basic schema" `Quick test_tuples2_basic_schema;
    Alcotest.test_case "tuples2 basic transform" `Quick
      test_tuples2_basic_transform;
    Alcotest.test_case "tuples2 bad format" `Quick test_tuples2_bad_format;
    Alcotest.test_case "tuples2 non-basic" `Quick test_tuples2_non_basic;
    Alcotest.test_case "tuples3 basic schema" `Quick test_tuples3_basic_schema;
    Alcotest.test_case "tuples3 basic transform" `Quick
      test_tuples3_basic_transform;
    Alcotest.test_case "tuples3 bad format" `Quick test_tuples3_bad_format;
    Alcotest.test_case "tuples3 non-basic" `Quick test_tuples3_non_basic;
    Alcotest.test_case "tuples4 basic schema" `Quick test_tuples4_basic_schema;
    Alcotest.test_case "tuples4 basic transform" `Quick
      test_tuples4_basic_transform;
    Alcotest.test_case "tuples4 bad format" `Quick test_tuples4_bad_format;
    Alcotest.test_case "tuples4 non-basic" `Quick test_tuples4_non_basic;
    Alcotest.test_case "one_of e2e" `Quick test_one_of_e2e;
    Alcotest.test_case "one_of non-basic e2e" `Quick test_one_of_non_basic_e2e;
    Alcotest.test_case "optional e2e" `Quick test_optional_e2e;
    Alcotest.test_case "ip_addresses e2e" `Quick test_ip_addresses_e2e;
    Alcotest.test_case "ip_addresses both e2e" `Quick test_ip_both_e2e;
    Alcotest.test_case "tuples2 e2e" `Quick test_tuples2_e2e;
    Alcotest.test_case "tuples2 composite e2e" `Quick test_tuples2_composite_e2e;
    Alcotest.test_case "tuples3 e2e" `Quick test_tuples3_e2e;
    Alcotest.test_case "tuples3 composite e2e" `Quick test_tuples3_composite_e2e;
    Alcotest.test_case "tuples4 e2e" `Quick test_tuples4_e2e;
    Alcotest.test_case "tuples4 composite e2e" `Quick test_tuples4_composite_e2e;
  ]
