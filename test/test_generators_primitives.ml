open Hegel
open Generators

(** Test: booleans() produces a Basic generator with type=boolean schema. *)
let test_booleans_schema () =
  let gen = booleans () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "boolean" typ
  | None -> Alcotest.fail "expected schema"

(** Test: integers(0, 100) generates values in range. *)
let test_integers_in_range () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen = integers ~min_value:0 ~max_value:100 () in
      let v = Hegel.draw tc gen in
      assert (v >= 0 && v <= 100))

(** Test: just schema is const null. *)
let test_just_schema () =
  let gen = just 42 in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      Alcotest.(check int) "one pair" 1 (List.length pairs);
      Alcotest.(check bool)
        "const is null" true
        (Cbor_helpers.is_null (List.assoc (`Text "const") pairs))
  | None -> Alcotest.fail "expected schema"

(** Test: just transform ignores server value. *)
let test_just_transform () =
  let gen = just "hello" in
  match gen with
  | Basic { transform; _ } ->
      Alcotest.(check string) "ignores server" "hello" (transform (`Int 999))
  | _ -> Alcotest.fail "expected Basic"

(** Test: from_regex schema with default fullmatch. *)
let test_from_regex_schema () =
  let gen = from_regex "[a-z]+" () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "regex" typ;
      let pat =
        Cbor_helpers.extract_string (List.assoc (`Text "pattern") pairs)
      in
      Alcotest.(check string) "pattern" "[a-z]+" pat;
      let fm =
        Cbor_helpers.extract_bool (List.assoc (`Text "fullmatch") pairs)
      in
      Alcotest.(check bool) "fullmatch" true fm
  | None -> Alcotest.fail "expected schema"

(** Test: from_regex with fullmatch=false. *)
let test_from_regex_no_fullmatch () =
  let gen = from_regex "abc" ~fullmatch:false () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let fm =
        Cbor_helpers.extract_bool (List.assoc (`Text "fullmatch") pairs)
      in
      Alcotest.(check bool) "fullmatch false" false fm
  | None -> Alcotest.fail "expected schema"

(** Test: emails schema. *)
let test_emails_schema () =
  let gen = emails () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "email" typ
  | None -> Alcotest.fail "expected schema"

(** Test: urls schema. *)
let test_urls_schema () =
  let gen = urls () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "url" typ
  | None -> Alcotest.fail "expected schema"

(** Test: domains schema without max_length. *)
let test_domains_schema () =
  let gen = domains () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "domain" typ;
      Alcotest.(check bool)
        "no max_length" false
        (List.mem_assoc (`Text "max_length") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: domains schema with max_length. *)
let test_domains_max_length () =
  let gen = domains ~max_length:100 () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let ml =
        Cbor_helpers.extract_int (List.assoc (`Text "max_length") pairs)
      in
      Alcotest.(check int) "max_length" 100 ml
  | None -> Alcotest.fail "expected schema"

(** Test: dates schema. *)
let test_dates_schema () =
  let gen = dates () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "date" typ
  | None -> Alcotest.fail "expected schema"

(** Test: times schema. *)
let test_times_schema () =
  let gen = times () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "time" typ
  | None -> Alcotest.fail "expected schema"

(** Test: datetimes schema. *)
let test_datetimes_schema () =
  let gen = datetimes () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "datetime" typ
  | None -> Alcotest.fail "expected schema"

(* ==== Validation tests ==== *)

(** Test: integers raises when min_value > max_value. *)
let test_integers_min_greater_than_max () =
  match integers ~min_value:10 ~max_value:5 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: floats raises when allow_nan=true with min_value set. *)
let test_floats_nan_with_min () =
  match floats ~allow_nan:true ~min_value:0.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: floats raises when allow_nan=true with max_value set. *)
let test_floats_nan_with_max () =
  match floats ~allow_nan:true ~max_value:1.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: floats raises when min_value > max_value. *)
let test_floats_min_greater_than_max () =
  match floats ~min_value:10.0 ~max_value:5.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: floats raises when allow_infinity=true with both bounds set. *)
let test_floats_infinity_with_both_bounds () =
  match floats ~allow_infinity:true ~min_value:0.0 ~max_value:1.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when min_size is negative. *)
let test_text_negative_min_size () =
  match text ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when max_size is negative. *)
let test_text_negative_max_size () =
  match text ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when min_size > max_size. *)
let test_text_min_greater_than_max () =
  match text ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: binary raises when min_size is negative. *)
let test_binary_negative_min_size () =
  match binary ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: binary raises when max_size is negative. *)
let test_binary_negative_max_size () =
  match binary ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: binary raises when min_size > max_size. *)
let test_binary_min_greater_than_max () =
  match binary ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: domains raises when max_length is below 4. *)
let test_domains_max_length_too_small () =
  match domains ~max_length:2 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: domains raises when max_length is above 255. *)
let test_domains_max_length_too_large () =
  match domains ~max_length:256 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(* ==== E2E tests ==== *)

(** Test: just always returns the constant. *)
let test_just_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (just 42) in
      Alcotest.(check int) "always 42" 42 v)

(** Test: from_regex generates matching strings. *)
let test_from_regex_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (from_regex "[0-9]+" ()) in
      assert (String.length v > 0))

(** Test: emails generates strings containing at-sign. *)
let test_emails_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (emails ()) in
      assert (String.contains v '@'))

(** Test: urls generates strings starting with http. *)
let test_urls_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (urls ()) in
      assert (
        String.length v >= 7
        && (String.sub v 0 7 = "http://" || String.sub v 0 8 = "https://")))

(** Test: domains generates non-empty strings. *)
let test_domains_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (domains ()) in
      assert (String.length v > 0))

(** Test: dates generates YYYY-MM-DD strings. *)
let test_dates_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (dates ()) in
      assert (String.contains v '-'))

(** Test: times generates strings with colons. *)
let test_times_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (times ()) in
      assert (String.contains v ':'))

(** Test: datetimes generates strings with T. *)
let test_datetimes_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let v = Hegel.draw tc (datetimes ()) in
      assert (String.contains v 'T'))

let tests =
  [
    Alcotest.test_case "booleans schema" `Quick test_booleans_schema;
    Alcotest.test_case "integers in range" `Quick test_integers_in_range;
    Alcotest.test_case "just schema" `Quick test_just_schema;
    Alcotest.test_case "just transform" `Quick test_just_transform;
    Alcotest.test_case "from_regex schema" `Quick test_from_regex_schema;
    Alcotest.test_case "from_regex no fullmatch" `Quick
      test_from_regex_no_fullmatch;
    Alcotest.test_case "emails schema" `Quick test_emails_schema;
    Alcotest.test_case "urls schema" `Quick test_urls_schema;
    Alcotest.test_case "domains schema" `Quick test_domains_schema;
    Alcotest.test_case "domains max_length" `Quick test_domains_max_length;
    Alcotest.test_case "dates schema" `Quick test_dates_schema;
    Alcotest.test_case "times schema" `Quick test_times_schema;
    Alcotest.test_case "datetimes schema" `Quick test_datetimes_schema;
    Alcotest.test_case "integers min > max" `Quick
      test_integers_min_greater_than_max;
    Alcotest.test_case "floats nan with min" `Quick test_floats_nan_with_min;
    Alcotest.test_case "floats nan with max" `Quick test_floats_nan_with_max;
    Alcotest.test_case "floats min > max" `Quick
      test_floats_min_greater_than_max;
    Alcotest.test_case "floats infinity with both bounds" `Quick
      test_floats_infinity_with_both_bounds;
    Alcotest.test_case "text negative min_size" `Quick
      test_text_negative_min_size;
    Alcotest.test_case "text negative max_size" `Quick
      test_text_negative_max_size;
    Alcotest.test_case "text min > max" `Quick test_text_min_greater_than_max;
    Alcotest.test_case "binary negative min_size" `Quick
      test_binary_negative_min_size;
    Alcotest.test_case "binary negative max_size" `Quick
      test_binary_negative_max_size;
    Alcotest.test_case "binary min > max" `Quick
      test_binary_min_greater_than_max;
    Alcotest.test_case "domains max_length too small" `Quick
      test_domains_max_length_too_small;
    Alcotest.test_case "domains max_length too large" `Quick
      test_domains_max_length_too_large;
    Alcotest.test_case "just e2e" `Quick test_just_e2e;
    Alcotest.test_case "from_regex e2e" `Quick test_from_regex_e2e;
    Alcotest.test_case "emails e2e" `Quick test_emails_e2e;
    Alcotest.test_case "urls e2e" `Quick test_urls_e2e;
    Alcotest.test_case "domains e2e" `Quick test_domains_e2e;
    Alcotest.test_case "dates e2e" `Quick test_dates_e2e;
    Alcotest.test_case "times e2e" `Quick test_times_e2e;
    Alcotest.test_case "datetimes e2e" `Quick test_datetimes_e2e;
  ]
