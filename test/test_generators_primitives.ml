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
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let gen = integers ~min_value:0 ~max_value:100 () in
      let v = Hegel.draw tc gen in
      assert (v >= 0 && v <= 100))

(** Test: just schema is constant null. *)
let test_just_schema () =
  let gen = just 42 in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      Alcotest.(check int) "two pairs" 2 (List.length pairs);
      Alcotest.(check string)
        "type is constant" "constant"
        (Cbor_helpers.extract_string (List.assoc (`Text "type") pairs));
      Alcotest.(check bool)
        "value is null" true
        (Cbor_helpers.is_null (List.assoc (`Text "value") pairs))
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

(* ==== Character filtering tests ==== *)

(** Test: text with codec option produces codec in schema. *)
let test_text_with_codec () =
  let gen = text ~codec:"ascii" () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let c = Cbor_helpers.extract_string (List.assoc (`Text "codec") pairs) in
      Alcotest.(check string) "codec" "ascii" c
  | None -> Alcotest.fail "expected schema"

(** Test: text with min_codepoint and max_codepoint options. *)
let test_text_with_codepoint_range () =
  let gen = text ~min_codepoint:65 ~max_codepoint:90 () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let min_cp =
        Cbor_helpers.extract_int (List.assoc (`Text "min_codepoint") pairs)
      in
      let max_cp =
        Cbor_helpers.extract_int (List.assoc (`Text "max_codepoint") pairs)
      in
      Alcotest.(check int) "min_codepoint" 65 min_cp;
      Alcotest.(check int) "max_codepoint" 90 max_cp
  | None -> Alcotest.fail "expected schema"

(** Test: text with categories produces categories in schema. *)
let test_text_with_categories () =
  let gen = text ~categories:[ "L"; "Nd" ] () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      (* categories should be present *)
      let cats = List.assoc (`Text "categories") pairs in
      (match cats with
      | `Array items ->
          let strs = List.map Cbor_helpers.extract_string items in
          Alcotest.(check (list string)) "categories" [ "L"; "Nd" ] strs
      | _ -> Alcotest.fail "expected array");
      (* exclude_categories should NOT be present when categories is set *)
      Alcotest.(check bool)
        "no exclude_categories" false
        (List.mem_assoc (`Text "exclude_categories") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: text with exclude_categories produces exclude_categories in schema and
    auto-appends Cs. *)
let test_text_with_exclude_categories () =
  let gen = text ~exclude_categories:[ "Zs" ] () in
  match schema gen with
  | Some s -> (
      let pairs = Cbor_helpers.extract_dict s in
      let cats = List.assoc (`Text "exclude_categories") pairs in
      match cats with
      | `Array items ->
          let strs = List.map Cbor_helpers.extract_string items in
          (* Cs should be auto-appended *)
          Alcotest.(check (list string))
            "exclude_categories" [ "Zs"; "Cs" ] strs
      | _ -> Alcotest.fail "expected array")
  | None -> Alcotest.fail "expected schema"

(** Test: text with exclude_categories already containing Cs does not duplicate
    it. *)
let test_text_exclude_categories_cs_already_present () =
  let gen = text ~exclude_categories:[ "Cs" ] () in
  match schema gen with
  | Some s -> (
      let pairs = Cbor_helpers.extract_dict s in
      let cats = List.assoc (`Text "exclude_categories") pairs in
      match cats with
      | `Array items ->
          let strs = List.map Cbor_helpers.extract_string items in
          Alcotest.(check (list string)) "no duplicate Cs" [ "Cs" ] strs
      | _ -> Alcotest.fail "expected array")
  | None -> Alcotest.fail "expected schema"

(** Test: text with include_characters option. *)
let test_text_with_include_characters () =
  let gen = text ~include_characters:"abc" () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let inc =
        Cbor_helpers.extract_string
          (List.assoc (`Text "include_characters") pairs)
      in
      Alcotest.(check string) "include_characters" "abc" inc
  | None -> Alcotest.fail "expected schema"

(** Test: text with exclude_characters option. *)
let test_text_with_exclude_characters () =
  let gen = text ~exclude_characters:"xyz" () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let exc =
        Cbor_helpers.extract_string
          (List.assoc (`Text "exclude_characters") pairs)
      in
      Alcotest.(check string) "exclude_characters" "xyz" exc
  | None -> Alcotest.fail "expected schema"

(** Test: text raises when both categories and exclude_categories are set. *)
let test_text_categories_and_exclude_categories () =
  match text ~categories:[ "L" ] ~exclude_categories:[ "Zs" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when categories include surrogate category Cs. *)
let test_text_categories_surrogate_cs () =
  match text ~categories:[ "Cs" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when categories include surrogate category C. *)
let test_text_categories_surrogate_c () =
  match text ~categories:[ "C" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text with alphabet produces include_characters and empty categories in
    schema. *)
let test_text_with_alphabet () =
  let gen = text ~alphabet:"abc" () in
  match schema gen with
  | Some s -> (
      let pairs = Cbor_helpers.extract_dict s in
      let inc =
        Cbor_helpers.extract_string
          (List.assoc (`Text "include_characters") pairs)
      in
      Alcotest.(check string) "include_characters from alphabet" "abc" inc;
      let cats = List.assoc (`Text "categories") pairs in
      match cats with
      | `Array items ->
          Alcotest.(check int) "empty categories" 0 (List.length items)
      | _ -> Alcotest.fail "expected array")
  | None -> Alcotest.fail "expected schema"

(** Test: text raises when alphabet is combined with codec. *)
let test_text_alphabet_with_codec () =
  match text ~alphabet:"abc" ~codec:"ascii" () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: text raises when alphabet is combined with max_codepoint. *)
let test_text_alphabet_with_max_codepoint () =
  match text ~alphabet:"abc" ~max_codepoint:90 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"

(** Test: characters produces a Basic generator with min_size=1 and max_size=1.
*)
let test_characters_schema () =
  let gen = characters () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "string" typ;
      let min_s =
        Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      let max_s =
        Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "min_size" 1 min_s;
      Alcotest.(check int) "max_size" 1 max_s
  | None -> Alcotest.fail "expected schema"

(** Test: characters with codec option. *)
let test_characters_with_codec () =
  let gen = characters ~codec:"ascii" () in
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let c = Cbor_helpers.extract_string (List.assoc (`Text "codec") pairs) in
      Alcotest.(check string) "codec" "ascii" c
  | None -> Alcotest.fail "expected schema"

(** Test: characters with categories option. *)
let test_characters_with_categories () =
  let gen = characters ~categories:[ "L" ] () in
  match schema gen with
  | Some s -> (
      let pairs = Cbor_helpers.extract_dict s in
      let cats = List.assoc (`Text "categories") pairs in
      match cats with
      | `Array items ->
          let strs = List.map Cbor_helpers.extract_string items in
          Alcotest.(check (list string)) "categories" [ "L" ] strs
      | _ -> Alcotest.fail "expected array")
  | None -> Alcotest.fail "expected schema"

(* ==== E2E tests ==== *)

(** Test: just always returns the constant. *)
let test_just_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (just 42) in
      Alcotest.(check int) "always 42" 42 v)

(** Test: from_regex generates matching strings. *)
let test_from_regex_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (from_regex "[0-9]+" ()) in
      assert (String.length v > 0))

(** Test: emails generates strings containing at-sign. *)
let test_emails_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (emails ()) in
      assert (String.contains v '@'))

(** Test: urls generates strings starting with http. *)
let test_urls_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (urls ()) in
      assert (
        String.length v >= 7
        && (String.sub v 0 7 = "http://" || String.sub v 0 8 = "https://")))

(** Test: domains generates non-empty strings. *)
let test_domains_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (domains ()) in
      assert (String.length v > 0))

(** Test: dates generates YYYY-MM-DD strings. *)
let test_dates_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (dates ()) in
      assert (String.contains v '-'))

(** Test: times generates strings with colons. *)
let test_times_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
      let v = Hegel.draw tc (times ()) in
      assert (String.contains v ':'))

(** Test: datetimes generates strings with T. *)
let test_datetimes_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:10 ())
    (fun tc ->
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
    Alcotest.test_case "text with codec" `Quick test_text_with_codec;
    Alcotest.test_case "text with codepoint range" `Quick
      test_text_with_codepoint_range;
    Alcotest.test_case "text with categories" `Quick test_text_with_categories;
    Alcotest.test_case "text with exclude_categories" `Quick
      test_text_with_exclude_categories;
    Alcotest.test_case "text exclude_categories Cs present" `Quick
      test_text_exclude_categories_cs_already_present;
    Alcotest.test_case "text with include_characters" `Quick
      test_text_with_include_characters;
    Alcotest.test_case "text with exclude_characters" `Quick
      test_text_with_exclude_characters;
    Alcotest.test_case "text categories + exclude_categories" `Quick
      test_text_categories_and_exclude_categories;
    Alcotest.test_case "text categories surrogate Cs" `Quick
      test_text_categories_surrogate_cs;
    Alcotest.test_case "text categories surrogate C" `Quick
      test_text_categories_surrogate_c;
    Alcotest.test_case "text with alphabet" `Quick test_text_with_alphabet;
    Alcotest.test_case "text alphabet with codec" `Quick
      test_text_alphabet_with_codec;
    Alcotest.test_case "text alphabet with max_codepoint" `Quick
      test_text_alphabet_with_max_codepoint;
    Alcotest.test_case "characters schema" `Quick test_characters_schema;
    Alcotest.test_case "characters with codec" `Quick test_characters_with_codec;
    Alcotest.test_case "characters with categories" `Quick
      test_characters_with_categories;
    Alcotest.test_case "just e2e" `Quick test_just_e2e;
    Alcotest.test_case "from_regex e2e" `Quick test_from_regex_e2e;
    Alcotest.test_case "emails e2e" `Quick test_emails_e2e;
    Alcotest.test_case "urls e2e" `Quick test_urls_e2e;
    Alcotest.test_case "domains e2e" `Quick test_domains_e2e;
    Alcotest.test_case "dates e2e" `Quick test_dates_e2e;
    Alcotest.test_case "times e2e" `Quick test_times_e2e;
    Alcotest.test_case "datetimes e2e" `Quick test_datetimes_e2e;
  ]
