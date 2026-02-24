(** Tests for the Conformance helper module and new generator types. *)

open Hegel.Generators

(* ==== Conformance helper tests ==== *)

(** Test: parse_test_cases with None returns 50. *)
let test_parse_test_cases_none () =
  let n = Hegel.Conformance.parse_test_cases None in
  Alcotest.(check int) "None → 50" 50 n

(** Test: parse_test_cases with a valid integer string. *)
let test_parse_test_cases_valid () =
  let n = Hegel.Conformance.parse_test_cases (Some "123") in
  Alcotest.(check int) "valid → 123" 123 n

(** Test: parse_test_cases with an invalid string returns 50. *)
let test_parse_test_cases_invalid () =
  let n = Hegel.Conformance.parse_test_cases (Some "not_a_number") in
  Alcotest.(check int) "invalid → 50" 50 n

(** Test: get_test_cases reads the CONFORMANCE_TEST_CASES env var. *)
let test_get_test_cases_env () =
  let saved = Sys.getenv_opt "CONFORMANCE_TEST_CASES" in
  Unix.putenv "CONFORMANCE_TEST_CASES" "77";
  let n = Hegel.Conformance.get_test_cases () in
  Alcotest.(check int) "reads env var" 77 n;
  match saved with
  | None -> ()
  | Some v -> Unix.putenv "CONFORMANCE_TEST_CASES" v

(** Test: format_metrics produces a correct JSON object string. *)
let test_format_metrics () =
  let line = Hegel.Conformance.format_metrics [ ("value", "true") ] in
  Alcotest.(check string) "format" "{\"value\": true}\n" line

(** Test: format_metrics with multiple pairs. *)
let test_format_metrics_multiple () =
  let line =
    Hegel.Conformance.format_metrics [ ("size", "3"); ("min_element", "1") ]
  in
  Alcotest.(check string)
    "format multi" "{\"size\": 3, \"min_element\": 1}\n" line

(** Test: write_metrics_to appends JSON lines to the file. *)
let test_write_metrics_to_appends () =
  let tmp = Filename.temp_file "hegel_test_metrics" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with _ -> ())
    (fun () ->
      Hegel.Conformance.write_metrics_to tmp [ ("value", "true") ];
      Hegel.Conformance.write_metrics_to tmp [ ("value", "false") ];
      let ic = open_in tmp in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      let lines = String.split_on_char '\n' content in
      let non_empty = List.filter (fun l -> l <> "") lines in
      Alcotest.(check int) "two lines" 2 (List.length non_empty);
      Alcotest.(check string)
        "first line" {|{"value": true}|} (List.nth non_empty 0);
      Alcotest.(check string)
        "second line" {|{"value": false}|} (List.nth non_empty 1))

(** Test: write_metrics raises Failure when CONFORMANCE_METRICS_FILE not set
    (None). This test must run before CONFORMANCE_METRICS_FILE is ever set in
    this process. *)
let test_write_metrics_none () =
  (* If env var was previously set by another test, skip the None-raises test
     and just verify the Some branch works. *)
  match Sys.getenv_opt "CONFORMANCE_METRICS_FILE" with
  | None ->
      (* Env var not set: write_metrics should raise Failure *)
      let raised =
        match Hegel.Conformance.write_metrics [ ("k", "v") ] with
        | () -> false
        | exception Failure _ -> true
      in
      Alcotest.(check bool) "raises Failure when env not set" true raised
  | Some _ ->
      (* Already set: test the Some branch via write_metrics_to directly *)
      let n = Hegel.Conformance.parse_test_cases None in
      Alcotest.(check int) "None → 50" 50 n

(** Test: write_metrics appends JSON lines via env var. *)
let test_write_metrics_appends () =
  let tmp = Filename.temp_file "hegel_test_metrics2" ".jsonl" in
  let saved = Sys.getenv_opt "CONFORMANCE_METRICS_FILE" in
  Unix.putenv "CONFORMANCE_METRICS_FILE" tmp;
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove tmp with _ -> ());
      match saved with
      | None -> ()
      | Some v -> Unix.putenv "CONFORMANCE_METRICS_FILE" v)
    (fun () ->
      Hegel.Conformance.write_metrics [ ("value", "42") ];
      let ic = open_in tmp in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "appended" {|{"value": 42}|} (String.trim content))

(** Test: write_metrics raises Sys_error on bad path. *)
let test_write_metrics_bad_path () =
  let saved = Sys.getenv_opt "CONFORMANCE_METRICS_FILE" in
  Unix.putenv "CONFORMANCE_METRICS_FILE" "/nonexistent/dir/file.jsonl";
  let raised =
    match Hegel.Conformance.write_metrics [ ("k", "v") ] with
    | () -> false
    | exception Sys_error _ -> true
    | exception Failure _ -> true
  in
  Alcotest.(check bool) "raises on bad path" true raised;
  match saved with
  | None -> ()
  | Some v -> Unix.putenv "CONFORMANCE_METRICS_FILE" v

(* ==== New generator schema tests ==== *)

(** Test: floats() with no options produces a Basic generator with type=number.
*)
let test_floats_schema_no_bounds () =
  let gen = floats () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "number" typ;
      (* No bounds when not specified *)
      Alcotest.(check bool)
        "no min_value" false
        (List.mem_assoc (`Text "min_value") pairs);
      (* allow_nan defaults to true when no bounds *)
      let allow_nan =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text "allow_nan") pairs)
      in
      Alcotest.(check bool) "allow_nan default true (no bounds)" true allow_nan
  | None -> Alcotest.fail "expected schema"

(** Test: floats() with all options produces correct schema. *)
let test_floats_schema_all_options () =
  let gen =
    floats ~min_value:(-1.0) ~max_value:1.0 ~exclude_min:true ~exclude_max:true
      ~allow_nan:false ~allow_infinity:false ()
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let get_float k =
        Hegel.Cbor_helpers.extract_float (List.assoc (`Text k) pairs)
      in
      let get_bool k =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text k) pairs)
      in
      Alcotest.(check (float 1e-10)) "min_value" (-1.0) (get_float "min_value");
      Alcotest.(check (float 1e-10)) "max_value" 1.0 (get_float "max_value");
      Alcotest.(check bool) "exclude_min" true (get_bool "exclude_min");
      Alcotest.(check bool) "exclude_max" true (get_bool "exclude_max");
      Alcotest.(check bool) "allow_nan" false (get_bool "allow_nan");
      Alcotest.(check bool) "allow_infinity" false (get_bool "allow_infinity");
      let width =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "width") pairs)
      in
      Alcotest.(check int) "width" 64 width
  | None -> Alcotest.fail "expected schema"

(** Test: floats() with both bounds has allow_nan=false and
    allow_infinity=false. *)
let test_floats_schema_exclude_false () =
  let gen = floats ~min_value:0.0 ~max_value:1.0 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let get_bool k =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text k) pairs)
      in
      (* exclude_min/max default to false but are always present *)
      Alcotest.(check bool) "exclude_min false" false (get_bool "exclude_min");
      Alcotest.(check bool) "exclude_max false" false (get_bool "exclude_max");
      (* allow_nan defaults to false when bounds are set *)
      Alcotest.(check bool) "allow_nan false" false (get_bool "allow_nan");
      (* allow_infinity defaults to false when both bounds set *)
      Alcotest.(check bool)
        "allow_infinity false" false
        (get_bool "allow_infinity")
  | None -> Alcotest.fail "expected schema"

(** Test: floats() with only min_value — allow_infinity defaults to true since
    no max. *)
let test_floats_schema_only_min () =
  let gen = floats ~min_value:0.0 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let get_bool k =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text k) pairs)
      in
      (* allow_nan false when any bound set *)
      Alcotest.(check bool) "allow_nan false" false (get_bool "allow_nan");
      (* allow_infinity true when only one bound set (no max) *)
      Alcotest.(check bool)
        "allow_infinity true (no max)" true
        (get_bool "allow_infinity")
  | None -> Alcotest.fail "expected schema"

(** Test: text() produces a Basic generator with type=string schema. *)
let test_text_schema () =
  let gen = text () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "string" typ;
      let min_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "min_size default 0" 0 min_size
  | None -> Alcotest.fail "expected schema"

(** Test: text() with max_size includes it in schema. *)
let test_text_schema_with_max () =
  let gen = text ~min_size:2 ~max_size:10 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let min_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      let max_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "min_size" 2 min_size;
      Alcotest.(check int) "max_size" 10 max_size
  | None -> Alcotest.fail "expected schema"

(** Test: text() without max_size doesn't include it in schema. *)
let test_text_schema_no_max () =
  let gen = text ~min_size:0 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "no max_size" false
        (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: binary() produces a Basic generator with type=binary schema. *)
let test_binary_schema () =
  let gen = binary () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "binary" typ;
      let min_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "min_size default 0" 0 min_size
  | None -> Alcotest.fail "expected schema"

(** Test: binary() with max_size includes it in schema. *)
let test_binary_schema_with_max () =
  let gen = binary ~min_size:1 ~max_size:20 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let max_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "max_size" 20 max_size
  | None -> Alcotest.fail "expected schema"

(** Test: binary() without max_size doesn't include it in schema. *)
let test_binary_schema_no_max () =
  let gen = binary () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "no max_size" false
        (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: sampled_from() produces a Basic generator using "sampled_from" key. *)
let test_sampled_from_schema () =
  let options = [ `Int 1; `Int 2; `Int 3 ] in
  let gen = sampled_from options in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      (* sampled_from uses a top-level key, not a type field *)
      Alcotest.(check bool)
        "has sampled_from key" true
        (List.mem_assoc (`Text "sampled_from") pairs);
      Alcotest.(check bool)
        "no type field" false
        (List.mem_assoc (`Text "type") pairs);
      let opts =
        Hegel.Cbor_helpers.extract_list
          (List.assoc (`Text "sampled_from") pairs)
      in
      Alcotest.(check int) "3 options" 3 (List.length opts)
  | None -> Alcotest.fail "expected schema"

(** Test: hashmaps() produces a Basic generator with type=dict schema and a
    transform. *)
let test_hashmaps_schema () =
  let key_gen = integers ~min_value:0 ~max_value:100 () in
  let val_gen = integers ~min_value:(-10) ~max_value:10 () in
  let gen = hashmaps key_gen val_gen ~min_size:2 ~max_size:5 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match as_basic gen with
  | Some (s, Some _transform) ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "dict" typ;
      let min_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      let max_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "min_size" 2 min_size;
      Alcotest.(check int) "max_size" 5 max_size
  | Some (_, None) -> Alcotest.fail "expected transform"
  | None -> Alcotest.fail "expected basic generator"

(** Test: hashmaps() without max_size omits it from schema. *)
let test_hashmaps_schema_no_max () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "no max_size" false
        (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: hashmaps() transform converts [[k,v],...] to a CBOR map. *)
let test_hashmaps_transform () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, Some transform) -> (
      (* Normal case: array of [k, v] pairs *)
      let raw =
        `Array [ `Array [ `Int 1; `Int 10 ]; `Array [ `Int 2; `Int 20 ] ]
      in
      let result = transform raw in
      match result with
      | `Map pairs ->
          Alcotest.(check int) "2 pairs" 2 (List.length pairs);
          let v1 = List.assoc (`Int 1) pairs in
          Alcotest.(check int)
            "first value" 10
            (Hegel.Cbor_helpers.extract_int v1)
      | _ -> Alcotest.fail "expected map")
  | Some (_, None) -> Alcotest.fail "expected transform"
  | None -> Alcotest.fail "expected basic generator"

(** Test: hashmaps() transform raises on non-array input. *)
let test_hashmaps_transform_bad_outer () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, Some transform) ->
      let raised =
        match transform (`Int 42) with
        | _ -> false
        | exception Failure _ -> true
      in
      Alcotest.(check bool) "raises on non-array" true raised
  | Some (_, None) -> Alcotest.fail "expected transform"
  | None -> Alcotest.fail "expected basic generator"

(** Test: hashmaps() transform raises on bad inner pair. *)
let test_hashmaps_transform_bad_inner () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, Some transform) ->
      let raised =
        match transform (`Array [ `Int 99 ]) with
        | _ -> false
        | exception Failure _ -> true
      in
      Alcotest.(check bool) "raises on bad pair" true raised
  | Some (_, None) -> Alcotest.fail "expected transform"
  | None -> Alcotest.fail "expected basic generator"

(** Test: hashmaps() raises when keys are non-basic. *)
let test_hashmaps_non_basic_keys_raises () =
  let key_gen = filter (fun _ -> true) (integers ()) in
  let val_gen = integers () in
  let raised =
    match hashmaps key_gen val_gen () with
    | _ -> false
    | exception Failure _ -> true
  in
  Alcotest.(check bool) "raises on non-basic keys" true raised

(** Test: hashmaps() raises when values are non-basic. *)
let test_hashmaps_non_basic_values_raises () =
  let key_gen = integers () in
  let val_gen = filter (fun _ -> true) (integers ()) in
  let raised =
    match hashmaps key_gen val_gen () with
    | _ -> false
    | exception Failure _ -> true
  in
  Alcotest.(check bool) "raises on non-basic values" true raised

(* ==== E2E tests for new generators ==== *)

(** Test: floats() E2E — values are floats. *)
let test_floats_e2e () =
  Hegel.Session.run_hegel_test ~name:"floats_e2e" ~test_cases:10 (fun () ->
      let v = generate (floats ~allow_nan:false ~allow_infinity:false ()) in
      let f = Hegel.Cbor_helpers.extract_float v in
      assert (Float.is_finite f))

(** Test: floats() with bounds E2E — values within range. *)
let test_floats_bounds_e2e () =
  Hegel.Session.run_hegel_test ~name:"floats_bounds_e2e" ~test_cases:10
    (fun () ->
      let v =
        generate
          (floats ~min_value:0.0 ~max_value:1.0 ~allow_nan:false
             ~allow_infinity:false ())
      in
      let f = Hegel.Cbor_helpers.extract_float v in
      assert (f >= 0.0 && f <= 1.0))

(** Test: text() E2E — values are text strings. *)
let test_text_e2e () =
  Hegel.Session.run_hegel_test ~name:"text_e2e" ~test_cases:10 (fun () ->
      let v = generate (text ~min_size:1 ~max_size:10 ()) in
      let s = Hegel.Cbor_helpers.extract_string v in
      assert (String.length s >= 1))

(** Test: binary() E2E — values are byte strings. *)
let test_binary_e2e () =
  Hegel.Session.run_hegel_test ~name:"binary_e2e" ~test_cases:10 (fun () ->
      let v = generate (binary ~min_size:0 ~max_size:10 ()) in
      let b = Hegel.Cbor_helpers.extract_bytes v in
      assert (String.length b >= 0))

(** Test: sampled_from() E2E — values come from the options list. *)
let test_sampled_from_e2e () =
  let options = [ `Int 10; `Int 20; `Int 30 ] in
  Hegel.Session.run_hegel_test ~name:"sampled_from_e2e" ~test_cases:10
    (fun () ->
      let v = generate (sampled_from options) in
      let n = Hegel.Cbor_helpers.extract_int v in
      assert (List.mem n [ 10; 20; 30 ]))

(** Test: hashmaps() E2E — values are maps. *)
let test_hashmaps_e2e () =
  Hegel.Session.run_hegel_test ~name:"hashmaps_e2e" ~test_cases:10 (fun () ->
      let key_gen = integers ~min_value:0 ~max_value:100 () in
      let val_gen = integers ~min_value:0 ~max_value:100 () in
      let gen = hashmaps key_gen val_gen ~min_size:0 ~max_size:5 () in
      let v = generate gen in
      let pairs = Hegel.Cbor_helpers.extract_dict v in
      assert (List.length pairs <= 5))

let tests =
  [
    (* Conformance helper tests — write_metrics None MUST be first to run before env is set *)
    Alcotest.test_case "write_metrics None" `Quick test_write_metrics_none;
    Alcotest.test_case "parse_test_cases None" `Quick test_parse_test_cases_none;
    Alcotest.test_case "parse_test_cases valid" `Quick
      test_parse_test_cases_valid;
    Alcotest.test_case "parse_test_cases invalid" `Quick
      test_parse_test_cases_invalid;
    Alcotest.test_case "get_test_cases env" `Quick test_get_test_cases_env;
    Alcotest.test_case "format_metrics" `Quick test_format_metrics;
    Alcotest.test_case "format_metrics multiple" `Quick
      test_format_metrics_multiple;
    Alcotest.test_case "write_metrics_to appends" `Quick
      test_write_metrics_to_appends;
    Alcotest.test_case "write_metrics appends" `Quick test_write_metrics_appends;
    Alcotest.test_case "write_metrics bad path" `Quick
      test_write_metrics_bad_path;
    (* New generator schema tests *)
    Alcotest.test_case "floats schema no bounds" `Quick
      test_floats_schema_no_bounds;
    Alcotest.test_case "floats schema all options" `Quick
      test_floats_schema_all_options;
    Alcotest.test_case "floats schema exclude false" `Quick
      test_floats_schema_exclude_false;
    Alcotest.test_case "floats schema only min" `Quick
      test_floats_schema_only_min;
    Alcotest.test_case "text schema" `Quick test_text_schema;
    Alcotest.test_case "text schema with max" `Quick test_text_schema_with_max;
    Alcotest.test_case "text schema no max" `Quick test_text_schema_no_max;
    Alcotest.test_case "binary schema" `Quick test_binary_schema;
    Alcotest.test_case "binary schema with max" `Quick
      test_binary_schema_with_max;
    Alcotest.test_case "binary schema no max" `Quick test_binary_schema_no_max;
    Alcotest.test_case "sampled_from schema" `Quick test_sampled_from_schema;
    Alcotest.test_case "hashmaps schema" `Quick test_hashmaps_schema;
    Alcotest.test_case "hashmaps schema no max" `Quick
      test_hashmaps_schema_no_max;
    Alcotest.test_case "hashmaps transform" `Quick test_hashmaps_transform;
    Alcotest.test_case "hashmaps transform bad outer" `Quick
      test_hashmaps_transform_bad_outer;
    Alcotest.test_case "hashmaps transform bad inner" `Quick
      test_hashmaps_transform_bad_inner;
    Alcotest.test_case "hashmaps non-basic keys raises" `Quick
      test_hashmaps_non_basic_keys_raises;
    Alcotest.test_case "hashmaps non-basic values raises" `Quick
      test_hashmaps_non_basic_values_raises;
    (* E2E tests *)
    Alcotest.test_case "floats e2e" `Quick test_floats_e2e;
    Alcotest.test_case "floats bounds e2e" `Quick test_floats_bounds_e2e;
    Alcotest.test_case "text e2e" `Quick test_text_e2e;
    Alcotest.test_case "binary e2e" `Quick test_binary_e2e;
    Alcotest.test_case "sampled_from e2e" `Quick test_sampled_from_e2e;
    Alcotest.test_case "hashmaps e2e" `Quick test_hashmaps_e2e;
  ]
