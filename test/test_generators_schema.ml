(** Schema, transform, and e2e tests for floats / text / binary / sampled_from /
    hashmaps generators. *)

open Hegel
open Generators

(* ==== Generator schema tests ==== *)

(** Test: floats() with no options produces a Basic generator with type=number.
*)
let test_floats_schema_no_bounds () =
  let gen = floats () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "float" typ;
    (* No bounds when not specified *)
    Alcotest.(check bool) "no min_value" false (List.mem_assoc (`Text "min_value") pairs);
    (* allow_nan defaults to true when no bounds *)
    let allow_nan = Cbor_helpers.extract_bool (List.assoc (`Text "allow_nan") pairs) in
    Alcotest.(check bool) "allow_nan default true (no bounds)" true allow_nan
  | None -> Alcotest.fail "expected schema"
;;

(** Test: floats() with all options produces correct schema. *)
let test_floats_schema_all_options () =
  let gen =
    floats
      ~min_value:(-1.0)
      ~max_value:1.0
      ~exclude_min:true
      ~exclude_max:true
      ~allow_nan:false
      ~allow_infinity:false
      ()
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let get_float k = Cbor_helpers.extract_float (List.assoc (`Text k) pairs) in
    let get_bool k = Cbor_helpers.extract_bool (List.assoc (`Text k) pairs) in
    Alcotest.(check (float 1e-10)) "min_value" (-1.0) (get_float "min_value");
    Alcotest.(check (float 1e-10)) "max_value" 1.0 (get_float "max_value");
    Alcotest.(check bool) "exclude_min" true (get_bool "exclude_min");
    Alcotest.(check bool) "exclude_max" true (get_bool "exclude_max");
    Alcotest.(check bool) "allow_nan" false (get_bool "allow_nan");
    Alcotest.(check bool) "allow_infinity" false (get_bool "allow_infinity");
    let width = Cbor_helpers.extract_int (List.assoc (`Text "width") pairs) in
    Alcotest.(check int) "width" 64 width
  | None -> Alcotest.fail "expected schema"
;;

(** Test: floats() with both bounds has allow_nan=false and
    allow_infinity=false. *)
let test_floats_schema_exclude_false () =
  let gen = floats ~min_value:0.0 ~max_value:1.0 () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let get_bool k = Cbor_helpers.extract_bool (List.assoc (`Text k) pairs) in
    (* exclude_min/max default to false but are always present *)
    Alcotest.(check bool) "exclude_min false" false (get_bool "exclude_min");
    Alcotest.(check bool) "exclude_max false" false (get_bool "exclude_max");
    (* allow_nan defaults to false when bounds are set *)
    Alcotest.(check bool) "allow_nan false" false (get_bool "allow_nan");
    (* allow_infinity defaults to false when both bounds set *)
    Alcotest.(check bool) "allow_infinity false" false (get_bool "allow_infinity")
  | None -> Alcotest.fail "expected schema"
;;

(** Test: floats() with only min_value — allow_infinity defaults to true since
    no max. *)
let test_floats_schema_only_min () =
  let gen = floats ~min_value:0.0 () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let get_bool k = Cbor_helpers.extract_bool (List.assoc (`Text k) pairs) in
    (* allow_nan false when any bound set *)
    Alcotest.(check bool) "allow_nan false" false (get_bool "allow_nan");
    (* allow_infinity true when only one bound set (no max) *)
    Alcotest.(check bool) "allow_infinity true (no max)" true (get_bool "allow_infinity")
  | None -> Alcotest.fail "expected schema"
;;

(** Test: text() produces a Basic generator with type=string schema. *)
let test_text_schema () =
  let gen = text () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "string" typ;
    let min_size = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    Alcotest.(check int) "min_size default 0" 0 min_size
  | None -> Alcotest.fail "expected schema"
;;

(** Test: text() with max_size includes it in schema. *)
let test_text_schema_with_max () =
  let gen = text ~min_size:2 ~max_size:10 () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let min_size = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    let max_size = Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs) in
    Alcotest.(check int) "min_size" 2 min_size;
    Alcotest.(check int) "max_size" 10 max_size
  | None -> Alcotest.fail "expected schema"
;;

(** Test: text() without max_size doesn't include it in schema. *)
let test_text_schema_no_max () =
  let gen = text ~min_size:0 () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    Alcotest.(check bool) "no max_size" false (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"
;;

(** Test: binary() produces a Basic generator with type=binary schema. *)
let test_binary_schema () =
  let gen = binary () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "binary" typ;
    let min_size = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    Alcotest.(check int) "min_size default 0" 0 min_size
  | None -> Alcotest.fail "expected schema"
;;

(** Test: binary() with max_size includes it in schema. *)
let test_binary_schema_with_max () =
  let gen = binary ~min_size:1 ~max_size:20 () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let max_size = Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs) in
    Alcotest.(check int) "max_size" 20 max_size
  | None -> Alcotest.fail "expected schema"
;;

(** Test: binary() without max_size doesn't include it in schema. *)
let test_binary_schema_no_max () =
  let gen = binary () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    Alcotest.(check bool) "no max_size" false (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"
;;

(** Test: sampled_from() produces a Basic generator using integer index schema.
    Since sampled_from is implemented via integers + map, the schema is an
    integer schema (type=integer, min=0, max=n-1). *)
let test_sampled_from_schema () =
  let options = [ 1; 2; 3 ] in
  let gen = sampled_from options in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    (* sampled_from uses an integer index schema *)
    Alcotest.(check bool) "has type field" true (List.mem_assoc (`Text "type") pairs);
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type is integer" "integer" typ;
    let min_v = Cbor_helpers.extract_int (List.assoc (`Text "min_value") pairs) in
    Alcotest.(check int) "min_value 0" 0 min_v;
    let max_v = Cbor_helpers.extract_int (List.assoc (`Text "max_value") pairs) in
    Alcotest.(check int) "max_value 2 (n-1)" 2 max_v
  | None -> Alcotest.fail "expected schema"
;;

(** Test: hashmaps() produces a Basic generator with type=dict schema and a
    transform. *)
let test_hashmaps_schema () =
  let key_gen = integers ~min_value:0 ~max_value:100 () in
  let val_gen = integers ~min_value:(-10) ~max_value:10 () in
  let gen = hashmaps key_gen val_gen ~min_size:2 ~max_size:5 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match as_basic gen with
  | Some (s, _transform) ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "dict" typ;
    let min_size = Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs) in
    let max_size = Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs) in
    Alcotest.(check int) "min_size" 2 min_size;
    Alcotest.(check int) "max_size" 5 max_size
  | None -> Alcotest.fail "expected basic generator"
;;

(** Test: hashmaps() without max_size omits it from schema. *)
let test_hashmaps_schema_no_max () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    Alcotest.(check bool) "no max_size" false (List.mem_assoc (`Text "max_size") pairs)
  | None -> Alcotest.fail "expected schema"
;;

(** Test: hashmaps() transform converts [[k,v],...] to a list of typed pairs. *)
let test_hashmaps_transform () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, transform) ->
    (* Normal case: array of [k, v] pairs *)
    let raw = `Array [ `Array [ `Int 1; `Int 10 ]; `Array [ `Int 2; `Int 20 ] ] in
    let result = transform raw in
    Alcotest.(check int) "2 pairs" 2 (List.length result);
    let v1 = List.assoc 1 result in
    Alcotest.(check int) "first value" 10 v1
  | None -> Alcotest.fail "expected basic generator"
;;

(** Test: hashmaps() transform raises on non-array input. *)
let test_hashmaps_transform_bad_outer () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, transform) ->
    let raised =
      match transform (`Int 42) with
      | _ -> false
      | exception Failure _ -> true
    in
    Alcotest.(check bool) "raises on non-array" true raised
  | None -> Alcotest.fail "expected basic generator"
;;

(** Test: hashmaps() transform raises on bad inner pair. *)
let test_hashmaps_transform_bad_inner () =
  let key_gen = integers () in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  match as_basic gen with
  | Some (_schema, transform) ->
    let raised =
      match transform (`Array [ `Int 99 ]) with
      | _ -> false
      | exception Failure _ -> true
    in
    Alcotest.(check bool) "raises on bad pair" true raised
  | None -> Alcotest.fail "expected basic generator"
;;

(** Test: hashmaps() with non-basic keys produces a non-basic generator. *)
let test_hashmaps_non_basic_keys_composite () =
  let key_gen = filter (fun _ -> true) (integers ()) in
  let val_gen = integers () in
  let gen = hashmaps key_gen val_gen () in
  Alcotest.(check bool) "not basic" false (is_basic gen)
;;

(** Test: hashmaps() with non-basic values produces a non-basic generator. *)
let test_hashmaps_non_basic_values_composite () =
  let key_gen = integers () in
  let val_gen = filter (fun _ -> true) (integers ()) in
  let gen = hashmaps key_gen val_gen () in
  Alcotest.(check bool) "not basic" false (is_basic gen)
;;

(* ==== E2E tests ==== *)

(** Test: floats() E2E — values are floats. *)
let%hegel_test test_floats_e2e tc =
  let f = Hegel.draw tc (floats ~allow_nan:false ~allow_infinity:false ()) in
  assert (Float.is_finite f)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: floats() with bounds E2E — values within range. *)
let%hegel_test test_floats_bounds_e2e tc =
  let f =
    Hegel.draw
      tc
      (floats ~min_value:0.0 ~max_value:1.0 ~allow_nan:false ~allow_infinity:false ())
  in
  assert (f >= 0.0 && f <= 1.0)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: text() E2E — values are text strings. *)
let%hegel_test test_text_e2e tc =
  let s = Hegel.draw tc (text ~min_size:1 ~max_size:10 ()) in
  assert (String.length s >= 1)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: binary() E2E — values are byte strings. *)
let%hegel_test test_binary_e2e tc =
  let b = Hegel.draw tc (binary ~min_size:0 ~max_size:10 ()) in
  assert (String.length b >= 0)
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: sampled_from() E2E — values come from the options list. *)
let%hegel_test test_sampled_from_e2e tc =
  let options = [ 10; 20; 30 ] in
  let n = Hegel.draw_silent tc (sampled_from options) in
  assert (List.mem n [ 10; 20; 30 ])
[@@settings Client.settings ~test_cases:10 ()]
;;

(** Test: hashmaps() E2E — values are maps. *)
let%hegel_test test_hashmaps_e2e tc =
  let key_gen = integers ~min_value:0 ~max_value:100 () in
  let val_gen = integers ~min_value:0 ~max_value:100 () in
  let gen = hashmaps key_gen val_gen ~min_size:0 ~max_size:5 () in
  let pairs = Hegel.draw tc gen in
  assert (List.length pairs <= 5)
[@@settings Client.settings ~test_cases:10 ()]
;;

let tests =
  [ Alcotest.test_case "floats schema no bounds" `Quick test_floats_schema_no_bounds
  ; Alcotest.test_case "floats schema all options" `Quick test_floats_schema_all_options
  ; Alcotest.test_case
      "floats schema exclude false"
      `Quick
      test_floats_schema_exclude_false
  ; Alcotest.test_case "floats schema only min" `Quick test_floats_schema_only_min
  ; Alcotest.test_case "text schema" `Quick test_text_schema
  ; Alcotest.test_case "text schema with max" `Quick test_text_schema_with_max
  ; Alcotest.test_case "text schema no max" `Quick test_text_schema_no_max
  ; Alcotest.test_case "binary schema" `Quick test_binary_schema
  ; Alcotest.test_case "binary schema with max" `Quick test_binary_schema_with_max
  ; Alcotest.test_case "binary schema no max" `Quick test_binary_schema_no_max
  ; Alcotest.test_case "sampled_from schema" `Quick test_sampled_from_schema
  ; Alcotest.test_case "hashmaps schema" `Quick test_hashmaps_schema
  ; Alcotest.test_case "hashmaps schema no max" `Quick test_hashmaps_schema_no_max
  ; Alcotest.test_case "hashmaps transform" `Quick test_hashmaps_transform
  ; Alcotest.test_case
      "hashmaps transform bad outer"
      `Quick
      test_hashmaps_transform_bad_outer
  ; Alcotest.test_case
      "hashmaps transform bad inner"
      `Quick
      test_hashmaps_transform_bad_inner
  ; Alcotest.test_case
      "hashmaps non-basic keys composite"
      `Quick
      test_hashmaps_non_basic_keys_composite
  ; Alcotest.test_case
      "hashmaps non-basic values composite"
      `Quick
      test_hashmaps_non_basic_values_composite
  ; Alcotest.test_case "floats e2e" `Quick test_floats_e2e
  ; Alcotest.test_case "floats bounds e2e" `Quick test_floats_bounds_e2e
  ; Alcotest.test_case "text e2e" `Quick test_text_e2e
  ; Alcotest.test_case "binary e2e" `Quick test_binary_e2e
  ; Alcotest.test_case "sampled_from e2e" `Quick test_sampled_from_e2e
  ; Alcotest.test_case "hashmaps e2e" `Quick test_hashmaps_e2e
  ]
;;
