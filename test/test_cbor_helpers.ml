open Hegel
open Cbor_helpers

(* --- Round-trip tests --- *)

let test_roundtrip_int () =
  let v = `Int 42 in
  let decoded = decode (encode v) in
  Alcotest.(check int) "int round-trip" 42 (extract_int decoded)

let test_roundtrip_negative_int () =
  let v = `Int (-1) in
  let decoded = decode (encode v) in
  Alcotest.(check int) "negative int round-trip" (-1) (extract_int decoded)

let test_roundtrip_float () =
  let v = `Float 3.14 in
  let decoded = decode (encode v) in
  let f = extract_float decoded in
  Alcotest.(check (float 0.001)) "float round-trip" 3.14 f

let test_roundtrip_string () =
  let v = `Text "hello world" in
  let decoded = decode (encode v) in
  Alcotest.(check string)
    "string round-trip" "hello world" (extract_string decoded)

let test_roundtrip_empty_string () =
  let v = `Text "" in
  let decoded = decode (encode v) in
  Alcotest.(check string) "empty string round-trip" "" (extract_string decoded)

let test_roundtrip_bool_true () =
  let v = `Bool true in
  let decoded = decode (encode v) in
  Alcotest.(check bool) "bool true round-trip" true (extract_bool decoded)

let test_roundtrip_bool_false () =
  let v = `Bool false in
  let decoded = decode (encode v) in
  Alcotest.(check bool) "bool false round-trip" false (extract_bool decoded)

let test_roundtrip_bytes () =
  let v = `Bytes "\x01\x02\x03" in
  let decoded = decode (encode v) in
  Alcotest.(check string)
    "bytes round-trip" "\x01\x02\x03" (extract_bytes decoded)

let test_roundtrip_null () =
  let v = `Null in
  let decoded = decode (encode v) in
  Alcotest.(check bool) "null round-trip" true (is_null decoded)

let test_roundtrip_list () =
  let v = `Array [ `Int 1; `Text "two"; `Bool true ] in
  let decoded = decode (encode v) in
  let lst = extract_list decoded in
  Alcotest.(check int) "list length" 3 (List.length lst);
  Alcotest.(check int) "list[0]" 1 (extract_int (List.nth lst 0));
  Alcotest.(check string) "list[1]" "two" (extract_string (List.nth lst 1));
  Alcotest.(check bool) "list[2]" true (extract_bool (List.nth lst 2))

let test_roundtrip_dict () =
  let v = `Map [ (`Text "key", `Int 99) ] in
  let decoded = decode (encode v) in
  let m = extract_dict decoded in
  Alcotest.(check int) "dict length" 1 (List.length m);
  let k, value = List.hd m in
  Alcotest.(check string) "dict key" "key" (extract_string k);
  Alcotest.(check int) "dict value" 99 (extract_int value)

(* --- Nested structures --- *)

let test_nested_list_of_dicts () =
  let v =
    `Array
      [
        `Map [ (`Text "name", `Text "Alice"); (`Text "age", `Int 30) ];
        `Map [ (`Text "name", `Text "Bob"); (`Text "age", `Int 25) ];
      ]
  in
  let decoded = decode (encode v) in
  let lst = extract_list decoded in
  Alcotest.(check int) "list length" 2 (List.length lst);
  let d0 = extract_dict (List.nth lst 0) in
  let d1 = extract_dict (List.nth lst 1) in
  Alcotest.(check int) "d0 keys" 2 (List.length d0);
  Alcotest.(check int) "d1 keys" 2 (List.length d1)

let test_nested_dict_with_list_values () =
  let v =
    `Map
      [
        (`Text "scores", `Array [ `Int 90; `Int 85; `Int 95 ]);
        (`Text "name", `Text "test");
      ]
  in
  let decoded = decode (encode v) in
  let m = extract_dict decoded in
  Alcotest.(check int) "map keys" 2 (List.length m);
  let scores_kv = List.find (fun (k, _) -> extract_string k = "scores") m in
  let scores = extract_list (snd scores_kv) in
  Alcotest.(check int) "scores length" 3 (List.length scores);
  Alcotest.(check int) "score 0" 90 (extract_int (List.nth scores 0))

(* --- Extractor error tests --- *)

let check_failure msg f =
  let raised = ref false in
  (try
     f ();
     ()
   with Failure m ->
     raised := true;
     Alcotest.(check bool)
       (msg ^ " contains type info")
       true
       (String.length m > 0));
  Alcotest.(check bool) (msg ^ " raised") true !raised

let test_extract_int_wrong_type () =
  check_failure "int from text" (fun () -> ignore (extract_int (`Text "hi")))

let test_extract_float_wrong_type () =
  check_failure "float from int" (fun () -> ignore (extract_float (`Int 42)))

let test_extract_string_wrong_type () =
  check_failure "string from int" (fun () -> ignore (extract_string (`Int 42)))

let test_extract_bool_wrong_type () =
  check_failure "bool from text" (fun () -> ignore (extract_bool (`Text "hi")))

let test_extract_bytes_wrong_type () =
  check_failure "bytes from text" (fun () ->
      ignore (extract_bytes (`Text "hi")))

let test_extract_list_wrong_type () =
  check_failure "list from int" (fun () -> ignore (extract_list (`Int 42)))

let test_extract_dict_wrong_type () =
  check_failure "dict from int" (fun () -> ignore (extract_dict (`Int 42)))

(* --- Extractor with null input --- *)

let test_extract_int_null () =
  check_failure "int from null" (fun () -> ignore (extract_int `Null))

let test_extract_float_null () =
  check_failure "float from null" (fun () -> ignore (extract_float `Null))

let test_extract_string_null () =
  check_failure "string from null" (fun () -> ignore (extract_string `Null))

let test_extract_bool_null () =
  check_failure "bool from null" (fun () -> ignore (extract_bool `Null))

let test_extract_bytes_null () =
  check_failure "bytes from null" (fun () -> ignore (extract_bytes `Null))

let test_extract_list_null () =
  check_failure "list from null" (fun () -> ignore (extract_list `Null))

let test_extract_dict_null () =
  check_failure "dict from null" (fun () -> ignore (extract_dict `Null))

(* --- is_null tests --- *)

let test_is_null_true () =
  Alcotest.(check bool) "null is null" true (is_null `Null)

let test_is_null_false () =
  Alcotest.(check bool) "int is not null" false (is_null (`Int 0))

(* --- type_name tests --- *)

let test_type_name_all () =
  Alcotest.(check string) "null" "null" (type_name `Null);
  Alcotest.(check string) "undefined" "undefined" (type_name `Undefined);
  Alcotest.(check string) "simple" "simple" (type_name (`Simple 0));
  Alcotest.(check string) "bool" "bool" (type_name (`Bool true));
  Alcotest.(check string) "int" "int" (type_name (`Int 0));
  Alcotest.(check string) "float" "float" (type_name (`Float 0.0));
  Alcotest.(check string) "bytes" "bytes" (type_name (`Bytes ""));
  Alcotest.(check string) "text" "text" (type_name (`Text ""));
  Alcotest.(check string) "array" "array" (type_name (`Array []));
  Alcotest.(check string) "map" "map" (type_name (`Map []));
  Alcotest.(check string) "tag" "tag" (type_name (`Tag (0, `Null)))

let tests =
  [
    (* Round-trip *)
    Alcotest.test_case "round-trip int" `Quick test_roundtrip_int;
    Alcotest.test_case "round-trip negative int" `Quick
      test_roundtrip_negative_int;
    Alcotest.test_case "round-trip float" `Quick test_roundtrip_float;
    Alcotest.test_case "round-trip string" `Quick test_roundtrip_string;
    Alcotest.test_case "round-trip empty string" `Quick
      test_roundtrip_empty_string;
    Alcotest.test_case "round-trip bool true" `Quick test_roundtrip_bool_true;
    Alcotest.test_case "round-trip bool false" `Quick test_roundtrip_bool_false;
    Alcotest.test_case "round-trip bytes" `Quick test_roundtrip_bytes;
    Alcotest.test_case "round-trip null" `Quick test_roundtrip_null;
    Alcotest.test_case "round-trip list" `Quick test_roundtrip_list;
    Alcotest.test_case "round-trip dict" `Quick test_roundtrip_dict;
    (* Nested structures *)
    Alcotest.test_case "nested list of dicts" `Quick test_nested_list_of_dicts;
    Alcotest.test_case "nested dict with list values" `Quick
      test_nested_dict_with_list_values;
    (* Extractor errors *)
    Alcotest.test_case "extract_int wrong type" `Quick
      test_extract_int_wrong_type;
    Alcotest.test_case "extract_float wrong type" `Quick
      test_extract_float_wrong_type;
    Alcotest.test_case "extract_string wrong type" `Quick
      test_extract_string_wrong_type;
    Alcotest.test_case "extract_bool wrong type" `Quick
      test_extract_bool_wrong_type;
    Alcotest.test_case "extract_bytes wrong type" `Quick
      test_extract_bytes_wrong_type;
    Alcotest.test_case "extract_list wrong type" `Quick
      test_extract_list_wrong_type;
    Alcotest.test_case "extract_dict wrong type" `Quick
      test_extract_dict_wrong_type;
    (* Extractor with null *)
    Alcotest.test_case "extract_int null" `Quick test_extract_int_null;
    Alcotest.test_case "extract_float null" `Quick test_extract_float_null;
    Alcotest.test_case "extract_string null" `Quick test_extract_string_null;
    Alcotest.test_case "extract_bool null" `Quick test_extract_bool_null;
    Alcotest.test_case "extract_bytes null" `Quick test_extract_bytes_null;
    Alcotest.test_case "extract_list null" `Quick test_extract_list_null;
    Alcotest.test_case "extract_dict null" `Quick test_extract_dict_null;
    (* is_null *)
    Alcotest.test_case "is_null true" `Quick test_is_null_true;
    Alcotest.test_case "is_null false" `Quick test_is_null_false;
    (* type_name *)
    Alcotest.test_case "type_name all types" `Quick test_type_name_all;
  ]
