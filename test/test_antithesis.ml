(** Unit tests for the [Hegel.Antithesis] integration module (doc-hidden). *)

open! Core
module A = Hegel.Antithesis

let env_var = "ANTITHESIS_OUTPUT_DIR"

(** Run [f] with [ANTITHESIS_OUTPUT_DIR] set to [dir]; restore the previous
    value on exit. *)
let with_env_dir dir ~f =
  let prev = Sys.getenv env_var in
  Core_unix.putenv ~key:env_var ~data:dir;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:env_var ~data:v
      | None -> Test_helpers.unsetenv env_var)
    ~f
;;

(** Run [f] with [ANTITHESIS_OUTPUT_DIR] guaranteed unset. *)
let with_env_unset ~f =
  let prev = Sys.getenv env_var in
  Test_helpers.unsetenv env_var;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:env_var ~data:v
      | None -> Test_helpers.unsetenv env_var)
    ~f
;;

let with_tempdir ~f = Test_helpers.with_tempdir ~prefix:"/tmp/hegel-antithesis-test-" ~f

let sample_location =
  { A.function_name = "my_test"; file = "tests/test_basic.ml"; begin_line = 42 }
;;

let test_extract_file_base_strips_extension () =
  Alcotest.(check string)
    "basename without extension"
    "test_basic"
    (A.extract_file_base "tests/test_basic.ml")
;;

let test_extract_file_base_no_extension () =
  Alcotest.(check string)
    "basename with no extension is unchanged"
    "README"
    (A.extract_file_base "docs/README")
;;

let test_extract_file_base_bare_filename () =
  Alcotest.(check string)
    "bare filename without directory"
    "foo"
    (A.extract_file_base "foo.ml")
;;

let expected_id = "my_test in test_basic passes properties"

let expected_location_json : Yojson.Safe.t =
  `Assoc
    [ "function", `String "my_test"
    ; "file", `String "tests/test_basic.ml"
    ; "begin_line", `Int 42
    ; "begin_column", `Int 0
    ]
;;

let test_assertion_json_declaration () =
  let actual = A.assertion_json sample_location ~hit:false ~condition:false in
  let expected : Yojson.Safe.t =
    `Assoc
      [ ( "antithesis_assert"
        , `Assoc
            [ "hit", `Bool false
            ; "must_hit", `Bool true
            ; "assert_type", `String "always"
            ; "display_type", `String "Always"
            ; "condition", `Bool false
            ; "id", `String expected_id
            ; "message", `String expected_id
            ; "location", expected_location_json
            ] )
      ]
  in
  Alcotest.(check string)
    "declaration JSON"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string actual)
;;

let test_assertion_json_evaluation_passed () =
  let actual = A.assertion_json sample_location ~hit:true ~condition:true in
  let expected : Yojson.Safe.t =
    `Assoc
      [ ( "antithesis_assert"
        , `Assoc
            [ "hit", `Bool true
            ; "must_hit", `Bool true
            ; "assert_type", `String "always"
            ; "display_type", `String "Always"
            ; "condition", `Bool true
            ; "id", `String expected_id
            ; "message", `String expected_id
            ; "location", expected_location_json
            ] )
      ]
  in
  Alcotest.(check string)
    "evaluation JSON"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string actual)
;;

let test_write_jsonl_line_appends () =
  with_tempdir ~f:(fun dir ->
    let path = Filename.concat dir "out.jsonl" in
    A.write_jsonl_line path (`Assoc [ "x", `Int 1 ]);
    A.write_jsonl_line path (`Assoc [ "y", `Int 2 ]);
    let contents = In_channel.read_all path in
    Alcotest.(check string) "two appended lines" "{\"x\":1}\n{\"y\":2}\n" contents)
;;

let test_emit_assertion_round_trips () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      A.emit_assertion sample_location ~passed:true;
      let lines =
        In_channel.read_all (Filename.concat dir "sdk.jsonl") |> String.split_lines
      in
      Alcotest.(check int) "exactly two lines" 2 (List.length lines);
      let decl = Yojson.Safe.from_string (List.nth_exn lines 0) in
      let eval = Yojson.Safe.from_string (List.nth_exn lines 1) in
      let expected_decl = A.assertion_json sample_location ~hit:false ~condition:false in
      let expected_eval = A.assertion_json sample_location ~hit:true ~condition:true in
      Alcotest.(check string)
        "declaration line"
        (Yojson.Safe.to_string expected_decl)
        (Yojson.Safe.to_string decl);
      Alcotest.(check string)
        "evaluation line"
        (Yojson.Safe.to_string expected_eval)
        (Yojson.Safe.to_string eval)))
;;

let test_emit_assertion_failed () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      A.emit_assertion sample_location ~passed:false;
      let lines =
        In_channel.read_all (Filename.concat dir "sdk.jsonl") |> String.split_lines
      in
      let eval = Yojson.Safe.from_string (List.nth_exn lines 1) in
      let expected_eval = A.assertion_json sample_location ~hit:true ~condition:false in
      Alcotest.(check string)
        "evaluation has condition:false"
        (Yojson.Safe.to_string expected_eval)
        (Yojson.Safe.to_string eval)))
;;

let test_is_running_false_when_unset () =
  with_env_unset ~f:(fun () ->
    Alcotest.(check bool) "false when env unset" false (A.is_running_in_antithesis ()))
;;

let test_is_running_true_when_set_and_exists () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      Alcotest.(check bool)
        "true when env set and dir exists"
        true
        (A.is_running_in_antithesis ())))
;;

let test_is_running_raises_when_dir_missing () =
  with_env_dir "/nonexistent/hegel-antithesis-missing" ~f:(fun () ->
    match A.is_running_in_antithesis () with
    | exception _ -> ()
    | _ -> Alcotest.fail "expected exception when env points at missing directory")
;;

let tests =
  [ Alcotest.test_case
      "extract_file_base strips extension"
      `Quick
      test_extract_file_base_strips_extension
  ; Alcotest.test_case
      "extract_file_base no extension"
      `Quick
      test_extract_file_base_no_extension
  ; Alcotest.test_case
      "extract_file_base bare filename"
      `Quick
      test_extract_file_base_bare_filename
  ; Alcotest.test_case
      "assertion_json declaration shape"
      `Quick
      test_assertion_json_declaration
  ; Alcotest.test_case
      "assertion_json evaluation passed"
      `Quick
      test_assertion_json_evaluation_passed
  ; Alcotest.test_case "write_jsonl_line appends" `Quick test_write_jsonl_line_appends
  ; Alcotest.test_case
      "emit_assertion writes 2 lines (passed)"
      `Quick
      test_emit_assertion_round_trips
  ; Alcotest.test_case
      "emit_assertion writes 2 lines (failed)"
      `Quick
      test_emit_assertion_failed
  ; Alcotest.test_case
      "is_running false when unset"
      `Quick
      test_is_running_false_when_unset
  ; Alcotest.test_case
      "is_running true when set + exists"
      `Quick
      test_is_running_true_when_set_and_exists
  ; Alcotest.test_case
      "is_running raises when dir missing"
      `Quick
      test_is_running_raises_when_dir_missing
  ]
;;
