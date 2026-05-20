(** E2E tests for the [ppx_hegel_test] PPX.

    These tests exercise the full expansion of [let%hegel_test] including
    the [@@settings ...] attribute. They run against the real hegel-core
    server, so they live in their own executable (separate from
    [test_hegel]). *)

open! Core

let env_var = "ANTITHESIS_OUTPUT_DIR"

(** These tests always run with [ANTITHESIS_OUTPUT_DIR] set to a fresh
    tempdir, so we don't need to unset the env var. The "env unset" case
    is covered by the unit tests in [test_antithesis.ml]. *)
let with_env_dir dir ~f =
  let prev = Sys.getenv env_var in
  Core_unix.putenv ~key:env_var ~data:dir;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:env_var ~data:v
      | None ->
        (* Best effort: set to empty so subsequent code treats it as unset
           via the dir-exists check failing. The unit tests use the C
           unsetenv stub for true removal. *)
        Core_unix.putenv ~key:env_var ~data:"")
    ~f
;;

let make_tempdir () = Core_unix.mkdtemp "/tmp/hegel-ppx-test-"

(** A simple passing test, expected to expand into a unit -> unit wrapper. *)
let%hegel_test simple_pass (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A failing test. The generated wrapper will raise. *)
let%hegel_test simple_fail (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  failwith "deliberate failure"
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A test without an explicit [@@settings] attribute — should fall back to
    default settings. *)
let%hegel_test no_settings (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
;;

let test_passing_writes_sdk_jsonl () =
  let dir = make_tempdir () in
  with_env_dir dir ~f:(fun () ->
    simple_pass ();
    let path = Filename.concat dir "sdk.jsonl" in
    Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path);
    let lines = In_channel.read_all path |> String.split_lines in
    Alcotest.(check int) "two lines (declaration + evaluation)" 2 (List.length lines);
    let decl = Yojson.Safe.from_string (List.nth_exn lines 0) in
    let eval = Yojson.Safe.from_string (List.nth_exn lines 1) in
    let decl_assoc =
      match decl with
      | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
      | _ -> Alcotest.fail "declaration not wrapped in antithesis_assert"
    in
    let eval_assoc =
      match eval with
      | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
      | _ -> Alcotest.fail "evaluation not wrapped in antithesis_assert"
    in
    Alcotest.(check string)
      "declaration hit"
      "false"
      (Yojson.Safe.to_string (List.Assoc.find_exn decl_assoc ~equal:String.equal "hit"));
    Alcotest.(check string)
      "evaluation hit"
      "true"
      (Yojson.Safe.to_string (List.Assoc.find_exn eval_assoc ~equal:String.equal "hit"));
    Alcotest.(check string)
      "evaluation condition is true (passing test)"
      "true"
      (Yojson.Safe.to_string
         (List.Assoc.find_exn eval_assoc ~equal:String.equal "condition"));
    (* The id encodes <file_without_ext>.<function> passes properties. *)
    let id =
      Yojson.Safe.to_string (List.Assoc.find_exn eval_assoc ~equal:String.equal "id")
    in
    Alcotest.(check bool)
      "id mentions test_ppx_hegel_test"
      true
      (String.is_substring id ~substring:"test_ppx_hegel_test.simple_pass");
    (* location.file should mention the test file. *)
    let loc = List.Assoc.find_exn eval_assoc ~equal:String.equal "location" in
    let loc_assoc =
      match loc with
      | `Assoc f -> f
      | _ -> Alcotest.fail "location not an object"
    in
    let file =
      Yojson.Safe.to_string (List.Assoc.find_exn loc_assoc ~equal:String.equal "file")
    in
    Alcotest.(check bool)
      "location.file mentions test_ppx_hegel_test.ml"
      true
      (String.is_substring file ~substring:"test_ppx_hegel_test.ml"))
;;

let test_failing_writes_condition_false () =
  let dir = make_tempdir () in
  with_env_dir dir ~f:(fun () ->
    (try simple_fail () with
     | _ -> ());
    let path = Filename.concat dir "sdk.jsonl" in
    Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path);
    let lines = In_channel.read_all path |> String.split_lines in
    let eval = Yojson.Safe.from_string (List.nth_exn lines 1) in
    let eval_assoc =
      match eval with
      | `Assoc [ ("antithesis_assert", `Assoc fields) ] -> fields
      | _ -> Alcotest.fail "evaluation not wrapped in antithesis_assert"
    in
    Alcotest.(check string)
      "evaluation condition is false (failing test)"
      "false"
      (Yojson.Safe.to_string
         (List.Assoc.find_exn eval_assoc ~equal:String.equal "condition")))
;;

(** When no [@@settings] is supplied, the PPX must omit [~settings] from the
    call so [Hegel.run_hegel_test] uses its default. We verify that the
    generated wrapper runs to completion. *)
let test_no_settings_runs_with_defaults () =
  let dir = make_tempdir () in
  with_env_dir dir ~f:(fun () ->
    no_settings ();
    let path = Filename.concat dir "sdk.jsonl" in
    Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path))
;;

let () =
  Alcotest.run
    "hegel-ppx-hegel-test"
    [ ( "ppx_hegel_test"
      , [ Alcotest.test_case
            "passing test writes sdk.jsonl"
            `Quick
            test_passing_writes_sdk_jsonl
        ; Alcotest.test_case
            "failing test writes condition:false"
            `Quick
            test_failing_writes_condition_false
        ; Alcotest.test_case
            "no [@@settings] uses defaults"
            `Quick
            test_no_settings_runs_with_defaults
        ] )
    ]
;;
