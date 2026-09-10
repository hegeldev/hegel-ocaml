(** E2E tests for the [ppx_hegel_test] PPX.

    These tests exercise the full expansion of [let%hegel_test] including the
    [@@settings ...] attribute. *)

open! Core

let env_var = "ANTITHESIS_OUTPUT_DIR"

let with_env_dir dir ~f =
  let prev = Sys.getenv env_var in
  Core_unix.putenv ~key:env_var ~data:dir;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:env_var ~data:v
      | None -> Core_unix.putenv ~key:env_var ~data:"")
    ~f
;;

let with_tempdir ~f =
  let dir =
    Core_unix.mkdtemp
      (Filename.concat (Stdlib.Filename.get_temp_dir_name ()) "hegel-ppx-test-")
  in
  Exn.protect
    ~finally:(fun () ->
      (try
         Stdlib.Sys.readdir dir
         |> Array.iter ~f:(fun name ->
           try Stdlib.Sys.remove (Filename.concat dir name) with
           | _ -> ())
       with
       | _ -> ());
      try Core_unix.rmdir dir with
      | _ -> ())
    ~f:(fun () -> f dir)
;;

(** A simple passing test, expected to expand into a unit -> unit wrapper. *)
let%hegel_test simple_pass (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.booleans ()) in
  ()
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A failing test. The generated wrapper will raise. *)
let%hegel_test simple_fail (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.booleans ()) in
  failwith "deliberate failure"
[@@settings Hegel.settings ~test_cases:3 ()]
;;

(** A test without an explicit [@@settings] attribute — should fall back to
    default settings. *)
let%hegel_test no_settings (tc : Hegel.test_case) =
  let _ = Hegel.draw tc (Hegel.booleans ()) in
  ()
;;

let test_passing_writes_sdk_jsonl () =
  with_tempdir ~f:(fun dir ->
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
      let id =
        Yojson.Safe.to_string (List.Assoc.find_exn eval_assoc ~equal:String.equal "id")
      in
      Alcotest.(check bool)
        "id mentions test_ppx_hegel_test"
        true
        (String.is_substring id ~substring:"simple_pass in test_ppx_hegel_test");
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
        (String.is_substring file ~substring:"test_ppx_hegel_test.ml")))
;;

let test_failing_writes_condition_false () =
  with_tempdir ~f:(fun dir ->
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
           (List.Assoc.find_exn eval_assoc ~equal:String.equal "condition"))))
;;

let%hegel_rule bump tc n =
  let by = Hegel.draw tc (Hegel.integers ~min_value:1 ~max_value:3 ()) in
  n + by
;;

let invariant_checks = ref 0

let%hegel_invariant positive _ n =
  incr invariant_checks;
  assert (n >= 0)
[@@always_check]
;;

let%hegel_test runs_rule (tc : Hegel.test_case) =
  invariant_checks := 0;
  Hegel.Stateful.run ~init:0 ~rules:[ bump ] ~invariants:[ positive ] ~step_count:5 tc;
  assert (!invariant_checks > 2)
[@@settings Hegel.settings ~test_cases:3 ()]
;;

let test_rule_is_named_by_its_binding () =
  Alcotest.(check string) "rule name" "bump" (Hegel.Stateful.Rule.name bump);
  Alcotest.(check string)
    "invariant name"
    "positive"
    (Hegel.Stateful.Invariant.name positive);
  with_tempdir ~f:(fun dir -> with_env_dir dir ~f:runs_rule)
;;

let test_no_settings_runs_with_defaults () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      no_settings ();
      let path = Filename.concat dir "sdk.jsonl" in
      Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path)))
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
        ; Alcotest.test_case
            "let%hegel_rule / let%hegel_invariant are named after their bindings"
            `Quick
            test_rule_is_named_by_its_binding
        ] )
    ]
;;
