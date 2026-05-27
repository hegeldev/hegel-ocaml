(** E2E tests for the [ppx_hegel_test] PPX.

    These tests exercise the full expansion of [let%hegel_test] including
    the [@@settings ...] attribute. *)

open! Core

let env_var = "ANTITHESIS_OUTPUT_DIR"

let with_env_dir dir ~f =
  let prev = Sys.getenv env_var in
  Core_unix.putenv ~key:env_var ~data:dir;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:env_var ~data:v
      | None -> Core_unix.unsetenv env_var)
    ~f
;;

let with_tempdir ~f =
  let dir = Core_unix.mkdtemp "/tmp/hegel-ppx-test-" in
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

(** A failing test carrying [[@@blobs []]]. Used by the record-mode E2E test
    below; running it should write [test_ppx_hegel_test.ml.corrected] in
    the current working directory (the dune build dir). *)
let%hegel_test record_blobs_failing (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  failwith "deliberate failure for blob recording"
[@@settings Hegel.settings ~test_cases:50 ()] [@@blobs []]
;;

(** A passing test carrying [[@@blobs []]]. Used to verify NO [.corrected]
    is written when the property holds. *)
let%hegel_test record_blobs_passing (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
[@@settings Hegel.settings ~test_cases:3 ()] [@@blobs []]
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

let test_no_settings_runs_with_defaults () =
  with_tempdir ~f:(fun dir ->
    with_env_dir dir ~f:(fun () ->
      no_settings ();
      let path = Filename.concat dir "sdk.jsonl" in
      Alcotest.(check bool) "sdk.jsonl exists" true (Stdlib.Sys.file_exists path)))
;;

(** Workspace-relative path of this test source — matches what the PPX
    threads through to the runtime as [Blobs.t.file]. *)
let blobs_source_path = "test/test_ppx_hegel_test.ml"

(** Path of the [.corrected] sibling produced by recording mode. At test
    runtime, cwd is the workspace root, so this is a relative path the
    test process can read directly. *)
let corrected_path () = blobs_source_path ^ ".corrected"

let remove_if_exists path =
  if Stdlib.Sys.file_exists path
  then (
    try Stdlib.Sys.remove path with
    | _ -> ())
;;

(** Capture a failure blob from a recording run; returns the base64 string
    extracted from the produced [.corrected] file. Looks for the payload
    spliced by [Blobs.write_corrected] (an OCaml list literal) and pulls
    the first quoted string out of it. The [.corrected] file is always
    removed before this function returns, success or failure. *)
let capture_blob () =
  remove_if_exists (corrected_path ());
  Exn.protect
    ~finally:(fun () -> remove_if_exists (corrected_path ()))
    ~f:(fun () ->
      (try record_blobs_failing () with
       | _ -> ());
      Alcotest.(check bool)
        "blob recording produced .corrected"
        true
        (Stdlib.Sys.file_exists (corrected_path ()));
      let corrected = In_channel.read_all (corrected_path ()) in
      let marker = "[@@blobs [ \"" in
      let payload_start =
        match String.substr_index corrected ~pattern:marker with
        | Some i -> i + String.length marker
        | None -> Alcotest.fail "marker not found in .corrected"
      in
      let payload_end = String.index_from_exn corrected payload_start '\"' in
      String.sub corrected ~pos:payload_start ~len:(payload_end - payload_start))
;;

let test_record_mode_writes_corrected_on_failure () =
  remove_if_exists (corrected_path ());
  Exn.protect
    ~finally:(fun () -> remove_if_exists (corrected_path ()))
    ~f:(fun () ->
      (try record_blobs_failing () with
       | _ -> ());
      Alcotest.(check bool)
        ".corrected file produced"
        true
        (Stdlib.Sys.file_exists (corrected_path ()));
      let contents = In_channel.read_all (corrected_path ()) in
      Alcotest.(check bool)
        "contains spliced list with at least one quoted blob"
        true
        (String.is_substring contents ~substring:"[@@blobs [ \""))
;;

let test_record_mode_no_corrected_when_passing () =
  remove_if_exists (corrected_path ());
  record_blobs_passing ();
  Alcotest.(check bool)
    "no .corrected file written for a passing test"
    false
    (Stdlib.Sys.file_exists (corrected_path ()))
;;

let test_replay_mode_blob_still_reproduces () =
  let blob = capture_blob () in
  let location : Hegel.Antithesis.test_location =
    { function_name = "replay_smoke"; file = blobs_source_path; begin_line = 0 }
  in
  let blobs : Hegel.Blobs.t =
    { recorded = [ blob ]; file = blobs_source_path; payload_start = 0; payload_end = 0 }
  in
  (* Body raises on every input — replay reproduces it, so the call
     should re-raise the original exception unchanged. *)
  let raised_msg = ref "" in
  (try
     Hegel.Session.run_hegel_test ~test_location:location ~blobs (fun _tc ->
       failwith "always fails on replay")
   with
   | Failure msg -> raised_msg := msg);
  Alcotest.(check string)
    "original Failure re-raised verbatim"
    "always fails on replay"
    !raised_msg
;;

let test_replay_mode_stale_blob_fails () =
  let blob = capture_blob () in
  let location : Hegel.Antithesis.test_location =
    { function_name = "replay_stale"; file = blobs_source_path; begin_line = 0 }
  in
  let blobs : Hegel.Blobs.t =
    { recorded = [ blob ]; file = blobs_source_path; payload_start = 0; payload_end = 0 }
  in
  let raised_msg = ref "" in
  (try Hegel.Session.run_hegel_test ~test_location:location ~blobs (fun _tc -> ()) with
   | Failure msg -> raised_msg := msg);
  Alcotest.(check bool)
    "raised stale-blob error"
    true
    (String.is_substring !raised_msg ~substring:"did not reproduce the original failure")
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
            "[@@blobs []] record mode writes .corrected on failure"
            `Quick
            test_record_mode_writes_corrected_on_failure
        ; Alcotest.test_case
            "[@@blobs []] record mode skips .corrected on success"
            `Quick
            test_record_mode_no_corrected_when_passing
        ; Alcotest.test_case
            "[@@blobs [<blob>]] replay still-reproducing blob"
            `Quick
            test_replay_mode_blob_still_reproduces
        ; Alcotest.test_case
            "[@@blobs [<blob>]] stale blob raises clear error"
            `Quick
            test_replay_mode_stale_blob_fails
        ] )
    ]
;;
