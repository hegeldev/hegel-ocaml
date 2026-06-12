(** Tests for {!Hegel.Client} against the native libhegel engine. *)

open! Core
open Hegel
module Unix = Core_unix

(* ==== Pure configuration tests ==== *)

let all_ci_vars =
  [ "CI"
  ; "TF_BUILD"
  ; "BUILDKITE"
  ; "CIRCLECI"
  ; "CIRRUS_CI"
  ; "CODEBUILD_BUILD_ID"
  ; "GITHUB_ACTIONS"
  ; "GITLAB_CI"
  ; "HEROKU_TEST_RUN_ID"
  ; "TEAMCITY_VERSION"
  ]
;;

let with_ci_vars_cleared f =
  let saved = List.map all_ci_vars ~f:(fun v -> v, Sys.getenv v) in
  List.iter all_ci_vars ~f:Test_helpers.unsetenv;
  Exn.protect
    ~finally:(fun () ->
      List.iter saved ~f:(fun (k, v) ->
        match v with
        | Some v -> Unix.putenv ~key:k ~data:v
        | None -> Test_helpers.unsetenv k))
    ~f
;;

let test_is_in_ci_false () =
  with_ci_vars_cleared (fun () ->
    Alcotest.(check bool) "not in ci" false (Client.is_in_ci ()))
;;

let test_is_in_ci_true_any () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"CODEBUILD_BUILD_ID" ~data:"anything";
    Alcotest.(check bool) "in ci (any value)" true (Client.is_in_ci ()))
;;

let test_is_in_ci_true_expected () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"GITHUB_ACTIONS" ~data:"true";
    Alcotest.(check bool) "in ci (expected value)" true (Client.is_in_ci ()))
;;

let test_is_in_ci_false_wrong_value () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"GITHUB_ACTIONS" ~data:"false";
    Alcotest.(check bool) "not in ci (wrong value)" false (Client.is_in_ci ()))
;;

let test_default_settings_not_ci () =
  with_ci_vars_cleared (fun () ->
    let s = Client.default_settings () in
    Alcotest.(check bool) "derandomize off" false s.derandomize;
    Alcotest.(check bool) "database unset" true (Poly.equal s.database Client.Unset))
;;

let test_default_settings_ci () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"CI" ~data:"1";
    let s = Client.default_settings () in
    Alcotest.(check bool) "derandomize on" true s.derandomize;
    Alcotest.(check bool) "database disabled" true (Poly.equal s.database Client.Disabled))
;;

let test_settings_seed () =
  let s = Client.settings ~test_cases:42 ~seed:7 () in
  Alcotest.(check int) "test_cases" 42 s.test_cases;
  Alcotest.(check (option int)) "seed" (Some 7) s.seed;
  let s2 = Client.settings () in
  Alcotest.(check (option int)) "no seed" None s2.seed
;;

let test_with_builders () =
  let s =
    Client.default_settings ()
    |> Client.with_test_cases 5
    |> Client.with_verbosity Client.Quiet
    |> Client.with_seed (Some 3)
    |> Client.with_derandomize true
    |> Client.with_database (Client.Path "/tmp/hegel-test-db")
    |> Client.with_suppress_health_check [ Client.Filter_too_much; Client.Too_slow ]
    |> Client.with_phases [ Client.Generate; Client.Shrink ]
    |> Client.with_mode Client.Single_test_case
  in
  Alcotest.(check int) "test_cases" 5 s.test_cases;
  Alcotest.(check (option int)) "seed" (Some 3) s.seed;
  Alcotest.(check bool) "derandomize" true s.derandomize;
  Alcotest.(check bool) "mode" true (Poly.equal s.mode Client.Single_test_case)
;;

let test_health_check_to_string () =
  Alcotest.(check string)
    "filter"
    "filter_too_much"
    (Client.health_check_to_string Filter_too_much);
  Alcotest.(check string) "slow" "too_slow" (Client.health_check_to_string Too_slow);
  Alcotest.(check string)
    "large"
    "test_cases_too_large"
    (Client.health_check_to_string Test_cases_too_large);
  Alcotest.(check string)
    "initial"
    "large_initial_test_case"
    (Client.health_check_to_string Large_initial_test_case)
;;

let test_phase_to_string () =
  Alcotest.(check string) "explicit" "explicit" (Client.phase_to_string Explicit);
  Alcotest.(check string) "reuse" "reuse" (Client.phase_to_string Reuse);
  Alcotest.(check string) "generate" "generate" (Client.phase_to_string Generate);
  Alcotest.(check string) "target" "target" (Client.phase_to_string Target);
  Alcotest.(check string) "shrink" "shrink" (Client.phase_to_string Shrink)
;;

let test_extract_origin () =
  let origin =
    try failwith "boom" with
    | e -> Client.extract_origin e
  in
  Alcotest.(check bool)
    "origin mentions Failure"
    true
    (Test_helpers.contains_substring origin "Failure")
;;

(** With backtrace recording off there are no slots, exercising the
    no-location fallback in [extract_origin]. *)
let test_extract_origin_no_backtrace () =
  let was = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace false;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace was)
    ~f:(fun () ->
      let origin =
        try failwith "boom" with
        | e -> Client.extract_origin e
      in
      Alcotest.(check bool)
        "fallback origin mentions Failure"
        true
        (Test_helpers.contains_substring origin "Failure"))
;;

(* Two same-typed exceptions ([Failure]) raised at different source lines must
   yield distinct origins: [extract_origin] keys on the innermost user frame,
   not the message. The two [failwith]s are inline (each a distinct, non-tail
   call site the backtrace records) rather than in helper functions, which would
   be tail-call-eliminated and collapse to the caller's frame. *)
let test_extract_origin_distinct_lines () =
  let a =
    try failwith "boom one" with
    | e -> Client.extract_origin e
  in
  let b =
    try failwith "boom two" with
    | e -> Client.extract_origin e
  in
  Alcotest.(check bool)
    "both origins mention Failure"
    true
    (Test_helpers.contains_substring a "Failure"
     && Test_helpers.contains_substring b "Failure");
  Alcotest.(check bool)
    "same-typed exceptions at different lines get distinct origins"
    false
    (String.equal a b)
;;

(** [failure_exn] prefers the captured OCaml exception, falls back to the
    engine's panic message, and survives a missing panic message. *)
let test_failure_exn () =
  let captured = Failure "captured" in
  Alcotest.(check bool)
    "captured exception wins"
    true
    (phys_equal
       (Client.failure_exn ~captured_exn:(Some captured) ~panic_message:(Some "panic"))
       captured);
  (match Client.failure_exn ~captured_exn:None ~panic_message:(Some "panic") with
   | Failure m -> Alcotest.(check string) "panic message used" "panic" m
   | e -> Alcotest.failf "expected Failure, got %s" (Exn.to_string e));
  match Client.failure_exn ~captured_exn:None ~panic_message:None with
  | Failure m ->
    Alcotest.(check bool)
      "default message"
      true
      (Test_helpers.contains_substring m "no panic message")
  | e -> Alcotest.failf "expected Failure, got %s" (Exn.to_string e)
;;

(* ==== Real-engine run tests ==== *)

let int_gen = Generators.integers ~min_value:0 ~max_value:100 ()

(** A passing property: drawn ints are always within bounds. *)
let test_run_passing () =
  Client.run_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0 && v <= 100))
;;

exception Boom

(** A failing property re-raises the (shrunk) OCaml exception. *)
let test_run_failing_reraises () =
  let raised =
    try
      Client.run_test ~settings:(Client.settings ~test_cases:200 ~seed:1 ()) (fun tc ->
        let v = Hegel.draw tc int_gen in
        if v >= 10 then raise Boom);
      None
    with
    | e -> Some e
  in
  match raised with
  | Some Boom -> ()
  | Some other -> Alcotest.failf "expected Boom, got %s" (Exn.to_string other)
  | None -> Alcotest.fail "expected a failure"
;;

(** [assume false] rejects cases without failing the run. *)
let test_run_assume_rejects () =
  Client.run_test ~settings:(Client.settings ~test_cases:20 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    Client.assume tc (v >= 0);
    assert (v >= 0))
;;

(** Nesting test cases is rejected. *)
let test_run_nested_guard () =
  let got_failure = ref false in
  (try
     Client.run_test ~settings:(Client.settings ~test_cases:5 ()) (fun _tc ->
       Client.run_test ~settings:(Client.settings ~test_cases:1 ()) (fun _ -> ()))
   with
   | Failure msg when Test_helpers.contains_substring msg "nest" -> got_failure := true
   | _ -> ());
  Alcotest.(check bool) "nested run rejected" true !got_failure
;;

(** Single-test-case mode runs the body and surfaces a failure. *)
let test_single_mode_failure () =
  let settings = Client.default_settings () |> Client.with_mode Client.Single_test_case in
  let raised =
    try
      Client.run_test ~settings (fun tc ->
        let _ = Hegel.draw tc int_gen in
        raise Boom);
      false
    with
    | Boom -> true
    | _ -> false
  in
  Alcotest.(check bool) "single-mode failure re-raised" true raised
;;

(** [note] only prints on the final replay; here it just must not raise. *)
let test_note_and_target () =
  Client.run_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    Client.note tc "a note";
    Client.target tc (Float.of_int v) "v";
    assert (v >= 0))
;;

(** Force [database = Unset] (independent of CI auto-detection) to cover the
    [Unset] arm of settings translation. *)
let test_run_database_unset () =
  let settings = Client.settings ~test_cases:3 () |> Client.with_database Client.Unset in
  Client.run_test ~settings (fun tc -> ignore (Hegel.draw tc int_gen : int))
;;

(** Exercise build_ffi_settings branches: phases, disabled database,
    suppressed health checks, derandomize, seed. *)
let test_run_with_full_settings () =
  let settings =
    Client.settings ~test_cases:10 ~seed:5 ()
    |> Client.with_derandomize true
    |> Client.with_database Client.Disabled
    |> Client.with_phases [ Client.Generate ]
    |> Client.with_suppress_health_check [ Client.Filter_too_much ]
  in
  Client.run_test ~settings (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0))
;;

(** Exercise every phase bit, every health-check bit, a [Path] database, a
    [database_key], and the non-default verbosities. *)
let test_run_all_settings_branches () =
  Test_helpers.with_tempdir ~prefix:"hegel-db" ~f:(fun dir ->
    List.iter [ Client.Quiet; Client.Verbose; Client.Debug ] ~f:(fun verbosity ->
      let settings =
        Client.settings ~test_cases:1 ~seed:1 ()
        |> Client.with_verbosity verbosity
        |> Client.with_database (Client.Path dir)
        |> Client.with_phases
             [ Client.Explicit
             ; Client.Reuse
             ; Client.Generate
             ; Client.Target
             ; Client.Shrink
             ]
        |> Client.with_suppress_health_check
             [ Client.Filter_too_much
             ; Client.Too_slow
             ; Client.Test_cases_too_large
             ; Client.Large_initial_test_case
             ]
      in
      Client.run_test ~settings ~database_key:"key" (fun tc ->
        ignore (Hegel.draw tc int_gen : int))))
;;

(** [Flaky_strategy] raised from the body is treated as an invalid case. *)
let test_run_flaky_strategy () =
  let settings =
    Client.settings ~test_cases:20 ()
    |> Client.with_suppress_health_check [ Client.Filter_too_much ]
  in
  Client.run_test ~settings (fun tc ->
    let v = Hegel.draw tc int_gen in
    if v >= 50 then raise Client.Flaky_strategy)
;;

exception A
exception B

(** Two distinct failing assertions surface as a "Multiple failures" report. *)
let test_run_multiple_failures () =
  let msg =
    try
      Client.run_test
        ~settings:
          (Client.settings ~test_cases:300 ~seed:9 ()
           |> Client.with_report_multiple_failures true)
        (fun tc ->
           let v = Hegel.draw tc int_gen in
           if v >= 60 then raise A;
           if v <= 30 then raise B);
      None
    with
    | Failure m -> Some m
    | _ -> None
  in
  match msg with
  | Some m ->
    Alcotest.(check bool)
      "reports multiple failures"
      true
      (Test_helpers.contains_substring m "Multiple failures")
  | None -> Alcotest.fail "expected multiple failures"
;;

(** A health-check failure is a run-level error (no counterexample), surfaced
    as a [Failure] carrying the engine's error message. *)
let test_run_health_check_failure () =
  let raised =
    try
      Client.run_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
        let v = Hegel.draw tc int_gen in
        (* Always-false precondition: every case is invalid → FilterTooMuch. *)
        Client.assume tc (v > 1_000_000));
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  Alcotest.(check bool) "health-check failure surfaced" true raised
;;

(** Exercise the optional-argument default paths of the primitives:
    [start_span] without [~label], [pool_generate] without [~consume]. *)
let test_run_primitive_defaults () =
  Client.run_test ~settings:(Client.settings ~test_cases:3 ()) (fun tc ->
    Client.start_span tc;
    let v = Hegel.draw tc int_gen in
    Client.stop_span tc;
    let pool_id = Client.new_pool tc in
    let _ = Client.pool_add tc ~pool_id in
    let a = Client.pool_generate tc ~pool_id () in
    let b = Client.pool_generate tc ~pool_id ~consume:true () in
    assert (v >= 0 && a >= 0 && b >= 0))
;;

let tests =
  [ Alcotest.test_case "is_in_ci false" `Quick test_is_in_ci_false
  ; Alcotest.test_case "is_in_ci any-value" `Quick test_is_in_ci_true_any
  ; Alcotest.test_case "is_in_ci expected-value" `Quick test_is_in_ci_true_expected
  ; Alcotest.test_case "is_in_ci wrong-value" `Quick test_is_in_ci_false_wrong_value
  ; Alcotest.test_case "default settings non-ci" `Quick test_default_settings_not_ci
  ; Alcotest.test_case "default settings ci" `Quick test_default_settings_ci
  ; Alcotest.test_case "settings seed" `Quick test_settings_seed
  ; Alcotest.test_case "with_* builders" `Quick test_with_builders
  ; Alcotest.test_case "health_check_to_string" `Quick test_health_check_to_string
  ; Alcotest.test_case "phase_to_string" `Quick test_phase_to_string
  ; Alcotest.test_case "extract_origin" `Quick test_extract_origin
  ; Alcotest.test_case
      "extract_origin no backtrace"
      `Quick
      test_extract_origin_no_backtrace
  ; Alcotest.test_case
      "extract_origin distinct lines"
      `Quick
      test_extract_origin_distinct_lines
  ; Alcotest.test_case "failure_exn fallbacks" `Quick test_failure_exn
  ; Alcotest.test_case "run passing" `Quick test_run_passing
  ; Alcotest.test_case "run failing re-raises" `Quick test_run_failing_reraises
  ; Alcotest.test_case "run assume rejects" `Quick test_run_assume_rejects
  ; Alcotest.test_case "run nested guard" `Quick test_run_nested_guard
  ; Alcotest.test_case "single mode failure" `Quick test_single_mode_failure
  ; Alcotest.test_case "note and target" `Quick test_note_and_target
  ; Alcotest.test_case "run database unset" `Quick test_run_database_unset
  ; Alcotest.test_case "run with full settings" `Quick test_run_with_full_settings
  ; Alcotest.test_case "run all settings branches" `Quick test_run_all_settings_branches
  ; Alcotest.test_case "run flaky strategy" `Quick test_run_flaky_strategy
  ; Alcotest.test_case "run multiple failures" `Quick test_run_multiple_failures
  ; Alcotest.test_case "run health-check failure" `Quick test_run_health_check_failure
  ; Alcotest.test_case "run primitive defaults" `Quick test_run_primitive_defaults
  ]
;;
