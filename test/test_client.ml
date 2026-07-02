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
    Alcotest.(check bool) "not in ci" false (Internal.is_in_ci ()))
;;

let test_is_in_ci_true_any () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"CODEBUILD_BUILD_ID" ~data:"anything";
    Alcotest.(check bool) "in ci (any value)" true (Internal.is_in_ci ()))
;;

let test_is_in_ci_true_expected () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"GITHUB_ACTIONS" ~data:"true";
    Alcotest.(check bool) "in ci (expected value)" true (Internal.is_in_ci ()))
;;

let test_is_in_ci_false_wrong_value () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"GITHUB_ACTIONS" ~data:"false";
    Alcotest.(check bool) "not in ci (wrong value)" false (Internal.is_in_ci ()))
;;

let test_default_settings_not_ci () =
  with_ci_vars_cleared (fun () ->
    let s = default_settings () in
    Alcotest.(check bool) "derandomize off" false s.derandomize;
    Alcotest.(check bool) "database unset" true (Poly.equal s.database Unset))
;;

let test_default_settings_ci () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"CI" ~data:"1";
    let s = default_settings () in
    Alcotest.(check bool) "derandomize on" true s.derandomize;
    Alcotest.(check bool) "database disabled" true (Poly.equal s.database Disabled))
;;

let test_settings_seed () =
  let s = Hegel.settings ~test_cases:42 ~seed:7 () in
  Alcotest.(check int) "test_cases" 42 s.test_cases;
  Alcotest.(check (option int)) "seed" (Some 7) s.seed;
  let s2 = Hegel.settings () in
  Alcotest.(check (option int)) "no seed" None s2.seed
;;

let test_with_builders () =
  let s =
    default_settings ()
    |> with_test_cases 5
    |> with_verbosity Quiet
    |> with_seed (Some 3)
    |> with_derandomize true
    |> with_database (Path "/tmp/hegel-test-db")
    |> with_suppress_health_check [ Filter_too_much; Too_slow ]
    |> with_phases [ Generate; Shrink ]
    |> with_mode Single_test_case
  in
  Alcotest.(check int) "test_cases" 5 s.test_cases;
  Alcotest.(check (option int)) "seed" (Some 3) s.seed;
  Alcotest.(check bool) "derandomize" true s.derandomize;
  Alcotest.(check bool) "mode" true (Poly.equal s.mode Single_test_case)
;;

let test_health_check_to_string () =
  Alcotest.(check string)
    "filter"
    "filter_too_much"
    (Internal.health_check_to_string Filter_too_much);
  Alcotest.(check string) "slow" "too_slow" (Internal.health_check_to_string Too_slow);
  Alcotest.(check string)
    "large"
    "test_cases_too_large"
    (Internal.health_check_to_string Test_cases_too_large);
  Alcotest.(check string)
    "initial"
    "large_initial_test_case"
    (Internal.health_check_to_string Large_initial_test_case)
;;

let test_phase_to_string () =
  Alcotest.(check string) "explicit" "explicit" (Internal.phase_to_string Explicit);
  Alcotest.(check string) "reuse" "reuse" (Internal.phase_to_string Reuse);
  Alcotest.(check string) "generate" "generate" (Internal.phase_to_string Generate);
  Alcotest.(check string) "target" "target" (Internal.phase_to_string Target);
  Alcotest.(check string) "shrink" "shrink" (Internal.phase_to_string Shrink)
;;

let test_extract_origin () =
  let origin =
    try failwith "boom" with
    | e -> Internal.extract_origin e
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
        | e -> Internal.extract_origin e
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
    | e -> Internal.extract_origin e
  in
  let b =
    try failwith "boom two" with
    | e -> Internal.extract_origin e
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

(* ==== Real-engine run tests ==== *)

let int_gen = Generators.integers ~min_value:0 ~max_value:100 ()

(** A passing property: drawn ints are always within bounds. *)
let test_run_passing () =
  run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0 && v <= 100))
;;

exception Boom

(** A failing property re-raises the (shrunk) OCaml exception. *)
let test_run_failing_reraises () =
  let raised =
    try
      run_hegel_test ~settings:(Hegel.settings ~test_cases:200 ~seed:1 ()) (fun tc ->
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
  run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    assume tc (v >= 0);
    assert (v >= 0))
;;

(** Nesting test cases is rejected. *)
let test_run_nested_guard () =
  let got_failure = ref false in
  (try
     run_hegel_test ~settings:(Hegel.settings ~test_cases:5 ()) (fun _tc ->
       run_hegel_test ~settings:(Hegel.settings ~test_cases:1 ()) (fun _ -> ()))
   with
   | Failure msg when Test_helpers.contains_substring msg "nest" -> got_failure := true
   | _ -> ());
  Alcotest.(check bool) "nested run rejected" true !got_failure
;;

(** Single-test-case mode runs the body and surfaces a failure. *)
let test_single_mode_failure () =
  let settings = default_settings () |> with_mode Single_test_case in
  let raised =
    try
      run_hegel_test ~settings (fun tc ->
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
  run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    note tc "a note";
    target tc (Float.of_int v) "v";
    assert (v >= 0))
;;

(** Force [database = Unset] (independent of CI auto-detection) to cover the
    [Unset] arm of settings translation. *)
let test_run_database_unset () =
  let settings = Hegel.settings ~test_cases:3 () |> with_database Unset in
  run_hegel_test ~settings (fun tc -> ignore (Hegel.draw tc int_gen : int))
;;

(** Exercise build_ffi_settings branches: phases, disabled database,
    suppressed health checks, derandomize, seed. *)
let test_run_with_full_settings () =
  let settings =
    Hegel.settings ~test_cases:10 ~seed:5 ()
    |> with_derandomize true
    |> with_database Disabled
    |> with_phases [ Generate ]
    |> with_suppress_health_check [ Filter_too_much ]
  in
  run_hegel_test ~settings (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0))
;;

(** Exercise every phase bit, every health-check bit, a [Path] database, a
    [database_key], and the non-default verbosities. *)
let test_run_all_settings_branches () =
  Test_helpers.with_tempdir ~prefix:"hegel-db" ~f:(fun dir ->
    List.iter [ Quiet; Verbose; Debug ] ~f:(fun verbosity ->
      let settings =
        Hegel.settings ~test_cases:1 ~seed:1 ()
        |> with_verbosity verbosity
        |> with_database (Path dir)
        |> with_phases [ Explicit; Reuse; Generate; Target; Shrink ]
        |> with_suppress_health_check
             [ Filter_too_much; Too_slow; Test_cases_too_large; Large_initial_test_case ]
      in
      run_hegel_test ~settings ~database_key:"key" (fun tc ->
        ignore (Hegel.draw tc int_gen : int))))
;;

(** [Flaky_strategy] raised from the body is treated as an invalid case. *)
let test_run_flaky_strategy () =
  let settings =
    Hegel.settings ~test_cases:20 () |> with_suppress_health_check [ Filter_too_much ]
  in
  run_hegel_test ~settings (fun tc ->
    let v = Hegel.draw tc int_gen in
    if v >= 50 then raise Internal.Flaky_strategy)
;;

exception A
exception B

(** Two distinct failing assertions surface as a "Multiple failures" report. *)
let test_run_multiple_failures () =
  let msg =
    try
      run_hegel_test
        ~settings:
          (Hegel.settings ~test_cases:300 ~seed:9 () |> with_report_multiple_failures true)
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
      (Test_helpers.contains_substring m "failures found")
  | None -> Alcotest.fail "expected multiple failures"
;;

let test_run_flaky_on_replay () =
  let calls = ref 0 in
  let msg =
    try
      run_hegel_test
        ~settings:
          (Hegel.settings ()
           |> with_phases [ Generate ]
           |> with_database Disabled
           |> with_verbosity Quiet)
        (fun tc ->
           ignore (Hegel.draw tc int_gen : int);
           let i = !calls in
           Int.incr calls;
           assert (i <> 0));
      None
    with
    | Failure m -> Some m
    | _ -> None
  in
  match msg with
  | Some m ->
    Alcotest.(check bool)
      "flaky detected"
      true
      (Test_helpers.contains_substring m "Flaky test detected")
  | None -> Alcotest.fail "expected a flaky failure"
;;

(** A health-check failure is a run-level error (no counterexample), surfaced
    as a [Failure] carrying the engine's error message. *)
let test_run_health_check_failure () =
  let raised =
    try
      run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
        let v = Hegel.draw tc int_gen in
        (* Always-false precondition: every case is invalid → FilterTooMuch. *)
        assume tc (v > 1_000_000));
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
  run_hegel_test ~settings:(Hegel.settings ~test_cases:3 ()) (fun tc ->
    Internal.start_span tc;
    let v = Hegel.draw tc int_gen in
    Internal.stop_span tc;
    let pool_id = Internal.new_pool tc in
    let _ = Internal.pool_add tc ~pool_id in
    let a = Internal.pool_generate tc ~pool_id () in
    let b = Internal.pool_generate tc ~pool_id ~consume:true () in
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
  ; Alcotest.test_case "run flaky on replay" `Quick test_run_flaky_on_replay
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
