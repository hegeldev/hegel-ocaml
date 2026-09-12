open! Core
open Hegel
module Unix = Core_unix

(* ==== Settings and profiles ==== *)

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
  ; "bamboo.buildKey"
  ; "HEGEL_DEFAULT_PROFILE"
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

(* Outside CI the default profile is [development]: the base settings. *)
let test_default_settings_not_ci () =
  with_ci_vars_cleared (fun () ->
    let s = Settings.default () in
    Alcotest.(check int) "test_cases" 100 s.test_cases;
    Alcotest.(check bool) "verbosity normal" true (Poly.equal s.verbosity Settings.Normal);
    Alcotest.(check (option int)) "no seed" None s.seed;
    Alcotest.(check bool) "derandomize off" false s.derandomize;
    Alcotest.(check bool) "database unset" true (Poly.equal s.database Settings.Unset);
    Alcotest.(check bool)
      "no health checks suppressed"
      true
      (List.is_empty s.suppress_health_check);
    Alcotest.(check int) "all phases" 5 (List.length s.phases);
    Alcotest.(check bool) "print_blob on" true s.print_blob;
    Alcotest.(check bool) "report_multiple_failures off" false s.report_multiple_failures;
    Alcotest.(check bool) "show_statistics off" false s.show_statistics;
    Alcotest.(check bool)
      "development is the base settings with print_blob on"
      true
      (Poly.equal s { (Settings.from_profile "base") with print_blob = true });
    Alcotest.(check bool)
      "same as from_profile development"
      true
      (Poly.equal s (Settings.from_profile "development"));
    Alcotest.(check bool) "backend default" true (Poly.equal s.backend Settings.Default);
    Alcotest.(check bool)
      "workload backend urandom"
      true
      (Poly.equal (Settings.from_profile "workload").backend Settings.Urandom))
;;

(* On a CI server the engine selects the shipped [ci] profile. *)
let test_default_settings_ci () =
  with_ci_vars_cleared (fun () ->
    Unix.putenv ~key:"CI" ~data:"1";
    let s = Settings.default () in
    Alcotest.(check bool) "derandomize on" true s.derandomize;
    Alcotest.(check bool)
      "database disabled"
      true
      (Poly.equal s.database Settings.Disabled);
    Alcotest.(check bool)
      "too_slow suppressed"
      true
      (Poly.equal s.suppress_health_check [ Settings.Too_slow ]);
    Alcotest.(check bool) "print_blob on" true s.print_blob;
    Alcotest.(check bool)
      "same as from_profile ci"
      true
      (Poly.equal s (Settings.from_profile "ci")))
;;

let test_from_profile_unknown () =
  match Settings.from_profile "hegel_ocaml_no_such_profile" with
  | _ -> Alcotest.fail "expected Usage_error"
  | exception Usage_error msg ->
    Alcotest.(check bool)
      "message names the profile"
      true
      (Test_helpers.contains_substring msg "hegel_ocaml_no_such_profile")
;;

let test_register_profile_round_trip () =
  List.iteri
    Settings.[ Quiet; Normal; Verbose; Debug ]
    ~f:(fun i verbosity ->
      let name = sprintf "hegel_ocaml_test_round_trip_%d" i in
      let s =
        { Settings.test_cases = 7 + i
        ; verbosity
        ; seed = Some (40 + i)
        ; derandomize = true
        ; database = Settings.Path "/tmp/hegel-ocaml-profile-db"
        ; suppress_health_check =
            [ Settings.Filter_too_much; Settings.Large_initial_test_case ]
        ; phases = [ Settings.Generate; Settings.Shrink ]
        ; print_blob = i mod 2 = 0
        ; report_multiple_failures = true
        ; show_statistics = true
        ; backend = (if i mod 2 = 0 then Settings.Default else Settings.Urandom)
        }
      in
      Settings.register_profile name s;
      Alcotest.(check bool) name true (Poly.equal s (Settings.from_profile name)))
;;

let test_set_default_profile () =
  let name = "hegel_ocaml_test_default" in
  Settings.register_profile name { (Settings.default ()) with test_cases = 7 };
  Settings.set_default_profile (Some name);
  Exn.protect
    ~finally:(fun () -> Settings.set_default_profile None)
    ~f:(fun () ->
      Alcotest.(check int)
        "default follows the override"
        7
        (Settings.default ()).test_cases);
  Alcotest.(check int) "cleared" 100 (Settings.default ()).test_cases
;;

(* [hegel.toml] is loaded once per process, so it is exercised in a child: the
   parent writes a config, points [HEGEL_CONFIG] at it, and re-executes this
   binary with [HEGEL_TEST_CONFIG_CHILD] set, which makes the runner call
   [config_child] and exit. *)
let config_child () =
  let n = (Settings.default ()).test_cases in
  if n <> 7 then failwithf "default: expected 7 test cases from hegel.toml, got %d" n ();
  let n = (Settings.from_profile "nightly").test_cases in
  if n <> 7 then failwithf "nightly: expected 7 test cases from hegel.toml, got %d" n ();
  let n = (Settings.from_profile "base").test_cases in
  if n <> 100 then failwithf "base: expected the base 100 test cases, got %d" n ();
  if not (Settings.default ()).print_blob then failwith "default: expected print_blob on";
  if (Settings.from_profile "noblob").print_blob
  then failwith "noblob: expected print_blob off from hegel.toml"
;;

let test_hegel_toml_config () =
  Test_helpers.with_tempdir ~prefix:"hegel-toml" ~f:(fun dir ->
    let path = Filename.concat dir "hegel.toml" in
    Out_channel.write_all
      path
      ~data:
        "default = \"nightly\"\n\n\
         [profiles.nightly]\n\
         test_cases = 7\n\n\
         [profiles.noblob]\n\
         print_blob = false\n";
    let pid =
      Unix.create_process_env
        ~prog:Stdlib.Sys.executable_name
        ~args:[]
        ~env:(`Extend [ "HEGEL_CONFIG", path; "HEGEL_TEST_CONFIG_CHILD", "1" ])
        ()
    in
    match Unix.waitpid pid.pid with
    | Ok () -> ()
    | Error _ as status ->
      Alcotest.failf
        "child reading hegel.toml failed: %s"
        (Unix.Exit_or_signal.to_string_hum status))
;;

let test_settings_create () =
  let s = Settings.create ~test_cases:42 ~seed:7 () in
  Alcotest.(check int) "test_cases" 42 s.test_cases;
  Alcotest.(check (option int)) "seed" (Some 7) s.seed;
  let d = Settings.create () in
  Alcotest.(check int) "default test_cases" 100 d.test_cases;
  Alcotest.(check (option int)) "no seed" None d.seed
;;

let test_health_check_to_string () =
  Alcotest.(check string)
    "filter"
    "filter_too_much"
    (Settings.health_check_to_string Settings.Filter_too_much);
  Alcotest.(check string)
    "slow"
    "too_slow"
    (Settings.health_check_to_string Settings.Too_slow);
  Alcotest.(check string)
    "large"
    "test_cases_too_large"
    (Settings.health_check_to_string Settings.Test_cases_too_large);
  Alcotest.(check string)
    "initial"
    "large_initial_test_case"
    (Settings.health_check_to_string Settings.Large_initial_test_case)
;;

let test_phase_to_string () =
  Alcotest.(check string)
    "explicit"
    "explicit"
    (Settings.phase_to_string Settings.Explicit);
  Alcotest.(check string) "reuse" "reuse" (Settings.phase_to_string Settings.Reuse);
  Alcotest.(check string)
    "generate"
    "generate"
    (Settings.phase_to_string Settings.Generate);
  Alcotest.(check string) "target" "target" (Settings.phase_to_string Settings.Target);
  Alcotest.(check string) "shrink" "shrink" (Settings.phase_to_string Settings.Shrink)
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

(** With backtrace recording off there are no slots, exercising the no-location
    fallback in [extract_origin]. *)
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

(* ==== Sexp renderer tests ==== *)

let render_to_string ~max_width sexp =
  let module Ffi = Hegel_ffi.Ffi in
  let ctx = Ffi.context_new () in
  Exn.protect
    ~finally:(fun () -> Ffi.context_free ctx)
    ~f:(fun () ->
      let options = Ffi.printer_options_new ctx in
      Ffi.printer_options_set_max_width ctx options max_width;
      let p = Ffi.printer_new ctx (Some options) in
      Ffi.printer_options_free ctx options;
      Exn.protect
        ~finally:(fun () -> Ffi.printer_free ctx p)
        ~f:(fun () ->
          Internal.render_sexp ctx p sexp;
          Ffi.printer_value ctx p))
;;

let test_render_sexp_atoms () =
  Alcotest.(check string) "bare" "foo" (render_to_string ~max_width:79 (Sexp.Atom "foo"));
  Alcotest.(check string)
    "escaped"
    "\"needs quoting\""
    (render_to_string ~max_width:79 (Sexp.Atom "needs quoting"));
  Alcotest.(check string)
    "newline never literal"
    "\"a\\nb\""
    (render_to_string ~max_width:79 (Sexp.Atom "a\nb"))
;;

let test_render_sexp_fits_matches_to_string_hum () =
  let sexps =
    [ Sexp.List []
    ; Sexp.List [ Sexp.Atom "a" ]
    ; Sexp.List
        [ Sexp.Atom "a"; Sexp.List []; Sexp.List [ Sexp.Atom "b"; Sexp.Atom "c" ] ]
    ]
  in
  List.iter sexps ~f:(fun sexp ->
    Alcotest.(check string)
      "one-line rendering agrees with to_string_hum"
      (Sexp.to_string_hum sexp)
      (render_to_string ~max_width:79 sexp))
;;

let test_render_sexp_breaks_when_narrow () =
  let sexp =
    Sexp.List [ Sexp.Atom "a"; Sexp.List []; Sexp.List [ Sexp.Atom "b"; Sexp.Atom "c" ] ]
  in
  Alcotest.(check string)
    "outer breaks, inner stays inline"
    "(a\n ()\n (b c))"
    (render_to_string ~max_width:7 sexp);
  Alcotest.(check string)
    "trailing paren glues, forcing the inner group to break too"
    "(a\n ()\n (b\n  c))"
    (render_to_string ~max_width:6 sexp);
  Alcotest.(check string)
    "nested break indents past both parens"
    "(a\n (bbbbb\n  ccccc))"
    (render_to_string
       ~max_width:8
       (Sexp.List [ Sexp.Atom "a"; Sexp.List [ Sexp.Atom "bbbbb"; Sexp.Atom "ccccc" ] ]))
;;

(* ==== Real-engine run tests ==== *)

let int_gen = integers ~min_value:0 ~max_value:100 ()

(** A passing property: drawn ints are always within bounds. *)
let test_run_passing () =
  run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:50 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0 && v <= 100))
;;

exception Boom

(** A failing property re-raises the (shrunk) OCaml exception. *)
let test_run_failing_reraises () =
  let raised =
    try
      run_hegel_test
        ~settings:(Hegel.Settings.create ~test_cases:200 ~seed:1 ())
        (fun tc ->
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
  run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    assume tc (v >= 0);
    assert (v >= 0))
;;

(** Nesting test cases is rejected. *)
let test_run_nested_guard () =
  let got_failure = ref false in
  (try
     run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun _tc ->
       run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun _ -> ()))
   with
   | Failure msg when Test_helpers.contains_substring msg "nest" -> got_failure := true
   | _ -> ());
  Alcotest.(check bool) "nested run rejected" true !got_failure
;;

(** [note] only prints on the final replay; here it just must not raise. *)
let test_note_and_target () =
  run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc int_gen in
    note tc "a note";
    target tc ~label:"v" ~value:(Float.of_int v);
    assert (v >= 0))
;;

(** A non-finite [event_value] observation is an engine-side argument error *)
let test_event_value_non_finite () =
  match
    run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
      event_value tc ~label:"x" ~value:Float.nan)
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error _ -> ()
;;

(** A non-UTF-8 [event] label is an engine-side argument error. *)
let test_event_bad_label () =
  match
    run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
      event tc ~label:"\xff")
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error _ -> ()
;;

(** Force [database = Settings.Unset] (independent of CI auto-detection) to cover the
    [Unset] arm of settings translation. *)
let test_run_database_unset () =
  let settings =
    { (Hegel.Settings.create ~test_cases:3 ()) with database = Settings.Unset }
  in
  run_hegel_test ~settings (fun tc -> ignore (Hegel.draw tc int_gen : int))
;;

(** Exercise Settings.to_ffi branches: phases, disabled database, suppressed
    health checks, derandomize, seed. *)
let test_run_with_full_settings () =
  let settings =
    { (Hegel.Settings.create ~test_cases:10 ~seed:5 ()) with
      derandomize = true
    ; database = Settings.Disabled
    ; phases = [ Settings.Generate ]
    ; suppress_health_check = [ Settings.Filter_too_much ]
    }
  in
  run_hegel_test ~settings (fun tc ->
    let v = Hegel.draw tc int_gen in
    assert (v >= 0))
;;

(** Exercise every phase bit, every health-check bit, a [Path] database, a
    [database_key], and the non-default verbosities. *)
let test_run_all_settings_branches () =
  Test_helpers.with_tempdir ~prefix:"hegel-db" ~f:(fun dir ->
    List.iter [ Settings.Quiet; Settings.Verbose; Settings.Debug ] ~f:(fun verbosity ->
      let settings =
        { (Hegel.Settings.create ~test_cases:1 ~seed:1 ()) with
          verbosity
        ; database = Settings.Path dir
        ; phases =
            [ Settings.Explicit
            ; Settings.Reuse
            ; Settings.Generate
            ; Settings.Target
            ; Settings.Shrink
            ]
        ; suppress_health_check =
            [ Settings.Filter_too_much
            ; Settings.Too_slow
            ; Settings.Test_cases_too_large
            ; Settings.Large_initial_test_case
            ]
        }
      in
      run_hegel_test ~settings ~database_key:"key" (fun tc ->
        ignore (Hegel.draw tc int_gen : int))))
;;

(** [Flaky_strategy] raised from the body is treated as an invalid case. *)
let test_run_flaky_strategy () =
  let settings =
    { (Hegel.Settings.create ~test_cases:20 ()) with
      suppress_health_check = [ Settings.Filter_too_much ]
    }
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
          { (Hegel.Settings.create ~test_cases:300 ~seed:9 ()) with
            report_multiple_failures = true
          }
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

(** [color_enabled] decision table: a HEGEL_COLOR override of 1/0 wins, anything
    else falls back to the tty state. *)
let test_color_enabled () =
  let check name expected ~override ~isatty =
    Alcotest.(check bool) name expected (Internal.color_enabled ~override ~isatty)
  in
  check "1 forces on" true ~override:(Some "1") ~isatty:false;
  check "0 forces off" false ~override:(Some "0") ~isatty:true;
  check "unset: tty" true ~override:None ~isatty:true;
  check "unset: not a tty" false ~override:None ~isatty:false;
  check "junk: falls back to tty" true ~override:(Some "junk") ~isatty:true
;;

(** Run [f] with HEGEL_COLOR set to [value], restoring the variable after. *)
let with_hegel_color value f =
  let saved = Sys.getenv "HEGEL_COLOR" in
  Unix.putenv ~key:"HEGEL_COLOR" ~data:value;
  Exn.protect
    ~finally:(fun () ->
      match saved with
      | Some v -> Unix.putenv ~key:"HEGEL_COLOR" ~data:v
      | None -> Test_helpers.unsetenv "HEGEL_COLOR")
    ~f
;;

(** [stderr_color_enabled] reads HEGEL_COLOR from the environment. *)
let test_stderr_color_enabled () =
  with_hegel_color "1" (fun () ->
    Alcotest.(check bool)
      "HEGEL_COLOR=1 forces on"
      true
      (Internal.stderr_color_enabled ()));
  with_hegel_color "0" (fun () ->
    Alcotest.(check bool)
      "HEGEL_COLOR=0 forces off"
      false
      (Internal.stderr_color_enabled ()))
;;

(** [stderr_color] wraps in SGR codes only when colors are enabled. *)
let test_stderr_color () =
  with_hegel_color "1" (fun () ->
    Alcotest.(check string)
      "enabled wraps"
      "\027[31mx\027[0m"
      (Internal.stderr_color "31" "x"));
  with_hegel_color "0" (fun () ->
    Alcotest.(check string) "disabled is identity" "x" (Internal.stderr_color "31" "x"))
;;

(** [render_diff]'s default rendering prints both values in full, [-]/[+]
    prefixed (red/green when [colored]); an installed renderer (the [hegel.jane]
    [sexp_diff] hook) replaces it until uninstalled. *)
let test_render_diff () =
  let original = Sexp.of_string "(1 2 3)" in
  let updated = Sexp.of_string "(1 9 3)" in
  let colored = Internal.render_diff ~colored:true ~original ~updated in
  Alcotest.(check string)
    "colored renders both values in red/green"
    "\027[31m- (1 2 3)\027[0m\n\027[32m+ (1 9 3)\027[0m"
    colored;
  let plain = Internal.render_diff ~colored:false ~original ~updated in
  Alcotest.(check string) "plain renders both values" "- (1 2 3)\n+ (1 9 3)" plain;
  (* An installed renderer takes over; uninstalling restores the default. *)
  Internal.set_diff_renderer
    (Some (fun ~colored ~original:_ ~updated:_ -> if colored then "custom!" else "custom"));
  Exn.protect
    ~finally:(fun () -> Internal.set_diff_renderer None)
    ~f:(fun () ->
      Alcotest.(check string)
        "installed renderer takes over"
        "custom"
        (Internal.render_diff ~colored:false ~original ~updated));
  Alcotest.(check string)
    "uninstalling restores the default"
    "- (1 2 3)\n+ (1 9 3)"
    (Internal.render_diff ~colored:false ~original ~updated)
;;

let test_run_flaky_on_replay () =
  let calls = ref 0 in
  let msg =
    try
      run_hegel_test
        ~settings:
          { (Hegel.Settings.default ()) with
            phases = [ Settings.Generate ]
          ; database = Settings.Disabled
          ; verbosity = Settings.Quiet
          }
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

(** A health-check failure is a run-level error (no counterexample), surfaced as
    a [Failure] carrying the engine's error message. *)
let test_run_health_check_failure () =
  let raised =
    try
      run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:50 ()) (fun tc ->
        let v = Hegel.draw tc int_gen in
        (* Always-false precondition: every case is invalid →
           FilterTooMuch. *)
        assume tc (v > 1_000_000));
      false
    with
    | Failure _ -> true
    | _ -> false
  in
  Alcotest.(check bool) "health-check failure surfaced" true raised
;;

(** Exercise the optional-argument default paths of the primitives: [start_span]
    without [~label], [pool_generate] without [~consume]. *)
let test_run_primitive_defaults () =
  run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:3 ()) (fun tc ->
    Internal.start_span tc;
    let v = Hegel.draw tc int_gen in
    Internal.stop_span tc;
    let pool = Internal.new_pool tc in
    let _ = Internal.pool_add tc ~pool in
    let a = Internal.pool_generate tc ~pool () in
    let b = Internal.pool_generate tc ~pool ~consume:true () in
    assert (v >= 0 && a >= 0 && b >= 0))
;;

let test_overrun_case_is_discarded () =
  run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:1 ()) with
        suppress_health_check =
          [ Settings.Test_cases_too_large
          ; Settings.Filter_too_much
          ; Settings.Large_initial_test_case
          ]
      }
    (fun tc ->
       ignore (Hegel.draw_silent tc int_gen : int);
       raise Internal.Data_exhausted)
;;

let tests =
  [ Alcotest.test_case "default settings non-ci" `Quick test_default_settings_not_ci
  ; Alcotest.test_case "default settings ci" `Quick test_default_settings_ci
  ; Alcotest.test_case "from_profile unknown" `Quick test_from_profile_unknown
  ; Alcotest.test_case
      "register_profile round trip"
      `Quick
      test_register_profile_round_trip
  ; Alcotest.test_case "set_default_profile" `Quick test_set_default_profile
  ; Alcotest.test_case "hegel.toml via HEGEL_CONFIG" `Quick test_hegel_toml_config
  ; Alcotest.test_case "Settings.create" `Quick test_settings_create
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
  ; Alcotest.test_case "color_enabled" `Quick test_color_enabled
  ; Alcotest.test_case "overrun case discarded" `Quick test_overrun_case_is_discarded
  ; Alcotest.test_case "stderr_color_enabled" `Quick test_stderr_color_enabled
  ; Alcotest.test_case "stderr_color" `Quick test_stderr_color
  ; Alcotest.test_case "render_diff" `Quick test_render_diff
  ; Alcotest.test_case "run flaky on replay" `Quick test_run_flaky_on_replay
  ; Alcotest.test_case "run passing" `Quick test_run_passing
  ; Alcotest.test_case "run failing re-raises" `Quick test_run_failing_reraises
  ; Alcotest.test_case "run assume rejects" `Quick test_run_assume_rejects
  ; Alcotest.test_case "run nested guard" `Quick test_run_nested_guard
  ; Alcotest.test_case "render_sexp atoms" `Quick test_render_sexp_atoms
  ; Alcotest.test_case
      "render_sexp fits"
      `Quick
      test_render_sexp_fits_matches_to_string_hum
  ; Alcotest.test_case "render_sexp breaks" `Quick test_render_sexp_breaks_when_narrow
  ; Alcotest.test_case "note and target" `Quick test_note_and_target
  ; Alcotest.test_case "event_value non-finite" `Quick test_event_value_non_finite
  ; Alcotest.test_case "event bad label" `Quick test_event_bad_label
  ; Alcotest.test_case "run database unset" `Quick test_run_database_unset
  ; Alcotest.test_case "run with full settings" `Quick test_run_with_full_settings
  ; Alcotest.test_case "run all settings branches" `Quick test_run_all_settings_branches
  ; Alcotest.test_case "run flaky strategy" `Quick test_run_flaky_strategy
  ; Alcotest.test_case "run multiple failures" `Quick test_run_multiple_failures
  ; Alcotest.test_case "run health-check failure" `Quick test_run_health_check_failure
  ; Alcotest.test_case "run primitive defaults" `Quick test_run_primitive_defaults
  ]
;;
