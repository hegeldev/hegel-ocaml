(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against the native libhegel engine (via {!Hegel_ffi.Ffi}). It manages:
    - Test lifecycle (run_start, test-case loop, mark_complete, run_result)
    - The per-test-case handle threaded through the test function
    - Helper functions (assume, note, target, generate_from_schema)
    - Origin extraction for error reporting *)

open! Core
module Ffi = Hegel_ffi.Ffi

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the engine runs out of choice budget for the current test case
    (StopTest). *)
exception Data_exhausted

(** Raised when the engine detects a flaky strategy definition. *)
exception Flaky_strategy

(** Health checks that can be suppressed during test execution. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(** [health_check_to_string hc] returns the canonical name for [hc]. *)
let health_check_to_string = function
  | Filter_too_much -> "filter_too_much"
  | Too_slow -> "too_slow"
  | Test_cases_too_large -> "test_cases_too_large"
  | Large_initial_test_case -> "large_initial_test_case"
;;

(** Controls how much output Hegel produces during test runs. *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** The database setting: unset, disabled, or a path. *)
type database =
  | Unset
  | Disabled
  | Path of string

(** Controls the test execution mode. *)
type mode =
  | Test_run
  (** Run a full property test: many test cases, shrinking, database
        replay, all other phases. This is the default. *)
  | Single_test_case
  (** Run the test body exactly once, with no shrinking, replay, or
        database. Useful when you want pure data generation without
        property-testing overhead. *)

(** Phases of the test lifecycle. Mirrors [hypothesis.Phase]. *)
type phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

(** [phase_to_string p] returns the lowercase name for [p]. *)
let phase_to_string = function
  | Explicit -> "explicit"
  | Reuse -> "reuse"
  | Generate -> "generate"
  | Target -> "target"
  | Shrink -> "shrink"
;;

(** Configuration for a Hegel test run. *)
type settings =
  { mode : mode
  ; test_cases : int
  ; stateful_step_count : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
  ; suppress_health_check : health_check list
  ; phases : phase list option
  ; print_blob : bool
  ; report_multiple_failures : bool
  }

(** Outcome of replaying a single failure blob. *)
type replay =
  | Undecodable of string
  (** the blob could not be decoded; carries the engine's diagnostic *)
  | Did_not_reproduce (** the blob replayed cleanly — it is stale *)
  | Reproduced of exn (** the blob re-triggered the original failure *)

(** CI environment variables to check for auto-detection. Each entry is
    [(var_name, expected_value)] where [None] means "any value". *)
let ci_vars =
  [ "CI", None
  ; "TF_BUILD", Some "true"
  ; "BUILDKITE", Some "true"
  ; "CIRCLECI", Some "true"
  ; "CIRRUS_CI", Some "true"
  ; "CODEBUILD_BUILD_ID", None
  ; "GITHUB_ACTIONS", Some "true"
  ; "GITLAB_CI", None
  ; "HEROKU_TEST_RUN_ID", None
  ; "TEAMCITY_VERSION", None
  ]
;;

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
let is_in_ci () =
  List.exists ci_vars ~f:(fun (key, expected) ->
    match Sys.getenv key, expected with
    | Some _, None -> true
    | Some v, Some exp -> String.equal v exp
    | None, _ -> false)
;;

(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically. *)
let default_settings () =
  let in_ci = is_in_ci () in
  { mode = Test_run
  ; test_cases = 100
  ; stateful_step_count = 50
  ; verbosity = Normal
  ; seed = None
  ; derandomize = in_ci
  ; database = (if in_ci then Disabled else Unset)
  ; suppress_health_check = []
  ; phases = None
  ; print_blob = false
  ; report_multiple_failures = false
  }
;;

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. *)
let settings ?(test_cases = 100) ?seed () =
  let s = default_settings () in
  let s = { s with test_cases } in
  match seed with
  | Some v -> { s with seed = Some v }
  | None -> s
;;

(** [with_test_cases n s] returns settings [s] with [test_cases] set to [n]. *)
let with_test_cases n s = { s with test_cases = n }

(** [with_stateful_step_count n s] returns settings [s] with [stateful_step_count] set to [n]. *)
let with_stateful_step_count n s = { s with stateful_step_count = n }

(** [with_verbosity v s] returns settings [s] with [verbosity] set to [v]. *)
let with_verbosity v s = { s with verbosity = v }

(** [with_seed seed s] returns settings [s] with [seed] set. *)
let with_seed seed s = { s with seed }

(** [with_derandomize b s] returns settings [s] with [derandomize] set to [b].
*)
let with_derandomize b s = { s with derandomize = b }

(** [with_database db s] returns settings [s] with [database] set to [db]. *)
let with_database db s = { s with database = db }

(** [with_suppress_health_check checks s] returns settings [s] with additional
    health checks suppressed. *)
let with_suppress_health_check checks s =
  { s with suppress_health_check = s.suppress_health_check @ checks }
;;

(** [with_phases phases s] returns settings [s] with [phases] set. *)
let with_phases phases s = { s with phases = Some phases }

(** [with_mode mode s] returns settings [s] with test [mode] set to [mode]. *)
let with_mode mode s = { s with mode }

(** [with_print_blob b s] returns settings [s] with [print_blob] set to [b]. When
    [true], a failing run prints replay instructions (the failure blob), and
    replay runs report which blobs reproduced the failure. *)
let with_print_blob b s = { s with print_blob = b }

(** [with_report_multiple_failures b s] returns settings [s] with [report_multiple_failures] 
    set to [b]. When [true], a failing run reports all the failures it found *)
let with_report_multiple_failures b s = { s with report_multiple_failures = b }

(** Per-test-case state passed explicitly to the test function. Holds the
    native test-case handle, the final-replay flag, whether verbose output is
    on, abort state, the current generation-span depth (used to print only the
    outermost drawn value), and the per-name occurrence counter that numbers
    repeatable draws. *)
type test_case =
  { handle : Ffi.test_case
  ; context : Ffi.context
  ; mode : mode
  ; stateful_step_count : int
  ; is_final : bool
  ; verbosity : verbosity
  ; mutable test_aborted : bool
  ; mutable draw_depth : int
  ; draw_counts : int String.Table.t
  }

(** Domain-local flag to detect nested test cases. *)
let in_test_context : bool Stdlib.Domain.DLS.key =
  Stdlib.Domain.DLS.new_key (fun () -> false)
;;

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. The origin is derived from the exception
    type plus the {e innermost user frame}, so the shrinker
    groups probes for the same bug while keeping failures at distinct source
    lines apart (see {!Ffi.mark_complete}).

    [failwith] and [invalid_arg] raise from within the runtime ([stdlib.ml]), so
    the innermost backtrace slot is the runtime, not the assertion's true source.
    Such frames are skipped so the origin points at the caller's line; without
    this, every same-typed exception in a run would collapse to one origin. *)
let extract_origin exn =
  let bt = Stdlib.Printexc.get_raw_backtrace () in
  let is_runtime_file file = String.is_suffix file ~suffix:"stdlib.ml" in
  let user_location =
    match Stdlib.Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
      Array.find_map slots ~f:(fun slot ->
        match Stdlib.Printexc.Slot.location slot with
        | Some (loc : Stdlib.Printexc.location) when not (is_runtime_file loc.filename) ->
          Some (loc.filename, loc.line_number)
        | _ -> None)
  in
  match user_location with
  | None -> sprintf "%s at :0" (Stdlib.Printexc.exn_slot_name exn)
  | Some (file, line) ->
    sprintf "%s at %s:%d" (Stdlib.Printexc.exn_slot_name exn) file line
;;

(** [with_stop_guard tc f] runs [f ()], translating the engine's per-case abort
    signals into the corresponding OCaml exceptions and marking the test case
    aborted: {!Ffi.Stop_test} becomes {!Data_exhausted} (choice budget exhausted)
    and {!Ffi.Assume_rejected} becomes {!Assume_rejected} (the engine rejected
    the case as invalid, e.g. an unsatisfiable uniqueness constraint). *)
let with_stop_guard tc f =
  try f () with
  | Ffi.Stop_test ->
    tc.test_aborted <- true;
    raise Data_exhausted
  | Ffi.Assume_rejected ->
    tc.test_aborted <- true;
    raise Assume_rejected
;;

(** [generate_from_schema schema tc] generates a value from a schema by drawing
    from the native engine. Raises {!Data_exhausted} if the engine signals
    StopTest. *)
let generate_from_schema schema tc =
  with_stop_guard tc (fun () ->
    Cbor_helpers.decode (Ffi.generate tc.context tc.handle (Cbor_helpers.encode schema)))
;;

let primitive_boolean tc p forced =
  with_stop_guard tc (fun () -> Ffi.primitive_boolean tc.context tc.handle p forced)
;;

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume _tc condition = if not condition then raise Assume_rejected

(** [note tc message] prints [message] to stderr subject to the run's
    {!type:verbosity}: never under [Quiet], only on the final (failing) replay
    under [Normal], and on every test case under [Verbose] or [Debug]. *)
let note tc message =
  let should_print =
    match tc.verbosity with
    | Quiet -> false
    | Normal -> tc.is_final
    | Verbose | Debug -> true
  in
  if should_print then eprintf "%s\n%!" message
;;

(** [draw_display_name tc ~label ~repeatable] returns the display name to print
    for a drawn value, bumping the per-test-case occurrence counter for [label].
    A [repeatable] name is numbered on every occurrence ([label_1], [label_2],
    …), while a non-repeatable name is printed bare. *)
let draw_display_name tc ~label ~repeatable =
  let n = Option.value (Hashtbl.find tc.draw_counts label) ~default:0 + 1 in
  Hashtbl.set tc.draw_counts ~key:label ~data:n;
  if repeatable then sprintf "%s_%d" label n else label
;;

(** [target tc value label] records a targeting observation to guide the search
    engine toward higher values. *)
let target tc value label =
  with_stop_guard tc (fun () -> Ffi.target tc.context tc.handle value label)
;;

(** [start_span ?label tc] starts a generation span for better shrinking. *)
let start_span ?(label = 0) tc =
  if tc.test_aborted
  then ()
  else with_stop_guard tc (fun () -> Ffi.start_span tc.context tc.handle label)
;;

(** [stop_span ?discard tc] ends the current generation span. *)
let stop_span ?(discard = false) tc =
  if tc.test_aborted
  then ()
  else with_stop_guard tc (fun () -> Ffi.stop_span tc.context tc.handle discard)
;;

(** [new_collection tc ~min_size ~max_size] starts an engine-managed collection
    and returns its id. Raises {!Data_exhausted} on StopTest. *)
let new_collection tc ~min_size ~max_size =
  with_stop_guard tc (fun () ->
    Ffi.new_collection tc.context tc.handle ~min_size ~max_size)
;;

(** [collection_more tc ~collection_id] returns whether the engine wants another
    element. Raises {!Data_exhausted} on StopTest. *)
let collection_more tc ~collection_id =
  with_stop_guard tc (fun () -> Ffi.collection_more tc.context tc.handle collection_id)
;;

(** [collection_reject tc ~collection_id] rejects the collection's last element.
    Raises {!Data_exhausted} on StopTest. *)
let collection_reject tc ~collection_id =
  with_stop_guard tc (fun () ->
    Ffi.collection_reject tc.context tc.handle collection_id None)
;;

(** [new_pool tc] creates a new engine-managed variable pool and returns its id.
*)
let new_pool tc = with_stop_guard tc (fun () -> Ffi.new_pool tc.context tc.handle)

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
let pool_add tc ~pool_id =
  with_stop_guard tc (fun () -> Ffi.pool_add tc.context tc.handle ~pool_id)
;;

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true], the variable is also removed from the pool.
    Drawing from an empty pool raises {!Data_exhausted}. *)
let pool_generate tc ~pool_id ?(consume = false) () =
  with_stop_guard tc (fun () -> Ffi.pool_generate tc.context tc.handle ~pool_id ~consume)
;;

(** [new_state_machine tc ~rule_names ~invariant_names] registers an
    engine-owned state machine and returns its id. The engine owns rule
    selection (including swarm testing). *)
let new_state_machine tc ~rule_names ~invariant_names =
  with_stop_guard tc (fun () ->
    Ffi.new_state_machine tc.context tc.handle ~rule_names ~invariant_names)
;;

(** [state_machine_next_rule tc ~state_machine_id] draws the index of the next
    rule to run. Raises {!Data_exhausted} when the engine's choice budget is
    exhausted. *)
let state_machine_next_rule tc ~state_machine_id =
  with_stop_guard tc (fun () ->
    Ffi.state_machine_next_rule tc.context tc.handle ~state_machine_id)
;;

(* ------------------------------------------------------------------ *)
(* Settings translation                                                *)
(* ------------------------------------------------------------------ *)

let ffi_mode = function
  | Test_run -> Ffi.Test_run
  | Single_test_case -> Ffi.Single_test_case
;;

let ffi_verbosity = function
  | Quiet -> Ffi.Quiet
  | Normal -> Ffi.Normal
  | Verbose -> Ffi.Verbose
  | Debug -> Ffi.Debug
;;

let phase_bit = function
  | Explicit -> Ffi.phase_explicit
  | Reuse -> Ffi.phase_reuse
  | Generate -> Ffi.phase_generate
  | Target -> Ffi.phase_target
  | Shrink -> Ffi.phase_shrink
;;

let health_check_bit = function
  | Filter_too_much -> Ffi.hc_filter_too_much
  | Too_slow -> Ffi.hc_too_slow
  | Test_cases_too_large -> Ffi.hc_test_cases_too_large
  | Large_initial_test_case -> Ffi.hc_large_initial_test_case
;;

let bitmask bit_of items = List.fold items ~init:0 ~f:(fun acc x -> acc lor bit_of x)

(** [build_ffi_settings ctx settings ~database_key] allocates and populates a native
    settings handle from the OCaml [settings]. The caller must free it. *)
let build_ffi_settings ctx (settings : settings) ~database_key =
  let s = Ffi.settings_new ctx in
  Ffi.settings_mode ctx s (ffi_mode settings.mode);
  Ffi.settings_test_cases ctx s settings.test_cases;
  Ffi.settings_verbosity ctx s (ffi_verbosity settings.verbosity);
  Ffi.settings_seed ctx s settings.seed;
  Ffi.settings_derandomize ctx s settings.derandomize;
  Ffi.settings_report_multiple_failures ctx s settings.report_multiple_failures;
  (match settings.database with
   | Unset -> ()
   | Disabled -> Ffi.settings_database ctx s (Some "")
   | Path p -> Ffi.settings_database ctx s (Some p));
  Option.iter database_key ~f:(fun k -> Ffi.settings_database_key ctx s (Some k));
  Option.iter settings.phases ~f:(fun phases ->
    Ffi.settings_phases ctx s (bitmask phase_bit phases));
  (match settings.suppress_health_check with
   | [] -> ()
   | checks -> Ffi.settings_suppress_health_check ctx s (bitmask health_check_bit checks));
  s
;;

(** [run_test_case ~mode ~verbose ~test_fn handle is_final] runs [test_fn] over a single 
    native test-case [handle], maps the outcome to a {!Ffi.status}, and marks 
    the case complete. Returns [Some (origin, exn)] when the case was {e interesting}
    (the body raised an unexpected exception), otherwise [None]. Shared by the
    engine-run and failure-blob replay paths. *)
let run_test_case ~(settings : settings) ~test_fn ctx handle is_final =
  let (tc : test_case) =
    { handle
    ; context = ctx
    ; mode = settings.mode
    ; is_final
    ; verbosity = settings.verbosity
    ; stateful_step_count = settings.stateful_step_count
    ; test_aborted = false
    ; draw_depth = 0
    ; draw_counts = String.Table.create ()
    }
  in
  Stdlib.Domain.DLS.set in_test_context true;
  let status, captured =
    match test_fn tc with
    | () -> Ffi.Valid, None
    | exception Assume_rejected -> Ffi.Invalid, None
    | exception Data_exhausted -> Ffi.Overrun, None
    | exception Flaky_strategy -> Ffi.Invalid, None
    | exception exn -> Ffi.Interesting, Some (extract_origin exn, exn)
  in
  Stdlib.Domain.DLS.set in_test_context false;
  Ffi.mark_complete ctx handle status (Option.map captured ~f:fst);
  captured
;;

(** Diagnostic raised when the engine's shrunk counterexample no longer fails on
    the client-driven final replay: the test produced a different outcome for the
    same generated data and is therefore non-deterministic. *)
let flaky_diagnostic =
  "Flaky test detected: Your test produced different outcomes when run with the same \
   generated data — it failed when it previously succeeded, or succeeded when it \
   previously failed. This usually means your test depends on external state such as \
   global variables, system time, or external random number generators."
;;

(** [final_replay ~settings ~ffi_settings ~test_fn ctx failure] performs the
    client-owned {e final replay} of one engine-discovered [failure]: a libhegel
    run only explores (generation, shrinking) and never replays a counterexample
    itself, so the client reads the counterexample's reproduction blob and
    replays it as a standalone final test case — re-running the body so its
    notes and drawn values print for the minimal example. Returns the blob and
    the test's own exception. The engine just produced the blob, so it always
    decodes; a replay that no longer fails means the test is non-deterministic
    and raises {!flaky_diagnostic}. *)
let final_replay ~(settings : settings) ~ffi_settings ~test_fn ctx failure =
  let blob = Option.value_exn (Ffi.failure_blob ctx failure) in
  let tc = Ffi.test_case_from_blob ctx ffi_settings (Some blob) in
  let outcome =
    Exn.protect
      ~finally:(fun () -> Ffi.blob_test_case_free ctx tc)
      ~f:(fun () -> run_test_case ~settings ~test_fn ctx tc true)
  in
  match outcome with
  | Some (_origin, exn) -> blob, exn
  | None -> raise (Failure flaky_diagnostic)
;;

(** [handle_result ~settings ~ffi_settings ~test_fn ~test_location ~single
    ~single_outcome ctx result] inspects a finished run's [result]. A clean run
    returns [unit]. On a run-level error (a failed health check, a
    nondeterministic test, an engine panic) it raises [Failure] with the
    engine's message — there is no counterexample to report.

    On a failed property the engine only explored, so the client owns the final
    replay. In {!Single_test_case} mode the one emitted case already ran as its
    own final case and its [single_outcome] exception is re-raised directly.
    Otherwise each discovered counterexample's blob is replayed via
    {!final_replay} (printing the blob when [settings.print_blob]); a single
    failure re-raises the test's own exception, several distinct failures raise
    an aggregated [Failure]. *)
let handle_result
      ~(settings : settings)
      ~ffi_settings
      ~test_fn
      ~test_location
      ~single
      ~single_outcome
      ctx
      result
  =
  let emit ~passed =
    Option.iter test_location ~f:(fun loc -> Antithesis.emit_assertion loc ~passed)
  in
  match Ffi.result_status ctx result with
  | Run_passed -> emit ~passed:true
  | Run_error ->
    emit ~passed:false;
    raise
      (Failure
         (Option.value
            (Ffi.result_error ctx result)
            ~default:"hegel: run error (no message)"))
  | Run_failed when single ->
    emit ~passed:false;
    (* The single emitted case already ran as its own final case; re-raise the
       test's own exception. An interesting result always carries one. *)
    let _origin, exn = Option.value_exn single_outcome in
    raise exn
  | Run_failed ->
    emit ~passed:false;
    (match Ffi.result_failures ctx result with
     | [ failure ] ->
       let blob, exn = final_replay ~settings ~ffi_settings ~test_fn ctx failure in
       if settings.print_blob then eprintf "failure blob: \"%s\"" blob;
       raise exn
     | failures ->
       let count = List.length failures in
       List.iteri failures ~f:(fun i failure ->
         eprintf "\nFailure %d:\n%!" (i + 1);
         let blob, exn = final_replay ~settings ~ffi_settings ~test_fn ctx failure in
         eprintf "Exception: %s\n%!" (Exn.to_string exn);
         if settings.print_blob then eprintf "Failure blob: \"%s\"\n%!" blob);
       raise (Failure (sprintf "\n%d failures found!" count)))
;;

(** [run_from_engine ctx ~settings ~ffi_settings ~test_fn ~test_location] drives a
    full property run: it starts the engine worker and pulls every scheduled test
    case. The engine only explores (generation, shrinking), so every pumped case
    is non-final — except in {!Single_test_case} mode, where the one emitted case
    is the whole run and is run as final, its outcome kept for the report.
    Discovered counterexamples are replayed from their blobs by {!handle_result}.
    The engine [run] handle is always freed. *)
let run_from_engine ctx ~(settings : settings) ~ffi_settings ~test_fn ~test_location =
  let single =
    match settings.mode with
    | Single_test_case -> true
    | Test_run -> false
  in
  let single_outcome = ref None in
  let run = Ffi.run_start ctx ffi_settings in
  Exn.protect
    ~finally:(fun () -> Ffi.run_free ctx run)
    ~f:(fun () ->
      let rec loop () =
        match Ffi.next_test_case ctx run with
        | None -> ()
        | Some handle ->
          if single
          then single_outcome := run_test_case ~settings ~test_fn ctx handle true
          else
            ignore
              (run_test_case ~settings ~test_fn ctx handle false : (string * exn) option);
          loop ()
      in
      loop ();
      handle_result
        ~settings
        ~ffi_settings
        ~test_fn
        ~test_location
        ~single
        ~single_outcome:!single_outcome
        ctx
        (Ffi.run_result ctx run))
;;

(** [replay_from_blob ~settings ~ffi_settings ~test_fn blob] replays a single failure
    [blob] as a standalone, deterministic test case (no engine worker, no
    shrinking). A corrupt or version-incompatible blob is reported as
    {!Undecodable}. The standalone case is always freed. *)
let replay_from_blob ~(settings : settings) ~ffi_settings ~test_fn blob ctx =
  match Ffi.test_case_from_blob ctx ffi_settings (Some blob) with
  | exception Ffi.Backend_error msg -> Undecodable msg
  | tc ->
    Exn.protect
      ~finally:(fun () -> Ffi.blob_test_case_free ctx tc)
      ~f:(fun () ->
        match run_test_case ~settings ~test_fn ctx tc true with
        | None -> Did_not_reproduce
        | Some (_, exn) -> Reproduced exn)
;;

(** [run_from_blob ~settings ~ffi_settings ~test_fn blob] replays a failure
    [blob] (only the first supplied blob is replayed). A reproducing blob re-raises
    the original exception; a stale or undecodable blob raises a clear [Failure].
*)
let run_from_blob ctx ~(settings : settings) ~ffi_settings ~test_fn blob =
  match replay_from_blob ~settings ~ffi_settings ~test_fn blob ctx with
  | Undecodable msg -> raise (Failure msg)
  | Did_not_reproduce -> raise (Failure "The failure blob did not reproduce an error")
  | Reproduced exn ->
    printf "%s\n" "The failure blob reproduced an error:";
    raise exn
;;

(** [run_test ~settings ?test_location ?database_key ?failure_blobs test_fn] runs
    a property test using the given settings against the native engine.

    With an empty [failure_blobs] (the default) it performs a normal engine run —
    generation, shrinking, and database replay. With a non-empty [failure_blobs]
    it instead replays the first blob as a standalone deterministic case and
    reports whether it reproduced the original failure (subsequent blobs are
    ignored); no engine worker, shrinking, or database is involved.

    @param test_location
      source location of the test, used by the Antithesis integration.
      Provided automatically by the [let%hegel_test] PPX. When omitted, no
      Antithesis assertion is emitted.
    @param database_key
      optional key scoping persisted/replayed failing examples. *)
let run_test
      ~(settings : settings)
      ?test_location
      ?database_key
      ?(failure_blobs = [])
      test_fn
  =
  if Stdlib.Domain.DLS.get in_test_context
  then failwith "Cannot nest test cases - already inside a test case";
  let ctx = Ffi.context_new () in
  let ffi_settings = build_ffi_settings ctx settings ~database_key in
  let run_body () =
    match failure_blobs with
    | [] -> run_from_engine ctx ~settings ~ffi_settings ~test_fn ~test_location
    | blob :: _ -> run_from_blob ctx ~settings ~ffi_settings ~test_fn blob
  in
  Exn.protect
    ~finally:(fun () ->
      Ffi.settings_free ctx ffi_settings;
      Ffi.context_free ctx)
    ~f:run_body
;;

(** [run_hegel_test ?settings ?test_location test_fn] is {!run_test} with
    [settings] defaulting to {!default_settings}. This is the entry point the
    [let%hegel_test] PPX targets and is re-exported as [Hegel.run_hegel_test]. *)
let run_hegel_test ?(settings = default_settings ()) ?test_location ?failure_blobs test_fn
  =
  run_test ~settings ?test_location ?failure_blobs test_fn
;;
