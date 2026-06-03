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
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
  ; suppress_health_check : health_check list
  ; phases : phase list option
  }

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
  ; verbosity = Normal
  ; seed = None
  ; derandomize = in_ci
  ; database = (if in_ci then Disabled else Unset)
  ; suppress_health_check = []
  ; phases = None
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

(** Per-test-case state passed explicitly to the test function. Holds the
    native test-case handle, the final-replay flag, abort state, and the
    current generation-span depth (used to print only the outermost drawn value
    on the final replay). *)
type test_case =
  { handle : Ffi.test_case
  ; mode : mode
  ; is_final : bool
  ; mutable test_aborted : bool
  ; mutable draw_depth : int
  }

(** Domain-local flag to detect nested test cases. *)
let in_test_context : bool Stdlib.Domain.DLS.key =
  Stdlib.Domain.DLS.new_key (fun () -> false)
;;

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. The origin is derived from the assertion's
    {e location}, not its message, so the shrinker groups probes for the same
    bug together (see {!Ffi.mark_complete}). *)
let extract_origin exn =
  let bt = Stdlib.Printexc.get_raw_backtrace () in
  let file_line =
    match Stdlib.Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
      Array.fold slots ~init:None ~f:(fun acc slot ->
        Option.value_map (Stdlib.Printexc.Slot.location slot) ~default:acc ~f:(fun loc ->
          Some loc))
      |> Option.map ~f:(fun (loc : Stdlib.Printexc.location) ->
        loc.filename, loc.line_number)
  in
  match file_line with
  | None -> sprintf "%s at :0" (Stdlib.Printexc.exn_slot_name exn)
  | Some (file, line) ->
    sprintf "%s at %s:%d" (Stdlib.Printexc.exn_slot_name exn) file line
;;

(** [with_stop_guard tc f] runs [f ()], translating {!Ffi.Stop_test} into
    {!Data_exhausted} and marking the test case aborted. *)
let with_stop_guard tc f =
  try f () with
  | Ffi.Stop_test ->
    tc.test_aborted <- true;
    raise Data_exhausted
;;

(** [generate_from_schema schema tc] generates a value from a schema by drawing
    from the native engine. Raises {!Data_exhausted} if the engine signals
    StopTest. *)
let generate_from_schema schema tc =
  with_stop_guard tc (fun () ->
    Cbor_helpers.decode (Ffi.generate tc.handle (Cbor_helpers.encode schema)))
;;

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume _tc condition = if not condition then raise Assume_rejected

(** [note tc message] records a message that will be printed on the final
    (failing) replay. *)
let note tc message = if tc.is_final then eprintf "%s\n%!" message

(** [target tc value label] records a targeting observation to guide the search
    engine toward higher values. *)
let target tc value label =
  with_stop_guard tc (fun () -> Ffi.target tc.handle value label)
;;

(** [start_span ?label tc] starts a generation span for better shrinking. *)
let start_span ?(label = 0) tc =
  if tc.test_aborted
  then ()
  else with_stop_guard tc (fun () -> Ffi.start_span tc.handle label)
;;

(** [stop_span ?discard tc] ends the current generation span. *)
let stop_span ?(discard = false) tc =
  if tc.test_aborted
  then ()
  else with_stop_guard tc (fun () -> Ffi.stop_span tc.handle discard)
;;

(** [new_collection tc ~min_size ~max_size] starts an engine-managed collection
    and returns its id. Raises {!Data_exhausted} on StopTest. *)
let new_collection tc ~min_size ~max_size =
  with_stop_guard tc (fun () -> Ffi.new_collection tc.handle ~min_size ~max_size)
;;

(** [collection_more tc ~collection_id] returns whether the engine wants another
    element. Raises {!Data_exhausted} on StopTest. *)
let collection_more tc ~collection_id =
  with_stop_guard tc (fun () -> Ffi.collection_more tc.handle collection_id)
;;

(** [collection_reject tc ~collection_id] rejects the collection's last element.
    Raises {!Data_exhausted} on StopTest. *)
let collection_reject tc ~collection_id =
  with_stop_guard tc (fun () -> Ffi.collection_reject tc.handle collection_id None)
;;

(** [new_pool tc] creates a new engine-managed variable pool and returns its id.
*)
let new_pool tc = with_stop_guard tc (fun () -> Ffi.new_pool tc.handle)

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
let pool_add tc ~pool_id = with_stop_guard tc (fun () -> Ffi.pool_add tc.handle ~pool_id)

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true], the variable is also removed from the pool.
    Drawing from an empty pool raises {!Data_exhausted}. *)
let pool_generate tc ~pool_id ?(consume = false) () =
  with_stop_guard tc (fun () -> Ffi.pool_generate tc.handle ~pool_id ~consume)
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

(** [build_ffi_settings settings ~database_key] allocates and populates a native
    settings handle from the OCaml [settings]. The caller must free it. *)
let build_ffi_settings (settings : settings) ~database_key =
  let s = Ffi.settings_new () in
  Ffi.settings_mode s (ffi_mode settings.mode);
  Ffi.settings_test_cases s settings.test_cases;
  Ffi.settings_verbosity s (ffi_verbosity settings.verbosity);
  Ffi.settings_seed s settings.seed;
  Ffi.settings_derandomize s settings.derandomize;
  (match settings.database with
   | Unset -> ()
   | Disabled -> Ffi.settings_database s (Some "")
   | Path p -> Ffi.settings_database s (Some p));
  Option.iter database_key ~f:(fun k -> Ffi.settings_database_key s (Some k));
  Option.iter settings.phases ~f:(fun phases ->
    Ffi.settings_phases s (bitmask phase_bit phases));
  (match settings.suppress_health_check with
   | [] -> ()
   | checks -> Ffi.settings_suppress_health_check s (bitmask health_check_bit checks));
  s
;;

(* ------------------------------------------------------------------ *)
(* Test run                                                            *)
(* ------------------------------------------------------------------ *)

(** [run_test ~settings ?test_location ?database_key test_fn] runs a property
    test using the given settings against the native engine.

    @param test_location
      source location of the test, used by the Antithesis integration.
      Provided automatically by the [let%hegel_test] PPX. When omitted, no
      Antithesis assertion is emitted.
    @param database_key
      optional key scoping persisted/replayed failing examples. *)
let run_test ~(settings : settings) ?test_location ?database_key test_fn =
  if Stdlib.Domain.DLS.get in_test_context
  then failwith "Cannot nest test cases - already inside a test case";
  let ffi_settings = build_ffi_settings settings ~database_key in
  (* OCaml exceptions captured from interesting test cases, keyed by origin.
     For a given bug the engine's final replay is the last interesting case,
     so overwriting by origin leaves us with the shrunk counterexample. *)
  let captured : (string, exn) Hashtbl.t = String.Table.create () in
  let run_ref = ref None in
  let run_body () =
    let run = Ffi.run_start ffi_settings in
    run_ref := Some run;
    let rec loop () =
      match Ffi.next_test_case run with
      | None -> ()
      | Some handle ->
        let is_final = Ffi.is_final_replay handle in
        let tc =
          { handle; mode = settings.mode; is_final; test_aborted = false; draw_depth = 0 }
        in
        (* The final replay of a property-test failure is where drawn values are
           printed; head that block with a marker. Not for [Single_test_case]
           (value inspection), which is not a counterexample. *)
        (match settings.mode with
         | Test_run when is_final -> eprintf "Counterexample found\n%!"
         | _ -> ());
        Stdlib.Domain.DLS.set in_test_context true;
        let status, origin =
          match test_fn tc with
          | () -> Ffi.Valid, None
          | exception Assume_rejected -> Ffi.Invalid, None
          | exception Data_exhausted -> Ffi.Overrun, None
          | exception Flaky_strategy -> Ffi.Invalid, None
          | exception exn ->
            let origin = extract_origin exn in
            Hashtbl.set captured ~key:origin ~data:exn;
            Ffi.Interesting, Some origin
        in
        Stdlib.Domain.DLS.set in_test_context false;
        Ffi.mark_complete handle status origin;
        loop ()
    in
    loop ();
    let result = Ffi.run_result run in
    match Ffi.result_failures result with
    | [] -> ()
    | failures ->
      let exns =
        List.map failures ~f:(fun f ->
          let origin = Option.value (Ffi.failure_origin f) ~default:"" in
          match Hashtbl.find captured origin with
          | Some e -> e
          | None ->
            Failure
              (Option.value
                 (Ffi.failure_diagnostic f)
                 ~default:"hegel: failing example (no diagnostic)"))
      in
      (match exns with
       | [ e ] -> raise e
       | _ ->
         let details =
           List.mapi exns ~f:(fun i e -> sprintf "  %d: %s" i (Exn.to_string e))
           |> String.concat ~sep:"\n"
         in
         raise
           (Failure (sprintf "Multiple failures (%d):\n%s" (List.length exns) details)))
  in
  let emit ~passed =
    Option.iter test_location ~f:(fun loc -> Antithesis.emit_assertion loc ~passed)
  in
  Exn.protect
    ~finally:(fun () ->
      Option.iter !run_ref ~f:Ffi.run_free;
      Ffi.settings_free ffi_settings)
    ~f:(fun () ->
      match run_body () with
      | () -> emit ~passed:true
      | exception e ->
        emit ~passed:false;
        raise e)
;;

(** [run_hegel_test ?settings ?test_location test_fn] is {!run_test} with
    [settings] defaulting to {!default_settings}. This is the entry point the
    [let%hegel_test] PPX targets and is re-exported as [Hegel.run_hegel_test]. *)
let run_hegel_test ?(settings = default_settings ()) ?test_location test_fn =
  run_test ~settings ?test_location test_fn
;;
