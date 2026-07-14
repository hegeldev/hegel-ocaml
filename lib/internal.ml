(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against the native libhegel engine (via {!Hegel_ffi.Ffi}). It manages:
    - Test lifecycle (run_start, test-case loop, mark_complete, run_result)
    - The per-test-case handle threaded through the test function
    - Helper functions (assume, note, target, the typed generate_* draws)
    - Origin extraction for error reporting *)

open! Core
module Ffi = Hegel_ffi.Ffi

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the engine runs out of choice budget for the current test case
    (StopTest). *)
exception Data_exhausted

(** Raised when the engine detects a flaky strategy definition or when 
the client side pool diverges from the engine side pool. *)
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

(** Phases of the test lifecycle. *)
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
  ; print_blob = true
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

(** [with_suppress_health_check checks s] returns settings [s] with
    [suppress_health_check] set to [checks], replacing any previously suppressed
    list. *)
let with_suppress_health_check checks s = { s with suppress_health_check = checks }

(** [with_phases phases s] returns settings [s] with [phases] set. *)
let with_phases phases s = { s with phases = Some phases }

(** [with_mode mode s] returns settings [s] with test [mode] set to [mode]. *)
let with_mode mode s = { s with mode }

(** [with_print_blob b s] returns settings [s] with [print_blob] set to [b]. When
    [true] (the default), a failing run's report ends with a copy-pasteable
    [rerun with:] line encoding the failure. *)
let with_print_blob b s = { s with print_blob = b }

(** [with_report_multiple_failures b s] returns settings [s] with [report_multiple_failures] 
    set to [b]. When [true], a failing run reports all the failures it found *)
let with_report_multiple_failures b s = { s with report_multiple_failures = b }

(** Draw-name bookkeeping: the per-name occurrence counter that numbers
    repeatable draws ([label_1], [label_2], …). Shared across every clone of a
    test case (see {!clone_test_case}) behind [lock], so concurrent clones number
    their draws in sequence. [lock] serializes only this frontend accounting. *)
type draw_state =
  { counts : int String.Table.t
  ; lock : Caml_threads.Mutex.t
  }

(** [new_draw_state ()] is a fresh, unshared draw-name counter with its own lock,
    for a test case at the head of a clone family. *)
let new_draw_state () =
  { counts = String.Table.create (); lock = Caml_threads.Mutex.create () }
;;

(** Per-test-case state passed explicitly to the test function. Holds the
    native test-case handle, the final-replay flag, whether verbose output is
    on, abort state, the current generation-span depth (used to print only the
    outermost drawn value), and the {!draw_state} numbering repeatable draws (the
    only field shared across a clone family). [note_indent] is the nesting depth
    every {!note}/draw line is indented to (two spaces per level). It starts at 1
    on the final replay, so the whole body sits inside the framed failure report,
    and at 0 otherwise; a caller bumps it further to group sub-output (e.g. the
    draws made within a stateful step nest under its [Step N] header).
    [printed_output] records whether any note/draw line printed (the report needs
    to know whether to separate the body from the exception, and to print that
    separator only once). *)
type test_case =
  { handle : Ffi.test_case
  ; context : Ffi.context
  ; mode : mode
  ; stateful_step_count : int
  ; is_final : bool
  ; verbosity : verbosity
  ; mutable test_aborted : bool
  ; mutable printed_output : bool
  ; mutable draw_depth : int
  ; mutable note_indent : int
  ; draw_state : draw_state
  }

(* Accessors so other library modules can read the internal fields they need
   without the record being exposed (the type is abstract in the interface). *)
let mode (tc : test_case) = tc.mode
let stateful_step_count (tc : test_case) = tc.stateful_step_count
let draw_depth (tc : test_case) = tc.draw_depth
let incr_draw_depth (tc : test_case) = tc.draw_depth <- tc.draw_depth + 1
let decr_draw_depth (tc : test_case) = tc.draw_depth <- tc.draw_depth - 1
let set_test_aborted (tc : test_case) v = tc.test_aborted <- v

(** [with_note_indent tc f] runs [f], nesting every {!note}/draw line it prints
    one level deeper. The depth is restored when [f] raises, so an aborted or
    failing step does not over-indent later output. *)
let with_note_indent (tc : test_case) f =
  tc.note_indent <- tc.note_indent + 1;
  Exn.protect ~finally:(fun () -> tc.note_indent <- tc.note_indent - 1) ~f
;;

(** [clone_test_case tc] forks a fresh {!test_case} onto an independent choice
    stream of the same underlying native test case (see {!Ffi.test_case_clone}),
    paired with its own native context so it can be drawn from on another thread
    concurrently with [tc]. The clone shares [tc]'s outcome and budget but
    generates from its own stream. The {!draw_state} (repeatable-draw numbering) 
    is {e shared} with [tc] behind its lock, and the span depth and note indent 
    are copied so draws forked mid-span stay nested. Only the per-stream abort
    and print flags start fresh, and the immutable configuration is copied. The
    clone owns its handle and context and must be released with {!free_clone}. 
    Only clone from within the test body, before the case completes. *)
let clone_test_case (tc : test_case) =
  let context = Ffi.context_new () in
  { handle = Ffi.test_case_clone tc.context tc.handle
  ; context
  ; mode = tc.mode
  ; stateful_step_count = tc.stateful_step_count
  ; is_final = tc.is_final
  ; verbosity = tc.verbosity
  ; test_aborted = false
  ; printed_output = false
  ; draw_depth = tc.draw_depth
  ; note_indent = tc.note_indent
  ; draw_state = tc.draw_state
  }
;;

(** [free_clone tc] releases a clone made by {!clone_test_case}. *)
let free_clone (tc : test_case) =
  Exn.protect
    ~finally:(fun () -> Ffi.context_free tc.context)
    ~f:(fun () -> Ffi.test_case_free tc.context tc.handle)
;;

(** [with_clone tc f] runs [f] with a fresh clone of [tc] on its own independent
    stream, and always frees the clone afterwards. Use
    it to drive generation from another thread. Clone, hand the clone to a worker,
    and {e join the worker before [f] returns} so the clone outlives every draw
    made on it. Driving a single clone from two threads at once, or letting a
    clone escape past [f], is unsupported. *)
let with_clone (tc : test_case) f =
  let clone = clone_test_case tc in
  Exn.protect ~finally:(fun () -> free_clone clone) ~f:(fun () -> f clone)
;;

(** Domain-local flag to detect nested test cases. *)
let in_test_context : bool Stdlib.Domain.DLS.key =
  Stdlib.Domain.DLS.new_key (fun () -> false)
;;

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. The origin is derived from the exception
    type plus the {e innermost user frame}, so the shrinker
    groups probes for the same bug while keeping failures at distinct source
    lines apart (see {!Ffi.mark_complete}).

    [failwith] and [invalid_arg] raise from within the runtime ([stdlib.ml]),
    and [require]/[require_equal] raise from within this file, so the innermost
    backtrace slot is not the assertion's true source. Such frames are skipped
    so the origin points at the caller's line; without this, every same-typed
    exception in a run would collapse to one origin. *)
let extract_origin exn =
  let bt = Stdlib.Printexc.get_raw_backtrace () in
  let is_runtime_file file =
    String.is_suffix file ~suffix:"stdlib.ml"
    || String.is_suffix file ~suffix:"lib/internal.ml"
  in
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

(** [generate_boolean tc p forced] draws a boolean with probability [p] of
    [true]. If [forced] is [Some b] the value is forced to [b]. Raises
    {!Data_exhausted} on StopTest. *)
let generate_boolean tc p forced =
  with_stop_guard tc (fun () -> Ffi.generate_boolean tc.context tc.handle p forced)
;;

(** [generate_integer tc ~min_value ~max_value] draws an integer in the inclusive
    range. Raises {!Data_exhausted} on StopTest. *)
let generate_integer tc ~min_value ~max_value =
  with_stop_guard tc (fun () ->
    Ffi.generate_integer tc.context tc.handle ~min_value ~max_value)
;;

(** [generate_float tc ...] draws a width-64 float under the given policy. Raises
    {!Data_exhausted} on StopTest. *)
let generate_float
      tc
      ~min_value
      ~max_value
      ~allow_nan
      ~allow_infinity
      ~exclude_min
      ~exclude_max
      ~smallest_nonzero_magnitude
  =
  with_stop_guard tc (fun () ->
    Ffi.generate_float
      tc.context
      tc.handle
      ~min_value
      ~max_value
      ~allow_nan
      ~allow_infinity
      ~exclude_min
      ~exclude_max
      ~smallest_nonzero_magnitude)
;;

(** [generate_bytes tc ~min_size ~max_size] draws a byte string. Raises
    {!Data_exhausted} on StopTest. *)
let generate_bytes tc ~min_size ~max_size =
  with_stop_guard tc (fun () ->
    Ffi.generate_bytes tc.context tc.handle ~min_size ~max_size)
;;

(** [with_string_generator tc make draw] builds a string-generator handle with
    [make tc.context], draws from it with [draw], and always frees the handle.
    Raises {!Data_exhausted} on StopTest and {!Assume_rejected} when the draw
    rejects itself. *)
let with_string_generator tc make =
  with_stop_guard tc (fun () ->
    let sg = make tc.context in
    Exn.protect
      ~finally:(fun () -> Ffi.string_generator_free tc.context sg)
      ~f:(fun () -> Ffi.generate_string tc.context tc.handle sg))
;;

(** [generate_text tc ...] draws a text string over the described alphabet. *)
let generate_text
      tc
      ~min_size
      ~max_size
      ~codec
      ~min_codepoint
      ~max_codepoint
      ~categories
      ~exclude_categories
      ~include_characters
      ~exclude_characters
  =
  with_string_generator tc (fun ctx ->
    Ffi.string_generator_text
      ctx
      ~min_size
      ~max_size
      ~codec
      ~min_codepoint
      ~max_codepoint
      ~categories
      ~exclude_categories
      ~include_characters
      ~exclude_characters)
;;

(** [generate_regex tc ~pattern ~fullmatch] draws a string matching [pattern]. *)
let generate_regex tc ~pattern ~fullmatch =
  with_string_generator tc (fun ctx -> Ffi.string_generator_regex ctx ~pattern ~fullmatch)
;;

(** [generate_email tc] draws an RFC 5321/5322 email address. *)
let generate_email tc = with_string_generator tc Ffi.string_generator_email

(** [generate_url tc] draws an RFC 3986 http/https URL. *)
let generate_url tc = with_string_generator tc Ffi.string_generator_url

(** [generate_domain tc ~max_length] draws an RFC 1035 domain name. *)
let generate_domain tc ~max_length =
  with_string_generator tc (fun ctx -> Ffi.string_generator_domain ctx ~max_length)
;;

(** [generate_date tc] draws a Gregorian date as [(year, month, day)]. *)
let generate_date tc =
  with_stop_guard tc (fun () -> Ffi.generate_date tc.context tc.handle)
;;

(** [generate_time tc] draws a time as [(hour, minute, second, microsecond)]. *)
let generate_time tc =
  with_stop_guard tc (fun () -> Ffi.generate_time tc.context tc.handle)
;;

(** [generate_datetime tc] draws a naive datetime as [(date, time)]. *)
let generate_datetime tc =
  with_stop_guard tc (fun () -> Ffi.generate_datetime tc.context tc.handle)
;;

(** [generate_ipv4 tc] draws an IPv4 address as its 4 network-order bytes. *)
let generate_ipv4 tc =
  with_stop_guard tc (fun () -> Ffi.generate_ipv4 tc.context tc.handle)
;;

(** [generate_ipv6 tc] draws an IPv6 address as its 16 network-order bytes. *)
let generate_ipv6 tc =
  with_stop_guard tc (fun () -> Ffi.generate_ipv6 tc.context tc.handle)
;;

(* ------------------------------------------------------------------ *)
(* ANSI colors                                                         *)
(* ------------------------------------------------------------------ *)

(** ANSI color codes for {!stderr_color}. *)
let ansi_red = "31"

(** [color_enabled ~override ~isatty] decides whether ANSI colors are on: an
    [override] of ["1"]/["0"] (the [HEGEL_COLOR] variable) forces it on/off;
    otherwise follow [isatty]. *)
let color_enabled ~override ~isatty =
  match override with
  | Some "1" -> true
  | Some "0" -> false
  | Some _ | None -> isatty
;;

(** [stderr_color_enabled ()] is {!color_enabled} for the failure report's
    stream: it reads [HEGEL_COLOR] afresh (tests toggle it) and checks whether
    stderr is a terminal. *)
let stderr_color_enabled () =
  color_enabled
    ~override:(Sys.getenv "HEGEL_COLOR")
    ~isatty:(Core_unix.isatty Core_unix.stderr)
;;

(** [stderr_color code s] wraps [s] in the ANSI SGR [code] when colors are
    enabled for stderr (see {!stderr_color_enabled}), else returns [s]
    unchanged. *)
let stderr_color code s =
  if stderr_color_enabled () then sprintf "\027[%sm%s\027[0m" code s else s
;;

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. The [tc] handle is accepted for API symmetry with the other
    per-test-case primitives; the rejection is client-side (raising
    {!Assume_rejected}) and does not consult [tc]. *)
let assume _tc condition = if not condition then raise Assume_rejected

(** [should_print tc] says whether {!note} output is visible for this test
    case under the run's {!type:verbosity}: never under [Quiet], only on the
    final (failing) replay under [Normal], and on every test case under
    [Verbose] or [Debug]. *)
let should_print tc =
  match tc.verbosity with
  | Quiet -> false
  | Normal -> tc.is_final
  | Verbose | Debug -> true
;;

(** [note tc message] prints [message] to stderr subject to {!should_print}.
    Inside the framed failure report (the final replay), every line of a
    (possibly multi-line) message prints indented. *)
let note tc message =
  if should_print tc
  then (
    if tc.note_indent > 0 && not tc.printed_output then eprintf "\n%!";
    tc.printed_output <- true;
    let indent = String.make (2 * tc.note_indent) ' ' in
    let body = String.concat ~sep:("\n" ^ indent) (String.split_lines message) in
    eprintf "%s%s\n%!" indent body)
;;

(** [require tc ?msg condition] fails the current test case when [condition] is
    [false] by raising [Failure msg]. *)
let require _tc ?(msg = "require: condition was false") condition =
  if not condition then raise (Failure msg)
;;

(** [render_diff ~colored ~original ~updated] renders a structural sexp diff
    of the two values: deletions and additions are marked red and green when
    [colored], and with [-] and [+] otherwise. *)
let render_diff ~colored ~original ~updated =
  let diff = Sexp_diff.Algo.diff ~original ~updated () in
  let display_options = Sexp_diff.Display.Display_options.create Two_column in
  if colored
  then Sexp_diff.Display.display_with_ansi_colors display_options diff
  else Sexp_diff.Display.display_as_plain_string display_options diff
;;

(** [require_equal tc ?msg sexp_of lhs rhs] fails the current test case when
    the two values render to different sexps under [sexp_of]. The failure
    report's body shows a structural sexp diff of the two values ([-] lines
    only in [lhs], [+] lines only in [rhs]; red/green on a terminal) before
    [Failure msg] is raised. The diff is only rendered when notes are visible
    (see {!should_print}), so shrink probes don't pay for it. *)
let require_equal tc ?(msg = "require_equal: values differ") sexp_of lhs rhs =
  let original = sexp_of lhs in
  let updated = sexp_of rhs in
  if not (Sexp.equal original updated)
  then (
    if should_print tc
    then (
      let rendered = render_diff ~colored:(stderr_color_enabled ()) ~original ~updated in
      note tc (sprintf "%s (- lhs / + rhs):\n%s" msg rendered));
    raise (Failure msg))
;;

(** [draw_display_name tc ~label ~repeatable] returns the display name to print
    for a drawn value, bumping the occurrence counter for [label]. A [repeatable]
    name is numbered on every occurrence ([label_1], [label_2], …), while a
    non-repeatable name is printed bare. The counter is shared across test cases. *)
let draw_display_name tc ~label ~repeatable =
  let ds = tc.draw_state in
  Caml_threads.Mutex.lock ds.lock;
  let n =
    Exn.protect
      ~finally:(fun () -> Caml_threads.Mutex.unlock ds.lock)
      ~f:(fun () ->
        let n = Option.value (Hashtbl.find ds.counts label) ~default:0 + 1 in
        Hashtbl.set ds.counts ~key:label ~data:n;
        n)
  in
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
    Drawing from an empty pool raises {!Assume_rejected}. *)
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

type case_outcome =
  { status : Ffi.status
  ; interesting : (string * exn) option
  ; printed_output : bool
  }

(** [run_test_case ~settings ~test_fn ?note_indent ctx handle is_final] runs
    [test_fn] over a single native test-case [handle], maps the outcome to a
    {!Ffi.status}, and marks the case complete. [note_indent] is the starting
    nesting depth of note/draw lines. Shared by the engine-run and failure-blob
    replay paths. *)
let run_test_case ~(settings : settings) ~test_fn ?(note_indent = 0) ctx handle is_final =
  let (tc : test_case) =
    { handle
    ; context = ctx
    ; mode = settings.mode
    ; is_final
    ; verbosity = settings.verbosity
    ; stateful_step_count = settings.stateful_step_count
    ; test_aborted = false
    ; printed_output = false
    ; draw_depth = 0
    ; note_indent
    ; draw_state = new_draw_state ()
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
  { status; interesting = captured; printed_output = tc.printed_output }
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
    notes and drawn values print for the minimal example. Returns the blob, the
    test's own exception, and whether the replay printed any note/draw line.
    The engine just produced the blob, so it always decodes; a replay that no
    longer fails means the test is non-deterministic and raises
    {!flaky_diagnostic}. *)
let final_replay ~(settings : settings) ~ffi_settings ~test_fn ctx failure =
  let blob = Option.value_exn (Ffi.failure_blob ctx failure) in
  let tc = Ffi.test_case_from_blob ctx ffi_settings (Some blob) in
  let outcome =
    Exn.protect
      ~finally:(fun () -> Ffi.test_case_free ctx tc)
      ~f:(fun () -> run_test_case ~settings ~test_fn ~note_indent:1 ctx tc true)
  in
  match outcome.interesting with
  | Some (_origin, exn) -> blob, exn, outcome.printed_output
  | None -> raise (Failure flaky_diagnostic)
;;

(** Width the framed failure report's header rule is padded to. *)
let frame_width = 72

let print_failure_header ~cases_run ~cases_discarded test_location =
  let title =
    match test_location with
    | None -> "Failure"
    | Some (loc : Antithesis.test_location) ->
      sprintf "Failure: %s (%s:%d)" loc.function_name loc.file loc.begin_line
  in
  let prefix = sprintf "--- %s " title in
  let rule = prefix ^ String.make (max 3 (frame_width - String.length prefix)) '-' in
  eprintf
    "%s\nFalsified after %d test case%s (%d discarded):\n%!"
    (stderr_color ansi_red rule)
    cases_run
    (if cases_run = 1 then "" else "s")
    cases_discarded
;;

let print_failure_body ~(settings : settings) ~from_ppx ~blob ~exn ~printed_output =
  if printed_output then eprintf "\n%!";
  eprintf "Exception: %s\n%!" (Stdlib.Printexc.to_string exn);
  if settings.print_blob
  then
    if from_ppx
    then eprintf "rerun with: [@@failure_blobs [ \"%s\" ]]\n%!" blob
    else eprintf "rerun with: ~failure_blobs:[ \"%s\" ]\n%!" blob
;;

let handle_result
      ~(settings : settings)
      ~ffi_settings
      ~test_fn
      ~test_location
      ~from_ppx
      ~single
      ~single_outcome
      ~cases_run
      ~cases_discarded
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
    (* Failures are caller-owned snapshots, independent of the run result. *)
    let failures = Ffi.result_failures ctx result in
    Exn.protect
      ~finally:(fun () -> List.iter failures ~f:(fun f -> Ffi.failure_free ctx f))
      ~f:(fun () ->
        match failures with
        | [ failure ] ->
          print_failure_header ~cases_run ~cases_discarded test_location;
          let blob, exn, printed_output =
            final_replay ~settings ~ffi_settings ~test_fn ctx failure
          in
          print_failure_body ~settings ~from_ppx ~blob ~exn ~printed_output;
          raise exn
        | failures ->
          let count = List.length failures in
          print_failure_header ~cases_run ~cases_discarded test_location;
          List.iteri failures ~f:(fun i failure ->
            eprintf
              "\n%s%!"
              (stderr_color ansi_red (sprintf "Failure %d of %d:" (i + 1) count));
            let blob, exn, printed_output =
              final_replay ~settings ~ffi_settings ~test_fn ctx failure
            in
            print_failure_body ~settings ~from_ppx ~blob ~exn ~printed_output);
          raise (Failure (sprintf "%d failures found!" count)))
;;

(** [run_from_engine ctx ~settings ~ffi_settings ~test_fn ~test_location] drives a
    full property run: it starts the engine worker and pulls every scheduled test
    case. The engine only explores (generation, shrinking), so every pumped case
    is non-final — except in {!Single_test_case} mode, where the one emitted case
    is the whole run and is run as final, its outcome kept for the report.
    Discovered counterexamples are replayed from their blobs by {!handle_result}.
    The engine [run] handle is always freed. *)
let run_from_engine
      ctx
      ~(settings : settings)
      ~ffi_settings
      ~test_fn
      ~test_location
      ~from_ppx
  =
  let single =
    match settings.mode with
    | Single_test_case -> true
    | Test_run -> false
  in
  let single_outcome = ref None in
  let seen_interesting = ref false in
  let cases_run = ref 0 in
  let cases_discarded = ref 0 in
  let run = Ffi.run_start ctx ffi_settings in
  Exn.protect
    ~finally:(fun () -> Ffi.run_free ctx run)
    ~f:(fun () ->
      let rec loop () =
        match Ffi.next_test_case ctx run with
        | None -> ()
        | Some handle ->
          (* Handles from [next_test_case] are caller-owned; free each once its
             case has been marked complete by [run_test_case]. In single mode
             the one emitted case is the whole run, so it runs as final. *)
          Exn.protect
            ~finally:(fun () -> Ffi.test_case_free ctx handle)
            ~f:(fun () ->
              let outcome = run_test_case ~settings ~test_fn ctx handle single in
              if not !seen_interesting
              then (
                match outcome.status with
                | Ffi.Interesting ->
                  Int.incr cases_run;
                  seen_interesting := true
                | Ffi.Valid -> Int.incr cases_run
                | Ffi.Invalid | Ffi.Overrun -> Int.incr cases_discarded);
              if single then single_outcome := outcome.interesting);
          loop ()
      in
      loop ();
      (* The run result is a caller-owned snapshot, independent of the run. *)
      let result = Ffi.run_result ctx run in
      Exn.protect
        ~finally:(fun () -> Ffi.run_result_free ctx result)
        ~f:(fun () ->
          handle_result
            ~settings
            ~ffi_settings
            ~test_fn
            ~test_location
            ~from_ppx
            ~single
            ~single_outcome:!single_outcome
            ~cases_run:!cases_run
            ~cases_discarded:!cases_discarded
            ctx
            result))
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
      ~finally:(fun () -> Ffi.test_case_free ctx tc)
      ~f:(fun () ->
        let outcome = run_test_case ~settings ~test_fn ctx tc true in
        match outcome.interesting with
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
    eprintf "The failure blob reproduced an error:\n%!";
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
    @param from_ppx
    [true] when the run is driven by the [let%hegel_test] PPX; only set by the
    PPX. Selects the [[@@failure_blobs [...]]] attribute form of the [rerun with:]
    hint vs. the [~failure_blobs] argument form a plain caller would use.
    @param database_key
    optional key scoping persisted/replayed failing examples and, under [derandomize],
    the per-test seed. Defaults to the test's [test_location] (as
    [file:function_name]) so each [let%hegel_test] gets a stable, distinct
    key; pass an explicit key to override. When both are absent, the engine
    uses its own default key.
    @param failure_blobs
    a list of base64 encoded strings (blobs), where each string encodes the choices 
    made in a failing test run. When the list is nonempty, only the first blob 
    is decoded and run. The blob is only guaranteed to reproduce a failure within 
    a specific version of Hegel *)
let run_test
      ~(settings : settings)
      ?test_location
      ?(from_ppx = false)
      ?database_key
      ?(failure_blobs = [])
      test_fn
  =
  if Stdlib.Domain.DLS.get in_test_context
  then failwith "Cannot nest test cases - already inside a test case";
  (* Default the database key to the test's identity so each [let%hegel_test]
     gets a stable, distinct key: this scopes its persisted corpus and, under
     [derandomize], its per-test seed. An explicit [database_key] wins. *)
  let database_key =
    match database_key with
    | Some _ as k -> k
    | None ->
      Option.map test_location ~f:(fun (loc : Antithesis.test_location) ->
        sprintf "%s:%s" loc.file loc.function_name)
  in
  let ctx = Ffi.context_new () in
  let ffi_settings = build_ffi_settings ctx settings ~database_key in
  let run_body () =
    match failure_blobs with
    | [] -> run_from_engine ctx ~settings ~ffi_settings ~test_fn ~test_location ~from_ppx
    | blob :: _ -> run_from_blob ctx ~settings ~ffi_settings ~test_fn blob
  in
  Exn.protect
    ~finally:(fun () ->
      Ffi.settings_free ctx ffi_settings;
      Ffi.context_free ctx)
    ~f:run_body
;;

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    is {!run_test} with [settings] defaulting to {!default_settings}. This is the
    public entry point the [let%hegel_test] PPX targets and is re-exported as
    [Hegel.run_hegel_test].

    @param database_key
    overrides the key scoping this test's persisted corpus and [derandomize]
    seed. When omitted it defaults to the test's [test_location] (see
    {!run_test}), so each [let%hegel_test] is scoped by its own identity. *)
let run_hegel_test
      ?(settings = default_settings ())
      ?test_location
      ?from_ppx
      ?database_key
      ?failure_blobs
      test_fn
  =
  run_test ~settings ?test_location ?from_ppx ?database_key ?failure_blobs test_fn
;;
