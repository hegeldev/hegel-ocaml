(** This module implements the client-side logic for running property-based
    tests against the native libhegel engine (via {!Hegel_ffi.Ffi}).

    Examples in this documentation assume [open Hegel]. *)

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the engine runs out of choice budget for the current test case
    (StopTest). *)
exception Data_exhausted

(** Raised when the engine detects a flaky strategy definition or when 
the client side pool diverges from the engine side pool. *)
exception Flaky_strategy

(** Health checks that can be suppressed during test execution.

    @canonical Hegel.health_check *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(**/**)

(** [health_check_to_string hc] returns the canonical name for [hc]. *)
val health_check_to_string : health_check -> string

(**/**)

(** Controls how much output Hegel produces during test runs.

    @canonical Hegel.verbosity *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** The database setting: unset, disabled, or a path.

    @canonical Hegel.database *)
type database =
  | Unset
  | Disabled
  | Path of string

(** Controls the test execution mode.

    @canonical Hegel.mode *)
type mode =
  | Test_run
  (** Run a full property test: many test cases, shrinking, database
        replay, all other phases. This is the default. *)
  | Single_test_case
  (** Run the test body exactly once, with no shrinking, replay, or
        database. Useful when you want pure data generation without
        property-testing overhead. *)

(** Phases of the test that can be enabled or disabled.

    @canonical Hegel.phase *)
type phase =
  | Explicit
  (** Reserved for future use: hegel-ocaml has no explicit-examples facility
        yet, so selecting this phase currently has no effect. *)
  | Reuse
  | Generate
  | Target
  | Shrink

(**/**)

(** [phase_to_string p] returns the lowercase name for [p] (the
    [Phase] value name). *)
val phase_to_string : phase -> string

(**/**)

(** Configuration for a Hegel test run.

    {[
      let%hegel_test example tc =
        ignore tc
      [@@settings
        Internal.default_settings ()
        |> Internal.with_test_cases 500
        |> Internal.with_verbosity Internal.Verbose
        |> Internal.with_database (Internal.Path "_hegel_db")]
      ;;
    ]}

    @canonical Hegel.settings *)
type settings =
  { mode : mode
  ; test_cases : int
  ; stateful_step_count : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
    (** stores previous failures. when set, Hegel replays test cases from previously
  failed runs and adds new failures when they occur. *)
  ; suppress_health_check : health_check list
  ; phases : phase list option
    (** [None] uses the engine's default phase list (all phases); [Some xs]
          restricts execution to [xs]. *)
  ; print_blob : bool
    (** print a [rerun with:] line for a failure;
        [true] by default *)
  ; report_multiple_failures : bool (** false by default *)
  }

(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically: in CI, [derandomize] is [true] and [database] is
    [Disabled]. *)
val default_settings : unit -> settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases.

    {[
      let s = Internal.settings ~test_cases:500 ~seed:42 ()
    ]} *)
val settings : ?test_cases:int -> ?seed:int -> unit -> settings

(**/**)

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
val is_in_ci : unit -> bool

(**/**)

(** [with_test_cases n s] returns settings [s] with [test_cases] set to [n]. *)
val with_test_cases : int -> settings -> settings

(** [with_stateful_step_count n s] returns settings [s] with [stateful_step_count] set to [n]. *)
val with_stateful_step_count : int -> settings -> settings

(** [with_verbosity v s] returns settings [s] with [verbosity] set to [v]. *)
val with_verbosity : verbosity -> settings -> settings

(** [with_seed seed s] returns settings [s] with [seed] set. *)
val with_seed : int option -> settings -> settings

(** [with_derandomize b s] returns settings [s] with [derandomize] set to [b].
*)
val with_derandomize : bool -> settings -> settings

(** [with_database db s] returns settings [s] with [database] set to [db]. *)
val with_database : database -> settings -> settings

(** [with_suppress_health_check checks s] returns settings [s] with
    [suppress_health_check] set to [checks], replacing any previously suppressed
    list (like the other [with_*] builders). *)
val with_suppress_health_check : health_check list -> settings -> settings

(** [with_phases phases s] returns settings [s] with [phases] set to restrict
    test execution to those phases.

    {[
      let s = Internal.with_phases [ Internal.Generate; Internal.Shrink ] s
    ]} *)
val with_phases : phase list -> settings -> settings

(** [with_mode mode s] returns settings [s] with test [mode] set to [mode]. *)
val with_mode : mode -> settings -> settings

(** [with_print_blob b s] returns settings [s] with [print_blob] set to [b]. When
    [true] (the default), a failing run's report ends with a copy-pasteable
    [rerun with:] line encoding the failure. *)
val with_print_blob : bool -> settings -> settings

(** [with_report_multiple_failures b s] returns settings [s] with [report_multiple_failures] 
    set to [b]. When [true], a failing run reports all the failures it found *)
val with_report_multiple_failures : bool -> settings -> settings

(** An opaque per-test-case handle, threaded to the test function and to the
    drawing primitives. Created and owned by the run loop.

    @canonical Hegel.test_case *)
type test_case

(**/**)

val mode : test_case -> mode
val stateful_step_count : test_case -> int
val is_high_verbosity : test_case -> bool
val draw_depth : test_case -> int
val incr_draw_depth : test_case -> unit
val decr_draw_depth : test_case -> unit
val set_test_aborted : test_case -> bool -> unit

(** [with_note_indent tc f] runs [f], nesting every {!note}/draw line it prints
    one level deeper (restoring the depth even if [f] raises). Used to indent the
    draws a stateful step makes under its [Step N] header. *)
val with_note_indent : test_case -> (unit -> 'a) -> 'a

(** [clone tc] forks a fresh clone of [tc] on an independent choice stream (its
    own native handle and context), freed by a GC finaliser once unreachable.
    Re-exported as [Hegel.clone]. *)
val clone : test_case -> test_case

(** A running worker spawned by {!spawn}; joined with {!join}. Re-exported as
    [Hegel.worker]. *)
type 'a worker

(** [spawn tc f] runs [f] on a fresh clone of [tc] on a new thread, capturing its
    result or exception. Re-exported as [Hegel.spawn]. *)
val spawn : test_case -> (test_case -> 'a) -> 'a worker

(** [join w] waits for [w] and returns its result — re-raising any exception the
    worker raised. Re-exported as [Hegel.join]. *)
val join : 'a worker -> 'a

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available; derived from the assertion's location so
    the shrinker can group probes for the same bug. *)
val extract_origin : exn -> string

(** [generate_boolean tc p forced] draws a boolean with probability [p] of
    [true]. If [forced] is [Some b] the value is forced to [b]. Raises
    {!Data_exhausted} on StopTest. *)
val generate_boolean : test_case -> float -> bool option -> bool

(** [generate_integer tc ~min_value ~max_value] draws an integer in the inclusive
    range. Raises {!Data_exhausted} on StopTest. *)
val generate_integer : test_case -> min_value:int -> max_value:int -> int

(** [generate_float tc ...] draws a width-64 float under the given NaN / infinity
    / exclusion policy. Raises {!Data_exhausted} on StopTest. *)
val generate_float
  :  test_case
  -> min_value:float
  -> max_value:float
  -> allow_nan:bool
  -> allow_infinity:bool
  -> exclude_min:bool
  -> exclude_max:bool
  -> smallest_nonzero_magnitude:float
  -> float

(** [generate_bytes tc ~min_size ~max_size] draws a byte string ([max_size = None]
    means unbounded). Raises {!Data_exhausted} on StopTest. *)
val generate_bytes : test_case -> min_size:int -> max_size:int option -> string

(** [generate_text tc ...] draws a text string over the described alphabet.
    Raises {!Data_exhausted} on StopTest. *)
val generate_text
  :  test_case
  -> min_size:int
  -> max_size:int option
  -> codec:string option
  -> min_codepoint:int
  -> max_codepoint:int
  -> categories:string list option
  -> exclude_categories:string list option
  -> include_characters:string option
  -> exclude_characters:string option
  -> string

(** [generate_regex tc ~pattern ~fullmatch] draws a string matching [pattern]
    (Python-[re] syntax). Raises {!Data_exhausted} on StopTest. *)
val generate_regex : test_case -> pattern:string -> fullmatch:bool -> string

(** [generate_email tc] draws an RFC 5321/5322 email address. Raises
    {!Assume_rejected} when the draw rejects itself. *)
val generate_email : test_case -> string

(** [generate_url tc] draws an RFC 3986 http/https URL. *)
val generate_url : test_case -> string

(** [generate_domain tc ~max_length] draws an RFC 1035 domain name. *)
val generate_domain : test_case -> max_length:int -> string

(** [generate_date tc] draws a Gregorian date as [(year, month, day)]. *)
val generate_date : test_case -> int * int * int

(** [generate_time tc] draws a time as [(hour, minute, second, microsecond)]. *)
val generate_time : test_case -> int * int * int * int

(** [generate_datetime tc] draws a naive datetime as [(date, time)]. *)
val generate_datetime : test_case -> (int * int * int) * (int * int * int * int)

(** [generate_ipv4 tc] draws an IPv4 address as its 4 network-order bytes. *)
val generate_ipv4 : test_case -> string

(** [generate_ipv6 tc] draws an IPv6 address as its 16 network-order bytes. *)
val generate_ipv6 : test_case -> string

(**/**)

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. The [tc] handle is accepted for API symmetry with the other
    per-test-case primitives; the rejection is client-side (raising
    {!Assume_rejected}) and does not consult [tc]. *)
val assume : test_case -> bool -> unit

(** [note tc message] prints [message] to stderr subject to the run's
    {!type:verbosity}: never under [Quiet], only on the final (failing) replay
    under [Normal], and on every test case under [Verbose] or [Debug]. *)
val note : test_case -> string -> unit

(**/**)

(** [color_enabled ~override ~isatty] decides whether ANSI colors are on: an
    [override] of ["1"]/["0"] (the [HEGEL_COLOR] variable) forces it on/off;
    otherwise follow [isatty]. *)
val color_enabled : override:string option -> isatty:bool -> bool

(** [stderr_color_enabled ()] is {!color_enabled} for the failure report's
    stream, reading the environment and stderr's tty state afresh. *)
val stderr_color_enabled : unit -> bool

(** [stderr_color code s] wraps [s] in the ANSI SGR [code] when colors are
    enabled for stderr, else returns [s] unchanged. *)
val stderr_color : string -> string -> string

(** [render_diff ~colored ~original ~updated] renders a structural sexp diff of
    the two values: red/green markings when [colored], [-]/[+] otherwise. *)
val render_diff : colored:bool -> original:Core.Sexp.t -> updated:Core.Sexp.t -> string

(**/**)

(** [require tc ?msg condition] fails the current test case when [condition] is
    [false] by raising [Failure msg]. Unlike [assert] the failure message is
    yours to choose, and unlike {!assume} the case counts as a genuine failure
    rather than being discarded. *)
val require : test_case -> ?msg:string -> bool -> unit

(** [require_equal tc ?msg sexp_of lhs rhs] fails the current test case when
    the two values render to different sexps under [sexp_of], printing a
    structural sexp diff of the two values in the failure report's body before
    raising [Failure msg]. *)
val require_equal : test_case -> ?msg:string -> ('a -> Core.Sexp.t) -> 'a -> 'a -> unit

(**/**)

(** [draw_display_name tc ~label ~repeatable] returns the display name to print
    for a drawn value, bumping the per-test-case occurrence counter for [label].
    A [repeatable] name is numbered on every occurrence ([label_1], [label_2],
    …), while a non-repeatable name is printed bare. *)
val draw_display_name : test_case -> label:string -> repeatable:bool -> string

(**/**)

(** [target tc value label] records a targeting observation to guide the search
    engine toward higher values. *)
val target : test_case -> float -> string -> unit

(**/**)

(** [start_span ?label tc] starts a generation span for better shrinking. *)
val start_span : ?label:int -> test_case -> unit

(** [stop_span ?discard tc] ends the current generation span. *)
val stop_span : ?discard:bool -> test_case -> unit

(** {2 Engine-managed collections}

    Collections let the engine choose the length of a variable-length sequence
    while the caller draws elements one at a time. *)

(** [new_collection tc ~min_size ~max_size] starts a collection and returns its
    id ([max_size = None] means unbounded). *)
val new_collection : test_case -> min_size:int -> max_size:int option -> int

(** [collection_more tc ~collection_id] returns whether the engine wants another
    element. *)
val collection_more : test_case -> collection_id:int -> bool

(** [collection_reject tc ~collection_id] rejects the collection's last element.
*)
val collection_reject : test_case -> collection_id:int -> unit

(** {2 Variable pools}

    Pools are the engine-side primitive backing variables in stateful testing
    (see {!Stateful.Pool}). A pool is a set of integer "variable ids" that
    the engine can sample from. *)

(** [new_pool tc] creates a new engine-managed variable pool and returns its id.
*)
val new_pool : test_case -> int

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
val pool_add : test_case -> pool_id:int -> int

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true] (default [false]), the variable is also removed
    from the pool. Drawing from an empty pool raises {!Assume_rejected} (the
    engine rejects the test case as invalid). *)
val pool_generate : test_case -> pool_id:int -> ?consume:bool -> unit -> int

(** [new_state_machine tc ~rule_names ~invariant_names] registers an
    engine-owned state machine with the named rules and invariants and returns
    its id. The engine owns rule selection, including swarm testing (each test
    case enables a random subset of rules). *)
val new_state_machine
  :  test_case
  -> rule_names:string list
  -> invariant_names:string list
  -> int

(** [state_machine_next_rule tc ~state_machine_id] draws the index (in
    [\[0, num_rules)]) of the next rule to run, letting the engine choose and
    shrink the rule sequence. Raises {!Data_exhausted} when the engine's choice
    budget is exhausted. *)
val state_machine_next_rule : test_case -> state_machine_id:int -> int

(**/**)

(** [run_test ~settings ?test_location ?database_key ?failure_blobs test_fn] runs
    a property test using the given settings against the native engine.

    @param test_location
    source location of the test, used by the Antithesis integration.
    Provided automatically by the [let%hegel_test] PPX. When omitted, no
    Antithesis assertion is emitted.
    @param database_key
    optional key scoping persisted/replayed failing examples and, under [derandomize],
    the per-test seed. Defaults to the test's [test_location] (as
    [file:function_name]) so each [let%hegel_test] gets a stable, distinct
    key; pass an explicit key to override. When both are absent, the engine
    uses its own default key.
    @param from_ppx
    [true] when the run is driven by the [let%hegel_test] PPX; only set by the
    PPX. Selects the [[@@failure_blobs [...]]] attribute form of the [rerun with:]
    hint vs. the [~failure_blobs] argument form a plain caller would use.
    @param failure_blobs
    a list of base64 encoded strings (blobs), where each string encodes the choices
    made in a failing test run. When the list is nonempty, only the first blob
    is decoded and run. The blob is only guaranteed to reproduce a failure within
    a specific version of Hegel *)
val run_test
  :  settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?from_ppx:bool
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(**/**)

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, with [settings] defaulting to
    {!default_settings}. This is the entry point the [let%hegel_test] PPX targets;
    re-exported as [Hegel.run_hegel_test].

    @param database_key
    overrides the per-test database key / [derandomize] seed. Defaults to the
    test's [test_location] so each [let%hegel_test] is scoped by its own
    identity. *)
val run_hegel_test
  :  ?settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?from_ppx:bool
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit
