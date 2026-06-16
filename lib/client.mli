(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against the native libhegel engine (via {!Hegel_ffi.Ffi}). *)

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
val health_check_to_string : health_check -> string

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

(** Phases of the test that can be enabled or disabled. *)
type phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

(** [phase_to_string p] returns the lowercase name for [p] (the
    [hypothesis.Phase] value name). *)
val phase_to_string : phase -> string

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
    (** [None] uses the engine's default phase list (all phases); [Some xs]
          restricts execution to [xs]. *)
  ; print_blob : bool
  ; report_multiple_failures : bool
  }

(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically: in CI, [derandomize] is [true] and [database] is
    [Disabled]. *)
val default_settings : unit -> settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)
val settings : ?test_cases:int -> ?seed:int -> unit -> settings

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
val is_in_ci : unit -> bool

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

(** [with_suppress_health_check checks s] returns settings [s] with additional
    health checks suppressed. *)
val with_suppress_health_check : health_check list -> settings -> settings

(** [with_phases phases s] returns settings [s] with [phases] set to restrict
    test execution to those phases. *)
val with_phases : phase list -> settings -> settings

(** [with_mode mode s] returns settings [s] with test [mode] set to [mode]. *)
val with_mode : mode -> settings -> settings

(** [with_print_blob b s] returns settings [s] with [print_blob] set to [b]. When
    [true], a failing run prints replay instructions (the failure blob), and
    replay runs report which blobs reproduced the failure. *)
val with_print_blob : bool -> settings -> settings

(** [with_report_multiple_failures b s] returns settings [s] with [report_multiple_failures] 
    set to [b]. When [true], a failing run reports all the failures it found *)
val with_report_multiple_failures : bool -> settings -> settings

(** Per-test-case state passed explicitly to the test function. Holds the
    native test-case handle, the final-replay flag, and abort state. *)
type test_case =
  { handle : Hegel_ffi.Ffi.test_case
  ; mode : mode
  ; stateful_step_count : int
  ; is_final : bool
  ; verbosity : verbosity
  ; mutable test_aborted : bool
  }

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available; derived from the assertion's location so
    the shrinker can group probes for the same bug. *)
val extract_origin : exn -> string

(** [failure_exn ~captured_exn ~panic_message] is the exception to raise for
    one engine-reported failure: the OCaml exception captured for the failure's
    origin when there is one, otherwise a [Failure] carrying the engine's panic
    message. *)
val failure_exn : captured_exn:exn option -> panic_message:string option -> exn

(** [generate_from_schema schema tc] generates a value from a schema by drawing
    from the native engine. Raises {!Data_exhausted} if the engine signals
    StopTest. *)
val generate_from_schema : Cbor.t -> test_case -> Cbor.t

val primitive_boolean : test_case -> float -> bool option -> bool

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
val assume : test_case -> bool -> unit

(** [note tc message] prints [message] to stderr subject to the run's
    {!type:verbosity}: never under [Quiet], only on the final (failing) replay
    under [Normal], and on every test case under [Verbose] or [Debug]. *)
val note : test_case -> string -> unit

(** [target tc value label] records a targeting observation to guide the search
    engine toward higher values. *)
val target : test_case -> float -> string -> unit

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
    (see {!Stateful.Variables}). A pool is a set of integer "variable ids" that
    the engine can sample from. *)

(** [new_pool tc] creates a new engine-managed variable pool and returns its id.
*)
val new_pool : test_case -> int

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
val pool_add : test_case -> pool_id:int -> int

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true] (default [false]), the variable is also removed
    from the pool. Drawing from an empty pool raises {!Data_exhausted}. *)
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

(** [run_test ~settings ?test_location ?database_key test_fn] runs a property
    test using the given settings against the native engine.

    @param test_location
      source location of the test, used by the Antithesis integration.
      Provided automatically by the [let%hegel_test] PPX. When omitted, no
      Antithesis assertion is emitted.
    @param database_key
      optional key scoping persisted/replayed failing examples. *)
val run_test
  :  settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(** [run_hegel_test ?settings ?test_location test_fn] is {!run_test} with
    [settings] defaulting to {!default_settings}. The entry point the
    [let%hegel_test] PPX targets; re-exported as [Hegel.run_hegel_test]. *)
val run_hegel_test
  :  ?settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit
