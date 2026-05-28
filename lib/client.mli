(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. *)

module Mutex = Caml_threads.Mutex

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the server runs out of test data (StopTest). *)
exception Data_exhausted

(** Raised when the server detects a flaky strategy definition. *)
exception Flaky_strategy

(** Health checks that can be suppressed during test execution. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(** [health_check_to_string hc] returns the wire protocol name for [hc]. *)
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

(** [phase_to_string p] returns the wire protocol name for [p] (the lowercase
    [hypothesis.Phase] value name). *)
val phase_to_string : phase -> string

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
    (** [None] uses the server's default phase list (all phases); [Some xs]
          restricts execution to [xs]. *)
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

(** Per-test-case state passed explicitly to the test function. Holds the data
    stream, final-run flag, and abort state. *)
type test_case =
  { stream : Connection.stream
  ; mode : mode
  ; is_final : bool
  ; mutable test_aborted : bool
  }

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)
val extract_origin : exn -> string

(** [generate_from_schema schema tc] generates a value from a schema by sending
    a generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest or {!Flaky_strategy} if it signals FlakyStrategyDefinition.
*)
val generate_from_schema : Cbor.t -> test_case -> Cbor.t

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
val assume : test_case -> bool -> unit

(** [note tc message] records a message that will be printed on the final
    (failing) run. *)
val note : test_case -> string -> unit

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
val target : test_case -> float -> string -> unit

(** [start_span ?label tc] starts a generation span for better shrinking. *)
val start_span : ?label:int -> test_case -> unit

(** [stop_span ?discard tc] ends the current generation span. *)
val stop_span : ?discard:bool -> test_case -> unit

(** {2 Variable pools}

    Pools are the server-side primitive backing variables in stateful testing
    (see {!Stateful.Variables}). A pool is a list of integer "variable ids" that
    the engine can sample from *)

(** [new_pool tc] creates a new server-side variable pool and returns its id. *)
val new_pool : test_case -> int

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
val pool_add : test_case -> pool_id:int -> int

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true] (default [false]), the variable is also removed
    from the pool server-side. Drawing from an empty pool raises
    {!Data_exhausted}. *)
val pool_generate : test_case -> pool_id:int -> ?consume:bool -> unit -> int

(** Lowest supported protocol version. *)
val supported_protocol_lo : string

(** Highest supported protocol version. *)
val supported_protocol_hi : string

(** Hegel client for running property-based tests. *)
type client =
  { connection : Connection.connection
  ; control : Connection.stream
  ; lock : Mutex.t
  }

(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)
val create_client : Connection.connection -> client

(** [run_test client ~settings ?test_location ?database_key ?failure_blob
      ?record_failure_blobs test_fn] runs a property test using the given
    settings.

    @param test_location
      source location of the test, used by the Antithesis integration.
      Provided automatically by the [let%hegel_test] PPX. When omitted, no
      Antithesis assertion is emitted.
    @param failure_blob
      when [Some b], replays the recorded failure encoded in [b] instead
      of running normal exploration. Only valid with [Test_run] mode.
    @param record_failure_blobs
      when [true] (default [false]), prints any server-reported
      [failure_blobs] to stdout after a failing test so the user can
      copy-paste them into a [[@@failure_blobs [...]]] attribute on the test for
      future replay. *)
val run_test
  :  client
  -> settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?database_key:string
  -> ?failure_blob:string
  -> ?record_failure_blobs:bool
  -> (test_case -> unit)
  -> unit
