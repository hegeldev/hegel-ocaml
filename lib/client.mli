(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. *)

module Mutex = Caml_threads.Mutex

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

(** Health checks that can be suppressed during test execution. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

val health_check_to_string : health_check -> string
(** [health_check_to_string hc] returns the wire protocol name for [hc]. *)

(** Controls how much output Hegel produces during test runs. *)
type verbosity = Quiet | Normal | Verbose | Debug

(** The database setting: unset, disabled, or a path. *)
type database = Unset | Disabled | Path of string

type settings = {
  test_cases : int;
  verbosity : verbosity;
  seed : int option;
  derandomize : bool;
  database : database;
  suppress_health_check : health_check list;
}
(** Configuration for a Hegel test run. *)

val default_settings : unit -> settings
(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically: in CI, [derandomize] is [true] and [database] is
    [Disabled]. *)

val settings : ?test_cases:int -> ?seed:int -> unit -> settings
(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)

val is_in_ci : unit -> bool
(** [is_in_ci ()] returns [true] if a CI environment is detected. *)

val with_test_cases : int -> settings -> settings
(** [with_test_cases n s] returns settings [s] with [test_cases] set to [n]. *)

val with_verbosity : verbosity -> settings -> settings
(** [with_verbosity v s] returns settings [s] with [verbosity] set to [v]. *)

val with_seed : int option -> settings -> settings
(** [with_seed seed s] returns settings [s] with [seed] set. *)

val with_derandomize : bool -> settings -> settings
(** [with_derandomize b s] returns settings [s] with [derandomize] set to [b].
*)

val with_database : database -> settings -> settings
(** [with_database db s] returns settings [s] with [database] set to [db]. *)

val with_suppress_health_check : health_check list -> settings -> settings
(** [with_suppress_health_check checks s] returns settings [s] with additional
    health checks suppressed. *)

type test_case = {
  stream : Connection.stream;
  is_final : bool;
  mutable test_aborted : bool;
}
(** Per-test-case state passed explicitly to the test function. Holds the data
    stream, final-run flag, and abort state. *)

val extract_origin : exn -> string
(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)

val generate_from_schema : CBOR.Simple.t -> test_case -> CBOR.Simple.t
(** [generate_from_schema schema tc] generates a value from a schema by sending
    a generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest. *)

val assume : test_case -> bool -> unit
(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)

val note : test_case -> string -> unit
(** [note tc message] records a message that will be printed on the final
    (failing) run. *)

val target : test_case -> float -> string -> unit
(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)

val start_span : ?label:int -> test_case -> unit
(** [start_span ?label tc] starts a generation span for better shrinking. *)

val stop_span : ?discard:bool -> test_case -> unit
(** [stop_span ?discard tc] ends the current generation span. *)

val supported_protocol_lo : string
(** Lowest supported protocol version. *)

val supported_protocol_hi : string
(** Highest supported protocol version. *)

type client = {
  connection : Connection.connection;
  control : Connection.stream;
  lock : Mutex.t;
}
(** Hegel client for running property-based tests. *)

val create_client : Connection.connection -> client
(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)

val run_test_case :
  client -> Connection.stream -> (test_case -> unit) -> is_final:bool -> unit
(** [run_test_case client stream test_fn ~is_final] runs a single test case.
    Passes the test case to [test_fn]. Reports status via mark_complete. *)

val run_test :
  client ->
  settings:settings ->
  ?database_key:string ->
  (test_case -> unit) ->
  unit
(** [run_test client ~settings ?database_key test_fn] runs a property test using
    the given settings. *)
