(** Test runner and lifecycle management for the Hegel SDK.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. *)

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

type test_case_data = {
  channel : Connection.channel;
  is_final : bool;
  mutable test_aborted : bool;
}
(** Per-test-case data, consolidating channel, final-run flag, and abort state.
    Domain-local so each domain has its own independent test state. *)

val current_data : test_case_data option Domain.DLS.key
(** Current test case data. Domain-local so each domain has its own independent
    test state. *)

val get_data : unit -> test_case_data
(** [get_data ()] returns the current test case data, raising [Failure] if not
    in a test context. *)

val extract_origin : exn -> string
(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)

val generate_from_schema : CBOR.Simple.t -> test_case_data -> CBOR.Simple.t
(** [generate_from_schema schema data] generates a value from a schema by
    sending a generate command to the server. Raises {!Data_exhausted} if the
    server signals StopTest. *)

val assume : bool -> unit
(** [assume condition] rejects the current test case if [condition] is [false].
*)

val note : string -> unit
(** [note message] records a message that will be printed on the final (failing)
    run. *)

val target : float -> string -> unit
(** [target value label] sends a target command to guide the search engine
    toward higher values. *)

val start_span : ?label:int -> test_case_data -> unit
(** [start_span ?label data] starts a generation span for better shrinking. *)

val stop_span : ?discard:bool -> test_case_data -> unit
(** [stop_span ?discard data] ends the current generation span. *)

type client = {
  connection : Connection.connection;
  control : Connection.channel;
  lock : Mutex.t;
}
(** Hegel client for running property-based tests. *)

val create_client : Connection.connection -> client
(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)

val run_test_case :
  client -> Connection.channel -> (unit -> unit) -> is_final:bool -> unit
(** [run_test_case client channel test_fn ~is_final] runs a single test case.
    Sets up domain-local state and calls [test_fn]. Reports status via
    mark_complete. *)

val run_test : client -> test_cases:int -> ?seed:int -> (unit -> unit) -> unit
(** [run_test client ~test_cases ?seed test_fn] runs a property test.

    @param seed
      optional seed for deterministic replay. If [None], the server generates
      its own seed. *)
