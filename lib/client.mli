(** Test runner and lifecycle management for the Hegel SDK.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. *)

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

val current_channel : Connection.channel option Domain.DLS.key
(** Current test case channel. Domain-local so each domain has its own
    independent test state. *)

val is_final_run : bool Domain.DLS.key
(** Whether this is the final (replay) run for a failing test case. *)

val test_aborted : bool Domain.DLS.key
(** Whether the server sent StopTest during this test case. Used by
    {!Generators} to short-circuit collection protocol. *)

val in_test : bool Domain.DLS.key
(** Whether we are currently inside a test case body. *)

val extract_origin : exn -> string
(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)

val get_channel : unit -> Connection.channel
(** [get_channel ()] returns the current test data channel, raising [Failure] if
    not in a test context. *)

val generate_from_schema : CBOR.Simple.t -> CBOR.Simple.t
(** [generate_from_schema schema] generates a value from a schema by sending a
    generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest. *)

val assume : bool -> unit
(** [assume condition] rejects the current test case if [condition] is [false].
*)

val note : string -> unit
(** [note message] records a message that will be printed on the final (failing)
    run. *)

val target : float -> string -> unit
(** [target value label] sends a target command to guide the search engine
    toward higher values. *)

val start_span : ?label:int -> unit -> unit
(** [start_span ?label ()] starts a generation span for better shrinking. *)

val stop_span : ?discard:bool -> unit -> unit
(** [stop_span ?discard ()] ends the current generation span. *)

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

val run_test : client -> name:string -> test_cases:int -> (unit -> unit) -> unit
(** [run_test client ~name ~test_cases test_fn] runs a property test. *)
