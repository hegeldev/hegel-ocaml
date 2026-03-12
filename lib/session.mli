(** Global session management for the Hegel SDK.

    This module manages a shared hegeld subprocess. It starts lazily on first
    use and cleans up when the process exits.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

type hegel_session = {
  mutable process : int option;
  mutable connection : Connection.connection option;
  mutable client : Client.client option;
  mutable socket_path : string option;
  mutable temp_dir : string option;
  lock : Mutex.t;
}
(** Internal mutable session state. *)

val hegel_version : string
(** The hegel-core commit this SDK is designed to work with. *)

val find_hegeld : unit -> string
(** [find_hegeld ()] locates the hegeld binary. If [HEGEL_SERVER_COMMAND] is
    set, uses that path directly. Otherwise, ensures hegel is installed in
    [.hegel/venv] at the version specified by [hegel_version] and returns the
    path to that binary. *)

val has_working_client : hegel_session -> bool
(** [has_working_client session] returns [true] if the session has a live
    client. *)

val cleanup : hegel_session -> unit
(** [cleanup session] cleans up the session, killing the subprocess and closing
    the connection. *)

val start : hegel_session -> unit
(** [start session] starts hegeld if not already running. *)

val run_hegel_test : ?test_cases:int -> ?seed:int -> (unit -> unit) -> unit
(** [run_hegel_test ?test_cases ?seed test_fn] runs a property test using the
    shared hegeld process. This is the main public API.

    @param test_cases number of test cases (default 100)
    @param seed optional seed for deterministic replay
    @param test_fn the test body function *)

val restart_session : unit -> unit
(** [restart_session ()] forces a restart of the global session. Useful when
    environment variables (like [HEGEL_PROTOCOL_TEST_MODE]) have changed. *)
