(** Global session management for Hegel.

    This module manages a shared hegel subprocess communicating over stdio
    pipes. It starts lazily on first use and cleans up when the process exits.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

val hegel_server_version : string
(** Version of hegel-core to install. *)

val hegel_server_command_env : string
(** Environment variable to override the hegel server command. *)

val hegel_server_dir : string
(** Directory for hegel server installation and logs. *)

val uv_not_found_message : string
(** Message shown when uv is not found. *)

val run_command_to_log : string -> string list -> string -> Unix.process_status
(** [run_command_to_log cmd args log_path] runs [cmd] with [args], redirecting
    stdout and stderr to [log_path]. Returns the process exit status. *)

val ensure_hegel_installed : unit -> string
(** [ensure_hegel_installed ()] checks for a cached hegel installation and
    installs via uv if needed. Returns the path to the hegel binary. *)

val find_on_path : string -> string option
(** [find_on_path cmd] searches [PATH] for [cmd] and returns [Some path] if
    found, or [None]. *)

val find_hegel : unit -> string
(** [find_hegel ()] locates the hegel binary. Checks, in order:
    - [HEGEL_SERVER_COMMAND] environment variable
    - [HEGEL_BINARY] environment variable (for backward compatibility)
    - [hegel] on [PATH]
    - Auto-install via uv to [.hegel/venv/] *)

type hegel_session = {
  mutable process : int option;
  mutable connection : Connection.connection option;
  mutable client : Client.client option;
  lock : Mutex.t;
}
(** Internal mutable session state. *)

val has_working_client : hegel_session -> bool
(** [has_working_client session] returns [true] if the session has a live
    client. *)

val cleanup : hegel_session -> unit
(** [cleanup session] cleans up the session, killing the subprocess and closing
    the connection. *)

val start : hegel_session -> unit
(** [start session] starts the hegel server if not already running. Spawns the
    server with [--stdio] for pipe-based communication. *)

val run_hegel_test :
  ?settings:Client.settings ->
  ?test_cases:int ->
  ?seed:int ->
  (Client.test_case -> unit) ->
  unit
(** [run_hegel_test ?settings ?test_cases ?seed test_fn] runs a property test
    using the shared hegel process. If [settings] is provided, [test_cases] and
    [seed] are ignored. *)

val restart_session : unit -> unit
(** [restart_session ()] forces a restart of the global session. Useful when
    environment variables (like [HEGEL_PROTOCOL_TEST_MODE]) have changed. *)
