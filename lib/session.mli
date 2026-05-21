(** Global session management for Hegel.

    This module manages a shared hegel subprocess communicating over stdio
    pipes. It starts lazily on first use and lives for the process lifetime (the
    OS cleans up on exit).

    When [HEGEL_PROTOCOL_TEST_MODE] is set, a disposable session is created per
    test so the test server gets a fresh subprocess with the right env var.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

module Mutex = Caml_threads.Mutex

(** Version of hegel-core to install. *)
val hegel_server_version : string

(** Environment variable to override the hegel server command. *)
val hegel_server_command_env : string

(** Directory for hegel server installation and logs. *)
val hegel_server_dir : string

(** Message shown when uv is not found. *)
val uv_not_found_message : string

(** [run_command_to_log cmd args log_path] runs [cmd] with [args], redirecting
    stdout and stderr to [log_path]. Returns the process exit status. *)
val run_command_to_log : string -> string list -> string -> Caml_unix.process_status

(** [ensure_hegel_installed ()] checks for a cached hegel installation and
    installs via uv if needed. Returns the path to the hegel binary. *)
val ensure_hegel_installed : unit -> string

(** [find_hegel ()] locates the hegel binary. Checks, in order:
    - [HEGEL_SERVER_COMMAND] environment variable
    - Auto-install via uv to [.hegel/venv/] *)
val find_hegel : unit -> string

(** Internal mutable session state. *)
type hegel_session =
  { mutable process : int option
  ; mutable connection : Connection.connection option
  ; mutable client : Client.client option
  ; lock : Mutex.t
  }

(** [has_working_client session] returns [true] if the session has a live
    client. *)
val has_working_client : hegel_session -> bool

(** [cleanup session] cleans up the session, killing the subprocess and closing
    the connection. *)
val cleanup : hegel_session -> unit

(** [start session] starts the hegel server if not already running. The server
    communicates over its own stdin/stdout, which we wire up to a pair of pipes.
*)
val start : hegel_session -> unit

(** [run_hegel_test ?settings test_location test_fn] runs a property test
    using the shared hegel process. When [HEGEL_PROTOCOL_TEST_MODE] is set,
    creates a disposable session so the test server gets a fresh subprocess
    with the right env var. Uses {!Client.default_settings} when [settings]
    is not provided.
    
    @param test_location
      forwarded to {!Client.run_test} for the Antithesis integration. 
      Supplied automatically by the [let%hegel_test] PPX. *)
val run_hegel_test
  :  ?settings:Client.settings
  -> Antithesis.test_location
  -> (Client.test_case -> unit)
  -> unit
