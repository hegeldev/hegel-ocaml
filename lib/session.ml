(** Global session management for Hegel.

    This module manages a shared hegel subprocess communicating over stdio
    pipes. It starts lazily on first use and cleans up when the process exits.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

open! Core
module Unix = Core_unix
module Mutex = Caml_threads.Mutex
module Thread = Caml_threads.Thread
open Connection

(** Version of hegel-core to install. *)
let hegel_server_version = "0.2.3"

(** Environment variable to override the hegel server command. *)
let hegel_server_command_env = "HEGEL_SERVER_COMMAND"

(** Directory for hegel server installation and logs. *)
let hegel_server_dir = ".hegel"

(** Message shown when uv is not found. *)
let uv_not_found_message =
  "You are seeing this error message because hegel-ocaml tried to use `uv` to \
   install hegel-core, but could not find uv on the PATH.\n\n\
   Hegel uses a Python server component called `hegel-core` to share core \
   property-based testing functionality across languages. There are two ways \
   for Hegel to get hegel-core:\n\n\
   * By default, Hegel looks for uv (https://docs.astral.sh/uv/) on the PATH, \
   and uses uv to install hegel-core to a local `.hegel/venv` directory. We \
   recommend this option. To continue, install uv: \
   https://docs.astral.sh/uv/getting-started/installation/.\n\
   * Alternatively, you can manage the installation of hegel-core yourself. \
   After installing, setting the HEGEL_SERVER_COMMAND environment variable to \
   your hegel-core binary path tells hegel-ocaml to use that hegel-core \
   instead.\n\n\
   See https://hegel.dev/reference/installation for more details."

(** [run_command_to_log cmd args log_path] runs a command and returns its exit
    status. Output is redirected to the install log file. *)
let run_command_to_log cmd args log_path =
  let log_fd =
    Unix.openfile log_path
      ~mode:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
      ~perm:0o644
  in
  Exn.protect
    ~finally:(fun () -> Unix.close log_fd)
    ~f:(fun () ->
      let pid =
        Caml_unix.create_process cmd
          (Array.of_list (cmd :: args))
          Caml_unix.stdin log_fd log_fd
      in
      let _, status = Caml_unix.waitpid [] pid in
      status)

(** [ensure_hegel_installed ()] checks for a cached hegel installation and
    installs via uv if needed. Returns the path to the hegel binary. *)
let ensure_hegel_installed () =
  let venv_dir = sprintf "%s/venv" hegel_server_dir in
  let version_file = sprintf "%s/hegel-version" venv_dir in
  let hegel_bin = sprintf "%s/bin/hegel" venv_dir in
  let install_log = sprintf "%s/install.log" hegel_server_dir in
  (* Check cached version *)
  try
    let cached = String.strip (In_channel.read_all version_file) in
    if
      String.equal cached hegel_server_version
      && Stdlib.Sys.file_exists hegel_bin
    then hegel_bin
    else raise Exit
  with _ ->
    (* Need to install *)
    (try Unix.mkdir hegel_server_dir ~perm:0o755 with Unix.Unix_error _ -> ());
    (* Create venv *)
    let status =
      try run_command_to_log "uv" [ "venv"; "--clear"; venv_dir ] install_log
      with Unix.Unix_error (Unix.ENOENT, _, _) ->
        failwith uv_not_found_message
    in
    (match status with
    | Caml_unix.WEXITED 0 -> ()
    | _ ->
        let log = try In_channel.read_all install_log with _ -> "" in
        failwith (sprintf "uv venv failed. Install log:\n%s" log));
    (* Install hegel-core *)
    let python_path = sprintf "%s/bin/python" venv_dir in
    let status =
      run_command_to_log "uv"
        [
          "pip";
          "install";
          "--python";
          python_path;
          sprintf "hegel-core==%s" hegel_server_version;
        ]
        install_log
    in
    (match status with
    | Caml_unix.WEXITED 0 -> ()
    | _ ->
        let log = try In_channel.read_all install_log with _ -> "" in
        failwith
          (sprintf
             "Failed to install hegel-core (version: %s). Set %s to a hegel \
              binary path to skip installation.\n\
              Install log:\n\
              %s"
             hegel_server_version hegel_server_command_env log));
    if not (Stdlib.Sys.file_exists hegel_bin) then
      failwith (sprintf "hegel not found at %s after installation" hegel_bin);
    (* Write version file *)
    Out_channel.write_all version_file ~data:hegel_server_version;
    hegel_bin

(** [find_hegel ()] locates the hegel binary. Checks, in order:
    - [HEGEL_SERVER_COMMAND] environment variable
    - Auto-install via uv to [.hegel/venv/] *)
let find_hegel () =
  match Sys.getenv hegel_server_command_env with
  | Some path when not (String.is_empty path) -> path
  | _ -> ensure_hegel_installed ()

(** [server_log_fd ()] returns a file descriptor for the server log file. *)
let server_log_fd () =
  (try Unix.mkdir hegel_server_dir ~perm:0o755 with Unix.Unix_error _ -> ());
  Unix.openfile
    (sprintf "%s/server.log" hegel_server_dir)
    ~mode:[ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
    ~perm:0o644

type hegel_session = {
  mutable process : int option;
  mutable connection : connection option;
  mutable client : Client.client option;
  lock : Mutex.t;
}
(** Internal mutable session state. *)

(** The global session singleton. *)
let global_session =
  { process = None; connection = None; client = None; lock = Mutex.create () }

(** [has_working_client session] returns [true] if the session has a live
    client. *)
let has_working_client session =
  match (session.client, session.connection) with
  | Some _, Some conn -> is_live conn
  | _ -> false

(** [cleanup session] cleans up the session, killing the subprocess and closing
    the connection. *)
let cleanup session =
  (match session.connection with
  | Some conn ->
      close conn;
      session.connection <- None;
      session.client <- None
  | None -> ());
  match session.process with
  | Some pid ->
      (try Caml_unix.kill pid Stdlib.Sys.sigterm with _ -> ());
      (try ignore (Caml_unix.waitpid [] pid) with _ -> ());
      session.process <- None
  | None -> ()

(** [start session] starts the hegel server if not already running. Spawns the
    server with [--stdio] for pipe-based communication. *)
let start session =
  if has_working_client session then ()
  else begin
    Mutex.lock session.lock;
    Exn.protect
      ~finally:(fun () -> Mutex.unlock session.lock)
      ~f:(fun () ->
        if not (has_working_client session) then begin
          (* Clean up any old session *)
          cleanup session;
          let hegel_cmd = find_hegel () in
          (* Create pipes for stdio communication *)
          let child_stdin_read, child_stdin_write = Unix.pipe () in
          let child_stdout_read, child_stdout_write = Unix.pipe () in
          let log_fd = server_log_fd () in
          (* Start hegel subprocess with --stdio *)
          let pid =
            Caml_unix.create_process hegel_cmd
              [| hegel_cmd; "--stdio"; "--verbosity"; "normal" |]
              child_stdin_read child_stdout_write log_fd
          in
          (* Close the child's ends of the pipes *)
          Unix.close child_stdin_read;
          Unix.close child_stdout_write;
          Unix.close log_fd;
          session.process <- Some pid;
          let conn =
            create_connection ~read_fd:child_stdout_read
              ~write_fd:child_stdin_write ~name:"Client" ()
          in
          session.connection <- Some conn;
          let c = Client.create_client conn in
          session.client <- Some c;
          (* Monitor thread: detect server crash *)
          ignore
            (Thread.create
               (fun () ->
                 ignore (Caml_unix.waitpid [] pid);
                 match session.connection with
                 | Some conn -> conn.server_exited <- true
                 | None -> ())
               ());
          Stdlib.at_exit (fun () -> cleanup session)
        end)
  end

(** [restart_session ()] forces a restart of the global session. Useful when
    environment variables (like [HEGEL_PROTOCOL_TEST_MODE]) have changed. *)
let restart_session () = cleanup global_session

(** [run_hegel_test ?settings test_fn] runs a property test using the shared
    hegel process. Uses {!Client.default_settings} when [settings] is not
    provided. *)
let run_hegel_test ?(settings = Client.default_settings ()) test_fn =
  start global_session;
  Client.run_test (Option.value_exn global_session.client) ~settings test_fn
