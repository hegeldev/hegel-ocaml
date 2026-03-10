(** Global session management for the Hegel SDK.

    This module manages a shared hegeld subprocess. It starts lazily on first
    use and cleans up when the process exits.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

open Connection

(** The hegel-core commit this SDK is designed to work with. *)
let hegel_version = "v0.3.3"

(** Environment variable name for overriding the hegel binary path. *)
let hegel_cmd_env = "HEGEL_CMD"

(** Directory for managed hegel installation. *)
let hegel_dir = ".hegel"

(** Path to the managed virtualenv. *)
let venv_dir = Filename.concat hegel_dir "venv"

(** Path to the version tracking file. *)
let version_file = Filename.concat venv_dir "hegel-version"

(** [hegel_pip_spec ()] returns the pip install spec for the current
    [hegel_version]. *)
let hegel_pip_spec () =
  Printf.sprintf
    "hegel @ git+ssh://git@github.com/antithesishq/hegel-core.git@%s"
    hegel_version

(** [run_command args] runs a command, returning [true] on success (exit code 0)
    and [false] on any failure (non-zero exit, signal, or missing command). *)
let run_command args =
  try
    let argv = Array.of_list args in
    let pid =
      Unix.create_process (List.hd args) argv Unix.stdin Unix.stderr Unix.stderr
    in
    let _, status = Unix.waitpid [] pid in
    status = Unix.WEXITED 0
  with Unix.Unix_error _ -> false

(** [read_file_contents path] reads a file's contents, returning [None] if the
    file does not exist. *)
let read_file_contents path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> Some (String.trim (In_channel.input_all ic)))
  with Sys_error _ -> None

(** [write_file_contents path contents] writes [contents] to [path]. *)
let write_file_contents path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

(** [ensure_hegel_installed ()] ensures hegel is installed in [.hegel/venv] and
    returns the path to the binary. Creates the venv and installs hegel if it
    doesn't exist, or reinstalls if the version file doesn't match
    [hegel_version]. *)
let ensure_hegel_installed () =
  let hegel_bin = Filename.concat (Filename.concat venv_dir "bin") "hegel" in
  (* Check if already installed at the right version *)
  (match read_file_contents version_file with
  | Some v when v = hegel_version && Sys.file_exists hegel_bin -> ()
  | _ ->
      (* Need to install *)
      (try Unix.mkdir hegel_dir 0o755
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      Printf.eprintf "Installing hegel (%s) into %s...\n%!" hegel_version
        venv_dir;
      if not (run_command [ "uv"; "venv"; "--clear"; venv_dir ]) then
        failwith "Failed to create venv. Is uv installed?";
      let python_bin =
        Filename.concat (Filename.concat venv_dir "bin") "python"
      in
      if
        not
          (run_command
             [
               "uv"; "pip"; "install"; "--python"; python_bin; hegel_pip_spec ();
             ])
      then
        failwith
          (Printf.sprintf
             "Failed to install hegel (version: %s). Set %s to a hegel binary \
              path to skip installation."
             hegel_version hegel_cmd_env);
      if not (Sys.file_exists hegel_bin) then
        failwith
          (Printf.sprintf "hegel not found at %s after installation" hegel_bin);
      write_file_contents version_file hegel_version);
  hegel_bin

(** [find_hegeld ()] locates the hegeld binary. If [HEGEL_CMD] is set, uses that
    path directly (the user is responsible for providing the right binary).
    Otherwise, ensures hegel is installed in [.hegel/venv] at the version
    specified by [hegel_version] and returns the path to that binary. *)
let find_hegeld () =
  match Sys.getenv_opt hegel_cmd_env with
  | Some path when path <> "" -> path
  | _ -> ensure_hegel_installed ()

type hegel_session = {
  mutable process : int option;
  mutable connection : connection option;
  mutable client : Client.client option;
  mutable socket_path : string option;
  mutable temp_dir : string option;
  lock : Mutex.t;
}
(** Internal mutable session state. *)

(** The global session singleton. *)
let global_session =
  {
    process = None;
    connection = None;
    client = None;
    socket_path = None;
    temp_dir = None;
    lock = Mutex.create ();
  }

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
      (try close conn with _ -> ());
      session.connection <- None;
      session.client <- None
  | None -> ());
  (match session.process with
  | Some pid ->
      (try Unix.kill pid Sys.sigterm with _ -> ());
      (try ignore (Unix.waitpid [] pid) with _ -> ());
      session.process <- None
  | None -> ());
  (match session.socket_path with
  | Some path ->
      (try Sys.remove path with _ -> ());
      session.socket_path <- None
  | None -> ());
  match session.temp_dir with
  | Some dir ->
      (try Unix.rmdir dir with _ -> ());
      session.temp_dir <- None
  | None -> ()

(** [start session] starts hegeld if not already running. *)
let start session =
  if has_working_client session then ()
  else begin
    Mutex.lock session.lock;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock session.lock)
      (fun () ->
        if not (has_working_client session) then begin
          (* Clean up any old session *)
          cleanup session;
          let hegel_cmd = find_hegeld () in
          let temp_dir = Filename.temp_dir "hegel-" "" in
          session.temp_dir <- Some temp_dir;
          let socket_path = Filename.concat temp_dir "hegel.sock" in
          session.socket_path <- Some socket_path;
          (* Start hegeld subprocess *)
          let pid =
            Unix.create_process hegel_cmd
              [| hegel_cmd; socket_path |]
              Unix.stdin Unix.stderr Unix.stderr
          in
          session.process <- Some pid;
          (* Wait for socket to appear and connect *)
          let rec try_connect attempts =
            if attempts >= 50 then begin
              Unix.kill pid Sys.sigkill;
              failwith "Timeout waiting for hegeld to start"
            end
            else if Sys.file_exists socket_path then (
              try
                let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
                Unix.connect s (Unix.ADDR_UNIX socket_path);
                s
              with Unix.Unix_error _ ->
                Unix.sleepf 0.1;
                try_connect (attempts + 1))
            else begin
              Unix.sleepf 0.1;
              try_connect (attempts + 1)
            end
          in
          let sock = try_connect 0 in
          let conn = create_connection sock ~name:"SDK" () in
          session.connection <- Some conn;
          let c = Client.create_client conn in
          session.client <- Some c;
          at_exit (fun () -> cleanup session)
        end)
  end

(** [restart_session ()] forces a restart of the global session. Useful when
    environment variables (like [HEGEL_PROTOCOL_TEST_MODE]) have changed. *)
let restart_session () = cleanup global_session

(** [run_hegel_test ?test_cases ?seed test_fn] runs a property test using the
    shared hegeld process. This is the main public API.

    @param test_cases number of test cases (default 100)
    @param seed optional seed for deterministic replay
    @param test_fn the test body function *)
let run_hegel_test ?(test_cases = 100) ?seed test_fn =
  start global_session;
  Client.run_test (Option.get global_session.client) ~test_cases ?seed test_fn
