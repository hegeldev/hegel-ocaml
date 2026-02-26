(** Global session management for the Hegel SDK.

    This module manages a shared hegeld subprocess. It starts lazily on first
    use and cleans up when the process exits.

    The main entry point is {!run_hegel_test}, which the user calls without
    needing to manage connections or sessions directly. *)

open Connection

(** [find_hegeld ()] locates the hegeld binary. Checks, in order:
    - [HEGEL_BINARY] environment variable
    - [hegel] on [PATH] Returns the path or raises [Failure]. *)
let find_hegeld () =
  match Sys.getenv_opt "HEGEL_BINARY" with
  | Some path when path <> "" -> path
  | _ -> (
      (* Search PATH for hegel binary *)
      let path_str = Option.value ~default:"" (Sys.getenv_opt "PATH") in
      let path_dirs = String.split_on_char ':' path_str in
      let found =
        List.find_map
          (fun dir ->
            let candidate = Filename.concat dir "hegel" in
            if Sys.file_exists candidate then Some candidate else None)
          path_dirs
      in
      match found with
      | Some p -> p
      | None ->
          failwith
            "Cannot find hegel binary. Set HEGEL_BINARY or add hegel to PATH.")

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

(** [run_hegel_test ?test_cases ?name test_fn] runs a property test using the
    shared hegeld process. This is the main public API.

    @param test_cases number of test cases (default 100)
    @param name test name (default ["test"])
    @param test_fn the test body function *)
let run_hegel_test ?(test_cases = 100) ?(name = "test") test_fn =
  start global_session;
  Client.run_test (Option.get global_session.client) ~name ~test_cases test_fn
