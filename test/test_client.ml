open Hegel
open Connection
open Client

let contains_substring = Test_helpers.contains_substring
let find_cmd = Test_helpers.find_cmd

(* ---- Unit tests for helpers that don't need a server ---- *)

let test_assume_true () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  assume true;
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2

let test_assume_false_raises () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  let raised = ref false in
  (try assume false with Assume_rejected -> raised := true);
  Alcotest.(check bool) "raised Assume_rejected" true !raised;
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2

let test_get_data_outside_context () =
  let raised = ref false in
  (try ignore (get_data ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Not in a test context'" true
       (contains_substring msg "Not in a test"));
  Alcotest.(check bool) "raised" true !raised

let test_assume_outside_context () =
  let raised = ref false in
  (try assume true
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'outside of a Hegel test'" true
       (contains_substring msg "outside of a Hegel test"));
  Alcotest.(check bool) "raised" true !raised

let test_note_outside_context () =
  let raised = ref false in
  (try note "should fail"
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'outside of a Hegel test'" true
       (contains_substring msg "outside of a Hegel test"));
  Alcotest.(check bool) "raised" true !raised

let test_note_when_not_final () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  note "should not print";
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2

let test_note_when_final () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = true; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  note "test message from note";
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2

let test_extract_origin_no_backtrace () =
  let origin = extract_origin (Failure "test") in
  Alcotest.(check bool) "has Failure" true (contains_substring origin "Failure")

let test_extract_origin_with_backtrace () =
  Printexc.record_backtrace true;
  let origin =
    try raise (Failure "test error") with exn -> extract_origin exn
  in
  Printexc.record_backtrace false;
  Alcotest.(check bool) "has Failure" true (contains_substring origin "Failure");
  Alcotest.(check bool)
    "has file:line" true
    (contains_substring origin "test_client.ml")

let test_start_span_when_aborted () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = true }
  in
  start_span data;
  stop_span data;
  close conn;
  Unix.close s2

(** Test: Domain.DLS initializer for current_data returns None when accessed
    from a fresh domain. This covers the (fun () -> None) initializer that is
    otherwise never triggered in the main domain. *)
let test_dls_initializers_in_new_domain () =
  let result = Domain.spawn (fun () -> Domain.DLS.get current_data = None) in
  let is_none = Domain.join result in
  Alcotest.(check bool) "current_data default None" true is_none

let test_nested_test_raises () =
  (* Set the current_data to simulate being inside a test *)
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  let raised = ref false in
  (try Session.run_hegel_test ~test_cases:1 (fun () -> ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Cannot nest'" true
       (contains_substring msg "Cannot nest"));
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2;
  Alcotest.(check bool) "raised" true !raised

(* ---- Tests using real hegel binary ---- *)

(** Helper: run a test with a specific HEGEL_PROTOCOL_TEST_MODE. Restarts the
    session so the new env var takes effect on the subprocess. *)
let with_test_mode mode f =
  Session.restart_session ();
  Unix.putenv "HEGEL_PROTOCOL_TEST_MODE" mode;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HEGEL_PROTOCOL_TEST_MODE" "";
      Session.restart_session ())
    f

let test_simple_passing_test () =
  Session.run_hegel_test ~test_cases:5 (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      ignore (Cbor_helpers.extract_bool v))

let test_simple_failing_test () =
  let raised = ref false in
  (try
     Session.run_hegel_test ~test_cases:100 (fun () ->
         let data = get_data () in
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
             data
         in
         let x = Cbor_helpers.extract_int v in
         if x >= 50 then failwith "too big")
   with _ -> raised := true);
  Alcotest.(check bool) "test failed" true !raised

let test_single_test_case () =
  Session.run_hegel_test ~test_cases:1 (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      ignore (Cbor_helpers.extract_bool v))

let test_assume_true_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun () -> assume true)

let test_assume_false_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun () -> assume false)

let test_note_not_final_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun () ->
      note "should not appear";
      let data = get_data () in
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 10);
             ])
          data
      in
      ignore (Cbor_helpers.extract_int v))

let test_target_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 100);
             ])
          data
      in
      let x = Cbor_helpers.extract_int v in
      target (float_of_int x) "maximize_x")

(* ---- HEGEL_PROTOCOL_TEST_MODE error injection tests ---- *)

let test_stop_test_on_generate () =
  with_test_mode "stop_test_on_generate" (fun () ->
      Session.run_hegel_test ~test_cases:5 (fun () ->
          let data = get_data () in
          ignore
            (generate_from_schema
               (`Map [ (`Text "type", `Text "boolean") ])
               data)))

let test_stop_test_on_mark_complete () =
  with_test_mode "stop_test_on_mark_complete" (fun () ->
      Session.run_hegel_test ~test_cases:5 (fun () ->
          let data = get_data () in
          ignore
            (generate_from_schema
               (`Map [ (`Text "type", `Text "boolean") ])
               data)))

let test_error_response () =
  with_test_mode "error_response" (fun () ->
      Session.run_hegel_test ~test_cases:5 (fun () ->
          let data = get_data () in
          ignore
            (generate_from_schema
               (`Map [ (`Text "type", `Text "boolean") ])
               data)))

let test_empty_test () =
  with_test_mode "empty_test" (fun () ->
      Session.run_hegel_test ~test_cases:5 (fun () ->
          let data = get_data () in
          ignore
            (generate_from_schema
               (`Map [ (`Text "type", `Text "boolean") ])
               data)))

(* ---- Mark complete status values ---- *)

let test_mark_complete_valid () =
  Session.run_hegel_test ~test_cases:3 (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      ignore (Cbor_helpers.extract_bool v))

let test_mark_complete_invalid () =
  Session.run_hegel_test ~test_cases:5 (fun () ->
      let data = get_data () in
      let _v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      assume false)

let test_mark_complete_interesting () =
  let raised = ref false in
  (try
     Session.run_hegel_test ~test_cases:100 (fun () ->
         let data = get_data () in
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
             data
         in
         let x = Cbor_helpers.extract_int v in
         assert (x < 50))
   with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: start_span and stop_span when NOT aborted (live connection). *)
let test_start_stop_span_live () =
  Session.run_hegel_test ~test_cases:1 (fun () ->
      let data = get_data () in
      start_span data;
      stop_span data;
      ignore
        (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data))

(** Test: version mismatch in create_client (version too high). *)
let test_version_mismatch () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let peer_conn = create_connection server_socket ~name:"Peer" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        (* Receive the handshake and respond with a bad version *)
        peer_conn.connection_state <- Client;
        let ch = control_channel peer_conn in
        let msg_id, _payload = receive_request_raw ch () in
        send_response_raw ch msg_id "Hegel/9.9")
      ()
  in
  let raised = ref false in
  (try ignore (create_client client_conn) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised version mismatch" true !raised;
  close client_conn;
  close peer_conn;
  Thread.join t

(** Test: version mismatch in create_client (version too low). *)
let test_version_mismatch_low () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let peer_conn = create_connection server_socket ~name:"Peer" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        peer_conn.connection_state <- Client;
        let ch = control_channel peer_conn in
        let msg_id, _payload = receive_request_raw ch () in
        send_response_raw ch msg_id "Hegel/0.0")
      ()
  in
  let raised = ref false in
  (try ignore (create_client client_conn) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised version mismatch low" true !raised;
  close client_conn;
  close peer_conn;
  Thread.join t

(** Test: run_test with explicit seed. *)
let test_run_test_with_seed () =
  Session.run_hegel_test ~test_cases:5 ~seed:42 (fun () ->
      let data = get_data () in
      ignore
        (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data))

(** Test: multiple interesting test cases (n_interesting > 1). Uses different
    exception types to produce distinct interesting origins, triggering the
    multi-interesting replay path. *)
let test_multiple_interesting () =
  let raised_msg = ref "" in
  (try
     Session.run_hegel_test ~test_cases:200 (fun () ->
         let v = Hegel.draw (Hegel.Generators.booleans ()) in
         if v then failwith "error from Failure branch" else raise Exit)
   with e -> raised_msg := Printexc.to_string e);
  Alcotest.(check bool)
    "has Multiple failures" true
    (contains_substring !raised_msg "Multiple failures")

(** Test: unrecognised event sends error response and continues (socketpair). *)
let test_unrecognised_event () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_conn = create_connection s1 ~name:"Client" () in
  let peer_conn = create_connection s2 ~name:"Peer" () in
  let t_hs =
    Thread.create
      (fun () ->
        Test_helpers.raw_handshake_responder peer_conn.socket;
        peer_conn.connection_state <- Client)
      ()
  in
  let client = create_client client_conn in
  Thread.join t_hs;
  let t_peer =
    Thread.create
      (fun () ->
        let ctrl = control_channel peer_conn in
        (* Accept run_test command *)
        let msg_id, msg = receive_request ctrl () in
        let pairs = Cbor_helpers.extract_dict msg in
        let test_ch_id =
          Int32.of_int
            (Cbor_helpers.extract_int (List.assoc (`Text "channel_id") pairs))
        in
        send_response_value ctrl msg_id `Null;
        let test_ch = connect_channel peer_conn test_ch_id ~role:"Test" () in
        (* Send unrecognised event *)
        (try
           ignore
             (pending_get
                (request test_ch
                   (`Map [ (`Text "event", `Text "unknown_foobar") ])))
         with Request_error _ -> ());
        (* Send test_done with 0 interesting *)
        ignore
          (pending_get
             (request test_ch
                (`Map
                   [
                     (`Text "event", `Text "test_done");
                     ( `Text "results",
                       `Map [ (`Text "interesting_test_cases", `Int 0) ] );
                   ]))))
      ()
  in
  run_test client ~test_cases:0 (fun () -> ());
  Thread.join t_peer;
  close client_conn;
  close peer_conn

(** Helper: run a function in a temporary directory, restoring cwd and env
    after. *)
let with_temp_install_dir f =
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  let temp_dir = Filename.temp_dir "hegel-test-install-" "" in
  let orig_cwd = Sys.getcwd () in
  Sys.chdir temp_dir;
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      (* Clean up .hegel dir recursively *)
      let hegel_dir = Filename.concat temp_dir ".hegel" in
      let rec rm path =
        if Sys.is_directory path then begin
          Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      (try rm hegel_dir with _ -> ());
      (try Unix.rmdir temp_dir with _ -> ());
      (match orig_cmd with
      | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
      | None -> Unix.putenv "HEGEL_SERVER_COMMAND" "");
      match orig_path with
      | Some v -> Unix.putenv "PATH" v
      | None -> Unix.putenv "PATH" "")
    (fun () -> f temp_dir)

(** Test: find_hegeld fails when uv is not on PATH (install fails at venv
    creation). *)
let test_find_hegeld_install_fails () =
  with_temp_install_dir (fun _temp_dir ->
      Unix.putenv "HEGEL_SERVER_COMMAND" "";
      Unix.putenv "PATH" "/nonexistent";
      let raised = ref false in
      (try ignore (Session.find_hegeld ()) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised)

(** Test: find_hegeld fails when uv pip install fails. Uses a fake uv that
    succeeds for venv but fails for pip. Also covers the mkdir EEXIST path. *)
let test_find_hegeld_pip_install_fails () =
  with_temp_install_dir (fun temp_dir ->
      Unix.putenv "HEGEL_SERVER_COMMAND" "";
      (* Create .hegel dir so mkdir hits EEXIST *)
      let hegel_dir = Filename.concat temp_dir ".hegel" in
      (try Unix.mkdir hegel_dir 0o755 with _ -> ());
      (* Create a fake uv script that succeeds for venv but fails for pip *)
      let bin_dir = Filename.concat temp_dir "bin" in
      Unix.mkdir bin_dir 0o755;
      let uv_script = Filename.concat bin_dir "uv" in
      let oc = open_out uv_script in
      output_string oc
        "#!/bin/sh\n\
         if [ \"$1\" = \"venv\" ]; then mkdir -p \"$3/bin\"; exit 0; fi\n\
         exit 1\n";
      close_out oc;
      Unix.chmod uv_script 0o755;
      Unix.putenv "PATH" bin_dir;
      let raised = ref false in
      let msg = ref "" in
      (try ignore (Session.find_hegeld ())
       with Failure m ->
         raised := true;
         msg := m);
      Alcotest.(check bool) "raised" true !raised;
      Alcotest.(check bool)
        "has 'Failed to install'" true
        (contains_substring !msg "Failed to install"))

(** Test: find_hegeld fails when hegel binary not found after install. Uses a
    fake uv that creates the venv but not the hegel binary. *)
let test_find_hegeld_binary_not_found () =
  with_temp_install_dir (fun temp_dir ->
      Unix.putenv "HEGEL_SERVER_COMMAND" "";
      (* Create a fake uv script that succeeds for both venv and pip *)
      let bin_dir = Filename.concat temp_dir "bin" in
      Unix.mkdir bin_dir 0o755;
      let uv_script = Filename.concat bin_dir "uv" in
      let oc = open_out uv_script in
      output_string oc
        "#!/bin/sh\n\
         if [ \"$1\" = \"venv\" ]; then mkdir -p \"$3/bin\"; touch \
         \"$3/bin/python\"; exit 0; fi\n\
         exit 0\n";
      close_out oc;
      Unix.chmod uv_script 0o755;
      Unix.putenv "PATH" bin_dir;
      let raised = ref false in
      let msg = ref "" in
      (try ignore (Session.find_hegeld ())
       with Failure m ->
         raised := true;
         msg := m);
      Alcotest.(check bool) "raised" true !raised;
      Alcotest.(check bool)
        "has 'not found at'" true
        (contains_substring !msg "not found at"))

(** Test: session start timeout (unreachable hegeld). Also covers the
    Unix.Unix_error retry path by creating a non-socket file at the socket path.
*)
let test_session_start_timeout () =
  let session : Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  (* Set HEGEL_SERVER_COMMAND to a script that creates a regular file (not socket)
     at the socket path, which will cause Unix.Unix_error on connect. *)
  let orig_binary =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  let script_path = Filename.temp_file "hegel-test-" ".sh" in
  let oc = open_out script_path in
  (* Create a regular file (not a socket) at the path and exit immediately.
     This covers both the Unix_error retry (connect to non-socket) and the
     SIGKILL error catch (process already dead). *)
  output_string oc "#!/bin/sh\ntouch \"$1\"\n";
  close_out oc;
  Unix.chmod script_path 0o755;
  Unix.putenv "HEGEL_SERVER_COMMAND" script_path;
  (* Run from a temp directory where .hegel doesn't exist yet, so the mkdir in
     start() creates the directory fresh (covering the non-EEXIST path). *)
  let orig_cwd = Sys.getcwd () in
  let test_dir = Filename.temp_dir "hegel-start-test-" "" in
  Sys.chdir test_dir;
  let raised = ref false in
  (try Session.start session
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Timeout'" true
       (contains_substring msg "Timeout"));
  Alcotest.(check bool) "raised timeout" true !raised;
  Session.cleanup session;
  Sys.chdir orig_cwd;
  (* Clean up the .hegel dir and temp dir *)
  let hegel_dir = Filename.concat test_dir ".hegel" in
  let rec rm path =
    if Sys.is_directory path then begin
      Array.iter (fun f -> rm (Filename.concat path f)) (Sys.readdir path);
      Unix.rmdir path
    end
    else Sys.remove path
  in
  (try rm hegel_dir with _ -> ());
  (try Unix.rmdir test_dir with _ -> ());
  (try Sys.remove script_path with _ -> ());
  (match orig_binary with
  | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
  | None -> Unix.putenv "HEGEL_SERVER_COMMAND" "")

(** Test: run_hegel_test when session is None (unreachable in normal usage, but
    covers the branch). *)
let test_session_not_started () =
  let session : Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  (* Test has_working_client returns false *)
  Alcotest.(check bool)
    "no working client" false
    (Session.has_working_client session)

(** Test: cleanup a session with actual resources. *)
let test_session_cleanup_with_resources () =
  (* Create a temp dir and socket path to clean up *)
  let temp_dir = Filename.temp_dir "hegel-test-" "" in
  let socket_path = Filename.concat temp_dir "test.sock" in
  (* Create the socket file so cleanup can try to remove it *)
  let oc = open_out socket_path in
  close_out oc;
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      socket_path = Some socket_path;
      temp_dir = Some temp_dir;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "conn cleaned" true (session.connection = None);
  Alcotest.(check bool) "socket cleaned" true (session.socket_path = None);
  Alcotest.(check bool) "temp cleaned" true (session.temp_dir = None);
  Unix.close s2

(** Test: cleanup with a connection whose socket is already closed (covers error
    catch in close). *)
let test_session_cleanup_closed_conn () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  (* Close the raw socket fd underneath, so conn.close will error *)
  Unix.close s1;
  Unix.close s2;
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.connection = None)

(** Test: cleanup with nonexistent socket/dir paths (covers error catches). *)
let test_session_cleanup_bad_paths () =
  let session : Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = Some "/nonexistent/path/hegel.sock";
      temp_dir = Some "/nonexistent/path";
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "socket cleaned" true (session.socket_path = None);
  Alcotest.(check bool) "temp cleaned" true (session.temp_dir = None)

(** Test: cleanup a session with a process (uses /bin/sleep for a real PID). *)
let test_session_cleanup_with_process () =
  let sleep_cmd = find_cmd "sleep" in
  let pid =
    Unix.create_process sleep_cmd [| sleep_cmd; "60" |] Unix.stdin Unix.stderr
      Unix.stderr
  in
  let session : Session.hegel_session =
    {
      process = Some pid;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "process cleaned" true (session.process = None)

(** Test: cleanup with already-dead process (covers kill error catch). *)
let test_session_cleanup_dead_process () =
  (* Start a process that exits immediately *)
  let true_cmd = find_cmd "true" in
  let pid =
    Unix.create_process true_cmd [| true_cmd |] Unix.stdin Unix.stderr
      Unix.stderr
  in
  (* Wait for it to finish *)
  ignore (Unix.waitpid [] pid);
  let session : Session.hegel_session =
    {
      process = Some pid;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.process = None)

(** Test: has_working_client with a live connection. *)
let test_has_working_client_live () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client =
        Some
          {
            connection = conn;
            control = control_channel conn;
            lock = Mutex.create ();
          };
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Alcotest.(check bool)
    "has working client" true
    (Session.has_working_client session);
  close conn;
  Unix.close s2

(** Test: run_hegel_test with default parameters. *)
let test_run_hegel_test_defaults () =
  Session.run_hegel_test (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      ignore (Cbor_helpers.extract_bool v))

(** Test: nest test_case raises (when current_data is already set). *)
let test_run_test_case_nest () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  Domain.DLS.set current_data (Some data);
  let raised = ref false in
  (try
     run_test_case
       {
         connection = conn;
         control = control_channel conn;
         lock = Mutex.create ();
       }
       (control_channel conn)
       (fun () -> ())
       ~is_final:false
   with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  Domain.DLS.set current_data None;
  close conn;
  Unix.close s2

(* ---- find_hegeld ---- *)

let test_find_hegeld_via_env () =
  Unix.putenv "HEGEL_SERVER_COMMAND" "/tmp/fake-hegel";
  let path = Session.find_hegeld () in
  Alcotest.(check string) "path" "/tmp/fake-hegel" path;
  Unix.putenv "HEGEL_SERVER_COMMAND" ""

let test_find_hegeld_auto_install () =
  let orig =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  Unix.putenv "HEGEL_SERVER_COMMAND" "";
  let path = Session.find_hegeld () in
  Alcotest.(check bool) "found hegel" true (String.length path > 0);
  Alcotest.(check bool)
    "path contains .hegel/venv" true
    (contains_substring path ".hegel/venv");
  match orig with
  | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
  | None -> Unix.putenv "HEGEL_SERVER_COMMAND" ""

(* ---- Session lifecycle ---- *)

let test_session_cleanup () =
  let session : Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session

let test_session_start_and_run () =
  Session.run_hegel_test ~test_cases:3 (fun () ->
      let data = get_data () in
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) data
      in
      ignore (Cbor_helpers.extract_bool v))

let tests =
  [
    (* Unit tests *)
    Alcotest.test_case "assume true" `Quick test_assume_true;
    Alcotest.test_case "assume false raises" `Quick test_assume_false_raises;
    Alcotest.test_case "get_data outside context" `Quick
      test_get_data_outside_context;
    Alcotest.test_case "assume outside context" `Quick
      test_assume_outside_context;
    Alcotest.test_case "note outside context" `Quick test_note_outside_context;
    Alcotest.test_case "note when not final" `Quick test_note_when_not_final;
    Alcotest.test_case "note when final" `Quick test_note_when_final;
    Alcotest.test_case "extract_origin no backtrace" `Quick
      test_extract_origin_no_backtrace;
    Alcotest.test_case "extract_origin with backtrace" `Quick
      test_extract_origin_with_backtrace;
    Alcotest.test_case "start/stop span when aborted" `Quick
      test_start_span_when_aborted;
    Alcotest.test_case "DLS initializers in new domain" `Quick
      test_dls_initializers_in_new_domain;
    Alcotest.test_case "nested test raises" `Quick test_nested_test_raises;
    (* Real-server converted tests *)
    Alcotest.test_case "start/stop span live" `Quick test_start_stop_span_live;
    Alcotest.test_case "version mismatch" `Quick test_version_mismatch;
    Alcotest.test_case "version mismatch low" `Quick test_version_mismatch_low;
    Alcotest.test_case "run_test with seed" `Quick test_run_test_with_seed;
    Alcotest.test_case "multiple interesting" `Quick test_multiple_interesting;
    Alcotest.test_case "unrecognised event" `Quick test_unrecognised_event;
    (* find_hegeld *)
    Alcotest.test_case "find_hegeld via env" `Quick test_find_hegeld_via_env;
    Alcotest.test_case "find_hegeld auto install" `Quick
      test_find_hegeld_auto_install;
    Alcotest.test_case "find_hegeld install fails" `Quick
      test_find_hegeld_install_fails;
    Alcotest.test_case "find_hegeld pip install fails" `Quick
      test_find_hegeld_pip_install_fails;
    Alcotest.test_case "find_hegeld binary not found" `Quick
      test_find_hegeld_binary_not_found;
    (* Session *)
    Alcotest.test_case "session cleanup" `Quick test_session_cleanup;
    Alcotest.test_case "session cleanup with resources" `Quick
      test_session_cleanup_with_resources;
    Alcotest.test_case "session cleanup closed conn" `Quick
      test_session_cleanup_closed_conn;
    Alcotest.test_case "session cleanup bad paths" `Quick
      test_session_cleanup_bad_paths;
    Alcotest.test_case "session cleanup with process" `Quick
      test_session_cleanup_with_process;
    Alcotest.test_case "session cleanup dead process" `Quick
      test_session_cleanup_dead_process;
    Alcotest.test_case "has working client live" `Quick
      test_has_working_client_live;
    Alcotest.test_case "session not started" `Quick test_session_not_started;
    Alcotest.test_case "session start timeout" `Quick test_session_start_timeout;
    Alcotest.test_case "run_test_case nest" `Quick test_run_test_case_nest;
    (* E2E tests with real hegel *)
    Alcotest.test_case "simple passing test" `Quick test_simple_passing_test;
    Alcotest.test_case "simple failing test" `Quick test_simple_failing_test;
    Alcotest.test_case "single test case" `Quick test_single_test_case;
    Alcotest.test_case "assume true e2e" `Quick test_assume_true_e2e;
    Alcotest.test_case "assume false e2e" `Quick test_assume_false_e2e;
    Alcotest.test_case "note not final e2e" `Quick test_note_not_final_e2e;
    Alcotest.test_case "target e2e" `Quick test_target_e2e;
    Alcotest.test_case "mark_complete VALID" `Quick test_mark_complete_valid;
    Alcotest.test_case "mark_complete INVALID" `Quick test_mark_complete_invalid;
    Alcotest.test_case "mark_complete INTERESTING" `Quick
      test_mark_complete_interesting;
    Alcotest.test_case "session start and run" `Quick test_session_start_and_run;
    Alcotest.test_case "run_hegel_test defaults" `Quick
      test_run_hegel_test_defaults;
    (* HEGEL_PROTOCOL_TEST_MODE error injection - these restart the session *)
    Alcotest.test_case "stop test on generate" `Quick test_stop_test_on_generate;
    Alcotest.test_case "stop test on mark complete" `Quick
      test_stop_test_on_mark_complete;
    Alcotest.test_case "error response" `Quick test_error_response;
    Alcotest.test_case "empty test" `Quick test_empty_test;
  ]
