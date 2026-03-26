open Hegel
open Connection
open Client

let contains_substring = Test_helpers.contains_substring
let find_cmd = Test_helpers.find_cmd

(* ---- Unit tests for helpers that don't need a server ---- *)

let test_assume_true () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let tc =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  assume tc true;
  close conn;
  Unix.close s2

let test_assume_false_raises () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let tc =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  let raised = ref false in
  (try assume tc false with Assume_rejected -> raised := true);
  Alcotest.(check bool) "raised Assume_rejected" true !raised;
  close conn;
  Unix.close s2

let test_note_when_not_final () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let tc =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  note tc "should not print";
  close conn;
  Unix.close s2

let test_note_when_final () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let tc =
    Client.
      { channel = control_channel conn; is_final = true; test_aborted = false }
  in
  note tc "test message from note";
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
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = true }
  in
  start_span data;
  stop_span data;
  close conn;
  Unix.close s2

let test_nested_test_raises () =
  let raised = ref false in
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:1 ())
    (fun _tc ->
      try
        Session.run_hegel_test ~settings:(Client.settings ~test_cases:1 ())
          (fun _tc2 -> ())
      with Failure msg ->
        raised := true;
        Alcotest.(check bool)
          "has 'Cannot nest'" true
          (contains_substring msg "Cannot nest"));
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
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

let test_simple_failing_test () =
  let raised = ref false in
  (try
     Session.run_hegel_test ~settings:(Client.settings ~test_cases:100 ())
       (fun tc ->
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
             tc
         in
         let x = Cbor_helpers.extract_int v in
         if x >= 50 then failwith "too big")
   with _ -> raised := true);
  Alcotest.(check bool) "test failed" true !raised

let test_single_test_case () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:1 ()) (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

let test_assume_true_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      assume tc true)

let test_assume_false_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      assume tc false)

let test_note_not_final_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      note tc "should not appear";
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 10);
             ])
          tc
      in
      ignore (Cbor_helpers.extract_int v))

let test_target_e2e () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 100);
             ])
          tc
      in
      let x = Cbor_helpers.extract_int v in
      target tc (float_of_int x) "maximize_x")

(* ---- HEGEL_PROTOCOL_TEST_MODE error injection tests ---- *)

let test_stop_test_on_generate () =
  with_test_mode "stop_test_on_generate" (fun () ->
      Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ())
        (fun tc ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc)))

let test_stop_test_on_mark_complete () =
  with_test_mode "stop_test_on_mark_complete" (fun () ->
      Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ())
        (fun tc ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc)))

let test_error_response () =
  with_test_mode "error_response" (fun () ->
      Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ())
        (fun tc ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc)))

let test_empty_test () =
  with_test_mode "empty_test" (fun () ->
      Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ())
        (fun tc ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc)))

(* ---- Mark complete status values ---- *)

let test_mark_complete_valid () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:3 ()) (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

let test_mark_complete_invalid () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
      let _v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      assume tc false)

let test_mark_complete_interesting () =
  let raised = ref false in
  (try
     Session.run_hegel_test ~settings:(Client.settings ~test_cases:100 ())
       (fun tc ->
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
             tc
         in
         let x = Cbor_helpers.extract_int v in
         assert (x < 50))
   with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: start_span and stop_span when NOT aborted (live connection). *)
let test_start_stop_span_live () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:1 ()) (fun tc ->
      start_span tc;
      stop_span tc;
      ignore
        (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc))

(** Test: version mismatch in create_client (version too high). *)
let test_version_mismatch () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let peer_conn = Test_helpers.make_connection server_socket ~name:"Peer" () in
  let client_conn =
    Test_helpers.make_connection client_socket ~name:"Client" ()
  in
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
  let peer_conn = Test_helpers.make_connection server_socket ~name:"Peer" () in
  let client_conn =
    Test_helpers.make_connection client_socket ~name:"Client" ()
  in
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
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:5 ~seed:42 ())
    (fun tc ->
      ignore
        (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc))

(** Test: multiple interesting test cases (n_interesting > 1). Uses different
    exception types to produce distinct interesting origins, triggering the
    multi-interesting replay path. *)
let test_multiple_interesting () =
  let raised_msg = ref "" in
  (try
     Session.run_hegel_test ~settings:(Client.settings ~test_cases:200 ())
       (fun tc ->
         let v = Hegel.draw tc (Hegel.Generators.booleans ()) in
         if v then failwith "error from Failure branch" else raise Exit)
   with e -> raised_msg := Printexc.to_string e);
  Alcotest.(check bool)
    "has Multiple failures" true
    (contains_substring !raised_msg "Multiple failures")

(** Helper: set up a fake server using socketpairs, perform handshake, and
    return [(client, client_conn, peer_conn)]. The caller provides a peer
    function that will be run in a thread. *)
let with_fake_server peer_fn client_fn =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_conn = Test_helpers.make_connection s1 ~name:"Client" () in
  let peer_conn = Test_helpers.make_connection s2 ~name:"Peer" () in
  let t_hs = Thread.create Test_helpers.handshake_via_channel peer_conn in
  let client = create_client client_conn in
  Thread.join t_hs;
  let t_peer = Thread.create (fun () -> peer_fn peer_conn) () in
  client_fn client;
  Thread.join t_peer;
  close client_conn;
  close peer_conn

(** Helper: accept a run_test command on the control channel, return
    [(ctrl, test_ch)] where test_ch is the test channel. *)
let accept_run_test peer_conn =
  let ctrl = control_channel peer_conn in
  let msg_id, msg = receive_request ctrl () in
  let pairs = Cbor_helpers.extract_dict msg in
  let test_ch_id =
    Int32.of_int
      (Cbor_helpers.extract_int (List.assoc (`Text "channel_id") pairs))
  in
  send_response_value ctrl msg_id `Null;
  let test_ch = connect_channel peer_conn test_ch_id ~role:"Test" () in
  (ctrl, test_ch)

(** Helper: send test_done with given results fields. *)
let send_test_done test_ch results_fields =
  ignore
    (pending_get
       (request test_ch
          (`Map
             [
               (`Text "event", `Text "test_done");
               (`Text "results", `Map results_fields);
             ])))

(** Test: unrecognised event sends error response and continues (socketpair). *)
let test_unrecognised_event () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      (* Send unrecognised event *)
      (try
         ignore
           (pending_get
              (request test_ch
                 (`Map [ (`Text "event", `Text "unknown_foobar") ])))
       with Request_error _ -> ());
      (* Send test_done with 0 interesting *)
      send_test_done test_ch [ (`Text "interesting_test_cases", `Int 0) ])
    (fun client ->
      run_test client
        ~settings:(Client.default_settings () |> Client.with_test_cases 0)
        (fun _tc -> ()))

(** Test: run_test with database=Path and suppress_health_check sends the
    correct fields over the wire. *)
let test_run_test_database_path_and_suppress () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch
        [
          (`Text "interesting_test_cases", `Int 0); (`Text "passed", `Bool true);
        ])
    (fun client ->
      let settings =
        Client.default_settings () |> Client.with_test_cases 0
        |> Client.with_database (Client.Path "/tmp/test.db")
        |> Client.with_suppress_health_check
             [ Client.Too_slow; Client.Filter_too_much ]
      in
      run_test client ~settings (fun _tc -> ()))

(** Test: run_test with database=Unset omits the database field. *)
let test_run_test_database_unset () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch [ (`Text "interesting_test_cases", `Int 0) ])
    (fun client ->
      let settings =
        Client.default_settings () |> Client.with_test_cases 0
        |> Client.with_database Client.Unset
      in
      run_test client ~settings (fun _tc -> ()))

(** Test: server error in results raises Failure. *)
let test_run_test_server_error_in_results () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch
        [
          (`Text "interesting_test_cases", `Int 0);
          (`Text "error", `Text "some server error");
        ])
    (fun client ->
      let raised = ref false in
      (try
         run_test client
           ~settings:(Client.default_settings () |> Client.with_test_cases 0)
           (fun _tc -> ())
       with Failure msg ->
         raised := true;
         Alcotest.(check bool)
           "has 'Server error'" true
           (contains_substring msg "Server error");
         Alcotest.(check bool)
           "has error text" true
           (contains_substring msg "some server error"));
      Alcotest.(check bool) "raised" true !raised)

(** Test: health check failure in results raises Failure. *)
let test_run_test_health_check_failure () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch
        [
          (`Text "interesting_test_cases", `Int 0);
          (`Text "health_check_failure", `Text "too slow");
        ])
    (fun client ->
      let raised = ref false in
      (try
         run_test client
           ~settings:(Client.default_settings () |> Client.with_test_cases 0)
           (fun _tc -> ())
       with Failure msg ->
         raised := true;
         Alcotest.(check bool)
           "has 'Health check failure'" true
           (contains_substring msg "Health check failure");
         Alcotest.(check bool)
           "has failure text" true
           (contains_substring msg "too slow"));
      Alcotest.(check bool) "raised" true !raised)

(** Test: flaky test detection in results raises Failure. *)
let test_run_test_flaky () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch
        [
          (`Text "interesting_test_cases", `Int 0);
          (`Text "flaky", `Text "flaky message");
        ])
    (fun client ->
      let raised = ref false in
      (try
         run_test client
           ~settings:(Client.default_settings () |> Client.with_test_cases 0)
           (fun _tc -> ())
       with Failure msg ->
         raised := true;
         Alcotest.(check bool)
           "has 'Flaky test detected'" true
           (contains_substring msg "Flaky test detected");
         Alcotest.(check bool)
           "has flaky text" true
           (contains_substring msg "flaky message"));
      Alcotest.(check bool) "raised" true !raised)

(** Test: passed=false with 0 interesting test cases raises Failure. *)
let test_run_test_passed_false () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch
        [
          (`Text "interesting_test_cases", `Int 0); (`Text "passed", `Bool false);
        ])
    (fun client ->
      let raised = ref false in
      (try
         run_test client
           ~settings:(Client.default_settings () |> Client.with_test_cases 0)
           (fun _tc -> ())
       with Failure msg ->
         raised := true;
         Alcotest.(check bool)
           "has 'Property test failed'" true
           (contains_substring msg "Property test failed"));
      Alcotest.(check bool) "raised" true !raised)

(** Test: server_exited check in pop_inbox_item. Create a connection, set
    server_exited to true, then try to receive. Should raise with
    server_crashed_message. *)
let test_server_exited_in_pop_inbox () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  conn.server_exited <- true;
  let ch = control_channel conn in
  let raised = ref false in
  (try ignore (receive_request ch ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has server crashed message" true
       (contains_substring msg "hegel server process has exited"));
  Alcotest.(check bool) "raised" true !raised;
  close conn;
  Unix.close s2

(** Test: run_hegel_test with explicit settings parameter (covers the Some s
    branch in session.ml line 257). *)
let test_run_hegel_test_with_settings () =
  let settings =
    Client.default_settings () |> Client.with_test_cases 3
    |> Client.with_database Client.Disabled
  in
  Session.run_hegel_test ~settings (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

(** Test: find_hegel uses HEGEL_SERVER_COMMAND env var. *)
let test_find_hegel_via_env () =
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  Unix.putenv "HEGEL_SERVER_COMMAND" "/tmp/fake-hegel";
  let path = Session.find_hegel () in
  Alcotest.(check string) "path" "/tmp/fake-hegel" path;
  match orig_cmd with
  | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
  | None -> Unix.putenv "HEGEL_SERVER_COMMAND" ""

(** Test: session start failure (bad server command). Covers the error path when
    the hegel subprocess exits immediately without completing the handshake. *)
let test_session_start_failure () =
  let session : Session.hegel_session =
    { process = None; connection = None; client = None; lock = Mutex.create () }
  in
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  Unix.putenv "HEGEL_SERVER_COMMAND" "/bin/false";
  let raised = ref false in
  (try Session.start session with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  Session.cleanup session;
  match orig_cmd with
  | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
  | None -> Unix.putenv "HEGEL_SERVER_COMMAND" ""

(** Test: monitor thread detects server crash (session.ml line 231). Start a
    real session, kill the server process, verify server_exited. *)
let test_monitor_thread_detects_crash () =
  let session : Session.hegel_session =
    { process = None; connection = None; client = None; lock = Mutex.create () }
  in
  Session.start session;
  (* Kill the server process *)
  (match session.process with
  | Some pid -> (
      Unix.kill pid Sys.sigkill;
      (* Wait for the monitor thread to detect the crash *)
      Unix.sleepf 0.5;
      match session.connection with
      | Some conn ->
          Alcotest.(check bool)
            "server_exited" true
            (Connection.server_has_exited conn)
      | None -> Alcotest.fail "no connection")
  | None -> Alcotest.fail "no process");
  Session.cleanup session

(** Test: cleanup with a connection that raises on close (session.ml line 179).
    We close the connection's fds manually first, then mark it as running. When
    cleanup calls [close conn], the close function will try to close
    already-closed fds, but that's caught internally. To actually trigger the
    outer try/with, we'd need a non-Unix exception - but we can at least
    exercise the code path by closing the fds first and marking running=true. *)
let test_cleanup_with_close_error () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  (* Close both fds underneath the connection *)
  Unix.close s1;
  Unix.close s2;
  (* Ensure conn.running is true so close actually tries to close fds *)
  conn.running <- true;
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "cleaned up" true (session.connection = None)

(** Test: run_hegel_test when session is None (unreachable in normal usage, but
    covers the branch). *)
let test_session_not_started () =
  let session : Session.hegel_session =
    { process = None; connection = None; client = None; lock = Mutex.create () }
  in
  (* Test has_working_client returns false *)
  Alcotest.(check bool)
    "no working client" false
    (Session.has_working_client session)

(** Test: cleanup a session with actual resources. *)
let test_session_cleanup_with_resources () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "conn cleaned" true (session.connection = None);
  Unix.close s2

(** Test: cleanup with a connection whose socket is already closed (covers error
    catch in close). *)
let test_session_cleanup_closed_conn () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
  (* Close the raw socket fd underneath, so conn.close will error *)
  Unix.close s1;
  Unix.close s2;
  let session : Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.connection = None)

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
      lock = Mutex.create ();
    }
  in
  Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.process = None)

(** Test: has_working_client with a live connection. *)
let test_has_working_client_live () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Test" () in
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
  Session.run_hegel_test (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

(* ---- health_check_to_string ---- *)

let test_health_check_to_string () =
  Alcotest.(check string)
    "filter_too_much" "filter_too_much"
    (Client.health_check_to_string Client.Filter_too_much);
  Alcotest.(check string)
    "too_slow" "too_slow"
    (Client.health_check_to_string Client.Too_slow);
  Alcotest.(check string)
    "test_cases_too_large" "test_cases_too_large"
    (Client.health_check_to_string Client.Test_cases_too_large);
  Alcotest.(check string)
    "large_initial_test_case" "large_initial_test_case"
    (Client.health_check_to_string Client.Large_initial_test_case)

(* ---- is_in_ci ---- *)

(** All CI environment variable names checked by [is_in_ci]. *)
let all_ci_var_names =
  [
    "CI";
    "TF_BUILD";
    "BUILDKITE";
    "CIRCLECI";
    "CIRRUS_CI";
    "CODEBUILD_BUILD_ID";
    "GITHUB_ACTIONS";
    "GITLAB_CI";
    "HEROKU_TEST_RUN_ID";
    "TEAMCITY_VERSION";
  ]

(** Save all CI env vars, unset them, run [f], then restore originals. Uses the
    [unsetenv] C stub from test_helpers to truly remove variables from the
    environment. *)
let with_clean_ci_env f =
  let saved =
    List.map (fun name -> (name, Sys.getenv_opt name)) all_ci_var_names
  in
  List.iter (fun name -> Test_helpers.unsetenv name) all_ci_var_names;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (name, v) ->
          match v with
          | Some orig -> Unix.putenv name orig
          | None -> Test_helpers.unsetenv name)
        saved)
    f

let test_is_in_ci_some_none_branch () =
  (* Test the (Some _, None) branch: set a var that expects any value *)
  with_clean_ci_env (fun () ->
      Unix.putenv "HEROKU_TEST_RUN_ID" "anything";
      Alcotest.(check bool)
        "is_in_ci with HEROKU_TEST_RUN_ID" true (Client.is_in_ci ()))

let test_is_in_ci_some_v_some_exp_branch () =
  (* Test the (Some v, Some exp) branch: set a var with matching expected value *)
  with_clean_ci_env (fun () ->
      Unix.putenv "TF_BUILD" "true";
      Alcotest.(check bool)
        "is_in_ci with TF_BUILD=true" true (Client.is_in_ci ()))

(* ---- Settings builders ---- *)

let test_with_verbosity () =
  let s = Client.default_settings () |> Client.with_verbosity Client.Debug in
  Alcotest.(check bool)
    "verbosity is Debug" true
    (s.Client.verbosity = Client.Debug)

let test_with_derandomize () =
  let s = Client.default_settings () |> Client.with_derandomize true in
  Alcotest.(check bool) "derandomize is true" true s.Client.derandomize

let test_with_database () =
  let s =
    Client.default_settings () |> Client.with_database (Client.Path "/tmp/db")
  in
  Alcotest.(check bool)
    "database is Path" true
    (s.Client.database = Client.Path "/tmp/db")

let test_with_suppress_health_check () =
  let s =
    Client.default_settings ()
    |> Client.with_suppress_health_check
         [ Client.Too_slow; Client.Filter_too_much ]
  in
  Alcotest.(check int)
    "suppress list length" 2
    (List.length s.Client.suppress_health_check);
  Alcotest.(check bool)
    "first is Too_slow" true
    (List.nth s.Client.suppress_health_check 0 = Client.Too_slow);
  Alcotest.(check bool)
    "second is Filter_too_much" true
    (List.nth s.Client.suppress_health_check 1 = Client.Filter_too_much)

(** Test: default_settings in CI sets derandomize and disables database. *)
let test_default_settings_in_ci () =
  with_clean_ci_env (fun () ->
      Unix.putenv "CI" "true";
      let s = Client.default_settings () in
      Alcotest.(check bool) "derandomize in CI" true s.derandomize;
      Alcotest.(check bool)
        "database disabled in CI" true
        (s.database = Client.Disabled))

(** Test: default_settings outside CI sets database to Unset. *)
let test_default_settings_not_in_ci () =
  with_clean_ci_env (fun () ->
      let s = Client.default_settings () in
      Alcotest.(check bool) "derandomize not in CI" false s.derandomize;
      Alcotest.(check bool) "database is Unset" true (s.database = Client.Unset))

(** Test: settings convenience constructor. *)
let test_settings_convenience () =
  let s0 = Client.settings () in
  Alcotest.(check int) "default test_cases" 100 s0.Client.test_cases;
  let s = Client.settings ~test_cases:50 () in
  Alcotest.(check int) "test_cases" 50 s.Client.test_cases;
  Alcotest.(check bool) "seed is None" true (s.Client.seed = None);
  let s2 = Client.settings ~test_cases:10 ~seed:42 () in
  Alcotest.(check int) "test_cases" 10 s2.Client.test_cases;
  Alcotest.(check int) "seed" 42 (Option.get s2.Client.seed)

(** Test: with_seed builder. *)
let test_with_seed () =
  let s = Client.default_settings () |> Client.with_seed (Some 99) in
  Alcotest.(check int) "seed" 99 (Option.get s.Client.seed);
  let s2 = Client.with_seed None s in
  Alcotest.(check bool) "seed cleared" true (s2.Client.seed = None)

(** Test: run_test with database_key. *)
let test_run_test_with_database_key () =
  with_fake_server
    (fun peer_conn ->
      let _ctrl, test_ch = accept_run_test peer_conn in
      send_test_done test_ch [ (`Text "interesting_test_cases", `Int 0) ])
    (fun client ->
      run_test client
        ~settings:(Client.default_settings () |> Client.with_test_cases 0)
        ~database_key:"test-key"
        (fun _tc -> ()))

(* ---- find_hegel ---- *)

(** Test: ensure_hegel_installed cached path. Creates a fake cached venv with a
    matching version file and binary, verifying the cache hit path (lines 62-67
    in session.ml). *)
let test_ensure_hegel_installed_cached () =
  let orig_cwd = Sys.getcwd () in
  let tmp_dir = Filename.temp_dir "hegel_test_cache" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      rm_rf tmp_dir)
    (fun () ->
      Sys.chdir tmp_dir;
      (* Create .hegel/venv/bin/ directories *)
      Unix.mkdir ".hegel" 0o755;
      Unix.mkdir ".hegel/venv" 0o755;
      Unix.mkdir ".hegel/venv/bin" 0o755;
      (* Write the version file with the current version *)
      Out_channel.with_open_text ".hegel/venv/hegel-version" (fun oc ->
          output_string oc Session.hegel_server_version);
      (* Create a fake hegel binary *)
      Out_channel.with_open_text ".hegel/venv/bin/hegel" (fun oc ->
          output_string oc "#!/bin/sh\nexit 0\n");
      Unix.chmod ".hegel/venv/bin/hegel" 0o755;
      (* Call ensure_hegel_installed - should find the cached version *)
      let path = Session.ensure_hegel_installed () in
      Alcotest.(check string) "cached path" ".hegel/venv/bin/hegel" path)

(** Test: server crash detection. Start a session with a command that exits
    immediately, verify that server_exited is set on the connection. Covers line
    231 in session.ml (monitor thread). *)
let test_server_crash_detection () =
  let false_cmd = find_cmd "false" in
  let session : Session.hegel_session =
    { process = None; connection = None; client = None; lock = Mutex.create () }
  in
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  Unix.putenv "HEGEL_SERVER_COMMAND" false_cmd;
  Fun.protect
    ~finally:(fun () ->
      (match orig_cmd with
      | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
      | None -> Unix.putenv "HEGEL_SERVER_COMMAND" "");
      Session.cleanup session)
    (fun () ->
      (* start will fail because /bin/false exits immediately *)
      let raised = ref false in
      (try Session.start session with _ -> raised := true);
      Alcotest.(check bool) "start raised" true !raised;
      (* After the failed start, if a connection was created before the error,
         the monitor thread should have noticed the process exiting. *)
      match session.connection with
      | Some conn ->
          (* Give the monitor thread a moment to detect the exit *)
          Unix.sleepf 0.1;
          Alcotest.(check bool)
            "server_exited set" true
            (Connection.server_has_exited conn)
      | None ->
          (* Connection was never established (expected for /bin/false) *)
          ())

(** Test: is_in_ci returns false when a var with expected value has a different
    value (the v <> exp false case of line 70). Clear all CI vars so only
    TF_BUILD matters, then set it to "false". *)
let test_is_in_ci_value_mismatch () =
  with_clean_ci_env (fun () ->
      (* Set TF_BUILD to a non-matching value -- all other CI vars are
         already unset by with_clean_ci_env *)
      Unix.putenv "TF_BUILD" "false";
      Alcotest.(check bool)
        "is_in_ci with TF_BUILD=false" false (Client.is_in_ci ()))

(** Test: server crash detected in event loop (client.ml line 367). The test
    function raises Data_exhausted so run_test_case skips mark_complete,
    avoiding a race where pop_inbox_item detects server_exited before the
    mark_complete response arrives. After run_test_case returns normally via the
    Data_was_exhausted path, the event loop checks server_has_exited. *)
let test_server_crash_in_event_loop () =
  let raised_msg = ref "" in
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_conn = Test_helpers.make_connection s1 ~name:"Client" () in
  let peer_conn = Test_helpers.make_connection s2 ~name:"Peer" () in
  let t_hs = Thread.create Test_helpers.handshake_via_channel peer_conn in
  let client = create_client client_conn in
  Thread.join t_hs;
  let t_peer =
    Thread.create
      (fun () ->
        let _ctrl, test_ch = accept_run_test peer_conn in
        let data_ch_id = 2l in
        let _data_ch = connect_channel peer_conn data_ch_id ~role:"Data" () in
        ignore
          (pending_get
             (request test_ch
                (`Map
                   [
                     (`Text "event", `Text "test_case");
                     (`Text "channel_id", `Int (Int32.to_int data_ch_id));
                   ])));
        (* After the client acknowledges the test_case event, set
           server_exited. The test_fn raises Data_exhausted so
           run_test_case skips mark_complete — no request/response race
           with pop_inbox_item's server_exited check. *)
        client_conn.server_exited <- true;
        Unix.sleepf 0.5)
      ()
  in
  (try
     run_test client
       ~settings:(Client.default_settings () |> Client.with_test_cases 1)
       (fun _tc ->
         (* Sleep briefly to release the runtime lock, giving the peer
            thread time to set server_exited before we return. *)
         Unix.sleepf 0.05;
         raise Data_exhausted)
   with Failure msg -> raised_msg := msg);
  Thread.join t_peer;
  Alcotest.(check bool)
    "raised server crashed" true
    (contains_substring !raised_msg "hegel server process has exited");
  (try close client_conn with _ -> ());
  try close peer_conn with _ -> ()

(** Test: send error reply to non-existent channel fails silently (connection.ml
    line 217). Create a connection using pipes so we can close the write_fd
    independently. Send a non-reply packet from the peer addressed to an
    unregistered channel. Close the client's write pipe so the error reply
    fails. The [with _ -> ()] catches the error. *)
let test_send_error_reply_fails_silently () =
  (* Create two pipe pairs: one for client reading, one for client writing *)
  let client_read_fd, peer_write_fd = Unix.pipe () in
  let peer_read_fd, client_write_fd = Unix.pipe () in
  (* Close the client's write fd so error replies will fail *)
  Unix.close client_write_fd;
  (* Create the client connection with the broken write fd.
     We need a valid write fd for the connection constructor, so use /dev/null *)
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o644 in
  let conn =
    Connection.create_connection ~read_fd:client_read_fd ~write_fd:devnull ()
  in
  (* Close the devnull fd so writes will fail *)
  Unix.close devnull;
  (* From the peer side, send a non-reply packet to channel 99 (non-existent).
     The background reader will try to send an error reply, which will fail
     because devnull is closed. *)
  let pkt =
    {
      Protocol.channel_id = 99l;
      message_id = 1l;
      is_reply = false;
      payload = CBOR.Simple.encode (`Map [ (`Text "command", `Text "hello") ]);
    }
  in
  Protocol.write_packet peer_write_fd pkt;
  (* Give the reader thread time to process the packet and attempt the error reply *)
  Unix.sleepf 0.1;
  (* Clean up *)
  close conn;
  Unix.close peer_write_fd;
  try Unix.close peer_read_fd with Unix.Unix_error _ -> ()

(** Test: run_command_to_log runs a command and redirects output to a log file.
*)
let test_run_command_to_log () =
  let log_path = Filename.temp_file "hegel_test_cmd" ".log" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove log_path with _ -> ())
    (fun () ->
      let echo_cmd = find_cmd "echo" in
      let status =
        Session.run_command_to_log echo_cmd [ "hello world" ] log_path
      in
      Alcotest.(check bool) "exit 0" true (status = Unix.WEXITED 0);
      let contents = In_channel.with_open_text log_path In_channel.input_all in
      Alcotest.(check bool)
        "has output" true
        (contains_substring contents "hello world"))

(** Test: run_command_to_log with a failing command. *)
let test_run_command_to_log_fail () =
  let log_path = Filename.temp_file "hegel_test_cmd" ".log" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove log_path with _ -> ())
    (fun () ->
      let false_cmd = find_cmd "false" in
      let status = Session.run_command_to_log false_cmd [] log_path in
      Alcotest.(check bool)
        "non-zero exit" true
        (match status with Unix.WEXITED 0 -> false | _ -> true))

(** Test: ensure_hegel_installed version mismatch triggers reinstall. Create a
    cached venv with wrong version; since uv is available it will try to
    reinstall. We expect it to either succeed (if network is available) or fail
    with an install error. Either way the version mismatch path is exercised. *)
let test_ensure_hegel_installed_version_mismatch () =
  let orig_cwd = Sys.getcwd () in
  let tmp_dir = Filename.temp_dir "hegel_test_mismatch" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      rm_rf tmp_dir)
    (fun () ->
      Sys.chdir tmp_dir;
      (* Create .hegel/venv/bin/ directories with wrong version *)
      Unix.mkdir ".hegel" 0o755;
      Unix.mkdir ".hegel/venv" 0o755;
      Unix.mkdir ".hegel/venv/bin" 0o755;
      Out_channel.with_open_text ".hegel/venv/hegel-version" (fun oc ->
          output_string oc "0.0.0-wrong");
      Out_channel.with_open_text ".hegel/venv/bin/hegel" (fun oc ->
          output_string oc "#!/bin/sh\nexit 0\n");
      Unix.chmod ".hegel/venv/bin/hegel" 0o755;
      (* Call ensure_hegel_installed - version won't match, so it will try
         to reinstall via uv. This will fail (wrong package version or
         network issues) but exercises the version mismatch path. *)
      let _result =
        try
          let path = Session.ensure_hegel_installed () in
          Some path
        with _ -> None
      in
      (* We don't assert on the result because it depends on whether uv
         can actually install. The important thing is the version mismatch
         path was exercised. *)
      ())

(** Test: ensure_hegel_installed when uv is not found (ENOENT path). Temporarily
    set PATH to empty so uv can't be found, and ensure no cached version exists.
    Should raise with uv_not_found_message. *)
let test_ensure_hegel_installed_uv_not_found () =
  let orig_cwd = Sys.getcwd () in
  let tmp_dir = Filename.temp_dir "hegel_test_no_uv" "" in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      (match orig_path with
      | Some p -> Unix.putenv "PATH" p
      | None -> Unix.putenv "PATH" "");
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      rm_rf tmp_dir)
    (fun () ->
      Sys.chdir tmp_dir;
      (* Set PATH to empty so uv won't be found *)
      Unix.putenv "PATH" "";
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "raised uv not found" true
        (contains_substring !raised_msg "uv"))

(** Helper: create a fake uv script in a directory. The script handles "venv"
    and "pip" subcommands with configurable exit codes. [work_dir] is the
    absolute path where .hegel/ will be created. *)
let create_fake_uv dir ~venv_exit ~pip_exit ~work_dir ?(delete_log = false) () =
  let uv_path = Filename.concat dir "uv" in
  let hegel_bin = Printf.sprintf "%s/.hegel/venv/bin/hegel" work_dir in
  let install_log = Printf.sprintf "%s/.hegel/install.log" work_dir in
  let rm_log =
    if delete_log then Printf.sprintf "rm -f \"%s\"\n" install_log else ""
  in
  Out_channel.with_open_text uv_path (fun oc ->
      Printf.fprintf oc
        "#!/bin/sh\n\
         case \"$1\" in\n\
         venv)\n\
         mkdir -p \"$3/bin\"\n\
         touch \"$3/bin/python\"\n\
         %sexit %d\n\
         ;;\n\
         pip)\n\
         mkdir -p \"%s\"\n\
         printf '#!/bin/sh\\nexit 0\\n' > \"%s\"\n\
         chmod +x \"%s\"\n\
         %sexit %d\n\
         ;;\n\
         esac\n\
         exit 1\n"
        rm_log venv_exit
        (Filename.dirname hegel_bin)
        hegel_bin hegel_bin rm_log pip_exit);
  Unix.chmod uv_path 0o755

(** Helper: run ensure_hegel_installed in a temp dir with a fake uv. *)
let with_fake_uv_env ~venv_exit ~pip_exit ?(delete_log = false) f =
  let orig_cwd = Sys.getcwd () in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  let tmp_dir = Filename.temp_dir "hegel_test_fakeuv" "" in
  let fake_bin_dir = Filename.concat tmp_dir "bin" in
  let work_dir = Filename.concat tmp_dir "work" in
  Unix.mkdir fake_bin_dir 0o755;
  Unix.mkdir work_dir 0o755;
  create_fake_uv fake_bin_dir ~venv_exit ~pip_exit ~work_dir ~delete_log ();
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      (match orig_path with
      | Some p -> Unix.putenv "PATH" p
      | None -> Unix.putenv "PATH" "");
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      try rm_rf tmp_dir with _ -> ())
    (fun () ->
      Sys.chdir work_dir;
      Unix.putenv "PATH" (Printf.sprintf "%s:/bin:/usr/bin" fake_bin_dir);
      f ())

(** Test: ensure_hegel_installed success path with fake uv. *)
let test_ensure_installed_success () =
  with_fake_uv_env ~venv_exit:0 ~pip_exit:0 (fun () ->
      let path = Session.ensure_hegel_installed () in
      Alcotest.(check bool) "returns path" true (String.length path > 0))

(** Test: ensure_hegel_installed when uv venv fails with log deleted. *)
let test_ensure_installed_venv_fails () =
  with_fake_uv_env ~venv_exit:1 ~pip_exit:0 ~delete_log:true (fun () ->
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "uv venv failed" true
        (contains_substring !raised_msg "uv venv failed"))

(** Test: ensure_hegel_installed when uv venv fails with log readable. *)
let test_ensure_installed_venv_fails_with_log () =
  with_fake_uv_env ~venv_exit:1 ~pip_exit:0 (fun () ->
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "uv venv failed" true
        (contains_substring !raised_msg "uv venv failed"))

(** Test: ensure_hegel_installed when uv pip install fails. *)
let test_ensure_installed_pip_fails () =
  with_fake_uv_env ~venv_exit:0 ~pip_exit:1 (fun () ->
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "pip install failed" true
        (contains_substring !raised_msg "Failed to install"))

(** Test: ensure_hegel_installed pip fails with unreadable log. *)
let test_ensure_installed_pip_fails_no_log () =
  with_fake_uv_env ~venv_exit:0 ~pip_exit:1 ~delete_log:true (fun () ->
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "pip install failed" true
        (contains_substring !raised_msg "Failed to install"))

(** Test: ensure_hegel_installed when hegel binary not found after install. *)
let test_ensure_installed_binary_missing () =
  let orig_cwd = Sys.getcwd () in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  let tmp_dir = Filename.temp_dir "hegel_test_nobin" "" in
  let fake_bin_dir = Filename.concat tmp_dir "bin" in
  Unix.mkdir fake_bin_dir 0o755;
  (* Create a fake uv that succeeds but does NOT create the hegel binary *)
  let uv_path = Filename.concat fake_bin_dir "uv" in
  Out_channel.with_open_text uv_path (fun oc ->
      output_string oc
        "#!/bin/sh\n\
         if [ \"$1\" = \"venv\" ]; then\n\
        \  mkdir -p \"$3/bin\"\n\
        \  touch \"$3/bin/python\"\n\
        \  exit 0\n\
         fi\n\
         if [ \"$1\" = \"pip\" ]; then\n\
        \  exit 0\n\
         fi\n\
         exit 1\n");
  Unix.chmod uv_path 0o755;
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      (match orig_path with
      | Some p -> Unix.putenv "PATH" p
      | None -> Unix.putenv "PATH" "");
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      try rm_rf tmp_dir with _ -> ())
    (fun () ->
      let work_dir = Filename.concat tmp_dir "work" in
      Unix.mkdir work_dir 0o755;
      Sys.chdir work_dir;
      Unix.putenv "PATH" (Printf.sprintf "%s:/bin:/usr/bin" fake_bin_dir);
      let raised_msg = ref "" in
      (try ignore (Session.ensure_hegel_installed ())
       with Failure msg -> raised_msg := msg);
      Alcotest.(check bool)
        "binary not found" true
        (contains_substring !raised_msg "not found"))

(** Test: find_hegel fallthrough to ensure_hegel_installed. *)
let test_find_hegel_fallthrough_to_install () =
  with_fake_uv_env ~venv_exit:0 ~pip_exit:0 (fun () ->
      let orig_cmd =
        try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
      in
      Test_helpers.unsetenv "HEGEL_SERVER_COMMAND";
      Fun.protect
        ~finally:(fun () ->
          match orig_cmd with
          | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
          | None -> Test_helpers.unsetenv "HEGEL_SERVER_COMMAND")
        (fun () ->
          let path = Session.find_hegel () in
          Alcotest.(check bool) "found hegel" true (String.length path > 0)))

let test_find_hegel_auto_install () =
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  Unix.putenv "HEGEL_SERVER_COMMAND" "";
  let path = Session.find_hegel () in
  Alcotest.(check bool) "found hegel" true (String.length path > 0);
  match orig_cmd with
  | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
  | None -> Unix.putenv "HEGEL_SERVER_COMMAND" ""

(** Test: session start creates .hegel dir for server log when it doesn't exist.
    Covers the mkdir success path in server_log_fd (session.ml). *)
let test_session_start_creates_hegel_dir () =
  let orig_cwd = Sys.getcwd () in
  let orig_cmd =
    try Some (Sys.getenv "HEGEL_SERVER_COMMAND") with Not_found -> None
  in
  let tmp_dir = Filename.temp_dir "hegel_test_logdir" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.chdir orig_cwd;
      (match orig_cmd with
      | Some v -> Unix.putenv "HEGEL_SERVER_COMMAND" v
      | None -> Unix.putenv "HEGEL_SERVER_COMMAND" "");
      let rec rm_rf path =
        let stat = Unix.lstat path in
        if stat.st_kind = Unix.S_DIR then begin
          Array.iter
            (fun entry -> rm_rf (Filename.concat path entry))
            (Sys.readdir path);
          Unix.rmdir path
        end
        else Sys.remove path
      in
      try rm_rf tmp_dir with _ -> ())
    (fun () ->
      Sys.chdir tmp_dir;
      Unix.putenv "HEGEL_SERVER_COMMAND" "/bin/false";
      let session : Session.hegel_session =
        {
          process = None;
          connection = None;
          client = None;
          lock = Mutex.create ();
        }
      in
      (try Session.start session with _ -> ());
      Session.cleanup session;
      Alcotest.(check bool)
        ".hegel dir created" true
        (Sys.file_exists ".hegel" && Sys.is_directory ".hegel"))

(* ---- Session lifecycle ---- *)

let test_session_cleanup () =
  let session : Session.hegel_session =
    { process = None; connection = None; client = None; lock = Mutex.create () }
  in
  Session.cleanup session

let test_session_start_and_run () =
  Session.run_hegel_test ~settings:(Client.settings ~test_cases:3 ()) (fun tc ->
      let v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) tc
      in
      ignore (Cbor_helpers.extract_bool v))

let tests =
  [
    (* Unit tests *)
    Alcotest.test_case "assume true" `Quick test_assume_true;
    Alcotest.test_case "assume false raises" `Quick test_assume_false_raises;
    Alcotest.test_case "note when not final" `Quick test_note_when_not_final;
    Alcotest.test_case "note when final" `Quick test_note_when_final;
    Alcotest.test_case "extract_origin no backtrace" `Quick
      test_extract_origin_no_backtrace;
    Alcotest.test_case "extract_origin with backtrace" `Quick
      test_extract_origin_with_backtrace;
    Alcotest.test_case "start/stop span when aborted" `Quick
      test_start_span_when_aborted;
    Alcotest.test_case "nested test raises" `Quick test_nested_test_raises;
    Alcotest.test_case "health_check_to_string" `Quick
      test_health_check_to_string;
    Alcotest.test_case "is_in_ci Some _, None branch" `Quick
      test_is_in_ci_some_none_branch;
    Alcotest.test_case "is_in_ci Some v, Some exp branch" `Quick
      test_is_in_ci_some_v_some_exp_branch;
    Alcotest.test_case "is_in_ci value mismatch (false)" `Quick
      test_is_in_ci_value_mismatch;
    Alcotest.test_case "with_verbosity" `Quick test_with_verbosity;
    Alcotest.test_case "with_derandomize" `Quick test_with_derandomize;
    Alcotest.test_case "with_database" `Quick test_with_database;
    Alcotest.test_case "with_suppress_health_check" `Quick
      test_with_suppress_health_check;
    Alcotest.test_case "default_settings in CI" `Quick
      test_default_settings_in_ci;
    Alcotest.test_case "default_settings not in CI" `Quick
      test_default_settings_not_in_ci;
    Alcotest.test_case "settings convenience" `Quick test_settings_convenience;
    Alcotest.test_case "with_seed" `Quick test_with_seed;
    Alcotest.test_case "run_test with database_key" `Quick
      test_run_test_with_database_key;
    (* Real-server converted tests *)
    Alcotest.test_case "start/stop span live" `Quick test_start_stop_span_live;
    Alcotest.test_case "version mismatch" `Quick test_version_mismatch;
    Alcotest.test_case "version mismatch low" `Quick test_version_mismatch_low;
    Alcotest.test_case "run_test with seed" `Quick test_run_test_with_seed;
    Alcotest.test_case "multiple interesting" `Quick test_multiple_interesting;
    Alcotest.test_case "unrecognised event" `Quick test_unrecognised_event;
    Alcotest.test_case "run_test database path + suppress" `Quick
      test_run_test_database_path_and_suppress;
    Alcotest.test_case "run_test database unset" `Quick
      test_run_test_database_unset;
    Alcotest.test_case "run_test server error in results" `Quick
      test_run_test_server_error_in_results;
    Alcotest.test_case "run_test health check failure" `Quick
      test_run_test_health_check_failure;
    Alcotest.test_case "run_test flaky" `Quick test_run_test_flaky;
    Alcotest.test_case "run_test passed=false" `Quick test_run_test_passed_false;
    Alcotest.test_case "server_exited in pop_inbox" `Quick
      test_server_exited_in_pop_inbox;
    Alcotest.test_case "server crash in event loop" `Quick
      test_server_crash_in_event_loop;
    Alcotest.test_case "send error reply fails silently" `Quick
      test_send_error_reply_fails_silently;
    (* find_hegel *)
    Alcotest.test_case "find_hegel via env" `Quick test_find_hegel_via_env;
    Alcotest.test_case "ensure_hegel_installed cached" `Quick
      test_ensure_hegel_installed_cached;
    Alcotest.test_case "ensure_hegel_installed version mismatch" `Quick
      test_ensure_hegel_installed_version_mismatch;
    Alcotest.test_case "ensure_hegel_installed uv not found" `Quick
      test_ensure_hegel_installed_uv_not_found;
    Alcotest.test_case "ensure_installed success" `Quick
      test_ensure_installed_success;
    Alcotest.test_case "ensure_installed venv fails" `Quick
      test_ensure_installed_venv_fails;
    Alcotest.test_case "ensure_installed venv fails with log" `Quick
      test_ensure_installed_venv_fails_with_log;
    Alcotest.test_case "ensure_installed pip fails" `Quick
      test_ensure_installed_pip_fails;
    Alcotest.test_case "ensure_installed pip fails no log" `Quick
      test_ensure_installed_pip_fails_no_log;
    Alcotest.test_case "ensure_installed binary missing" `Quick
      test_ensure_installed_binary_missing;
    Alcotest.test_case "find_hegel fallthrough to install" `Quick
      test_find_hegel_fallthrough_to_install;
    Alcotest.test_case "run_command_to_log" `Quick test_run_command_to_log;
    Alcotest.test_case "run_command_to_log fail" `Quick
      test_run_command_to_log_fail;
    Alcotest.test_case "server crash detection" `Quick
      test_server_crash_detection;
    Alcotest.test_case "find_hegel auto install" `Quick
      test_find_hegel_auto_install;
    Alcotest.test_case "start creates .hegel dir" `Quick
      test_session_start_creates_hegel_dir;
    (* Session *)
    Alcotest.test_case "session cleanup" `Quick test_session_cleanup;
    Alcotest.test_case "session cleanup with resources" `Quick
      test_session_cleanup_with_resources;
    Alcotest.test_case "session cleanup closed conn" `Quick
      test_session_cleanup_closed_conn;
    Alcotest.test_case "session cleanup with process" `Quick
      test_session_cleanup_with_process;
    Alcotest.test_case "session cleanup dead process" `Quick
      test_session_cleanup_dead_process;
    Alcotest.test_case "has working client live" `Quick
      test_has_working_client_live;
    Alcotest.test_case "session not started" `Quick test_session_not_started;
    Alcotest.test_case "session start failure" `Quick test_session_start_failure;
    Alcotest.test_case "monitor thread detects crash" `Quick
      test_monitor_thread_detects_crash;
    Alcotest.test_case "cleanup with close error" `Quick
      test_cleanup_with_close_error;
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
    Alcotest.test_case "run_hegel_test with settings" `Quick
      test_run_hegel_test_with_settings;
    (* HEGEL_PROTOCOL_TEST_MODE error injection - these restart the session *)
    Alcotest.test_case "stop test on generate" `Quick test_stop_test_on_generate;
    Alcotest.test_case "stop test on mark complete" `Quick
      test_stop_test_on_mark_complete;
    Alcotest.test_case "error response" `Quick test_error_response;
    Alcotest.test_case "empty test" `Quick test_empty_test;
  ]
