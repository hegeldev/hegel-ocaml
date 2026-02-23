open Hegel.Connection
open Hegel.Client

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
let contains_substring s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

(* ---- Unit tests for helpers that don't need a server ---- *)

let test_assume_true () = assume true

let test_assume_false_raises () =
  let raised = ref false in
  (try assume false with Assume_rejected -> raised := true);
  Alcotest.(check bool) "raised Assume_rejected" true !raised

let test_get_channel_outside_context () =
  let raised = ref false in
  (try ignore (get_channel ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Not in a test context'" true
       (contains_substring msg "Not in a test"));
  Alcotest.(check bool) "raised" true !raised

let test_note_when_not_final () =
  is_final_run := false;
  note "should not print"

let test_note_when_final () =
  is_final_run := true;
  note "test message from note";
  is_final_run := false

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
  test_aborted := true;
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  current_channel := Some (control_channel conn);
  start_span ();
  stop_span ();
  current_channel := None;
  test_aborted := false;
  close conn;
  Unix.close s2

let test_nested_test_raises () =
  (* Set the in_test flag to simulate being inside a test *)
  in_test := true;
  let raised = ref false in
  (try Hegel.Session.run_hegel_test ~name:"nested" ~test_cases:1 (fun () -> ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Cannot nest'" true
       (contains_substring msg "Cannot nest"));
  in_test := false;
  Alcotest.(check bool) "raised" true !raised

(* ---- Unrecognised event test using socketpair ---- *)

let test_unrecognised_event () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let fake_server () =
    receive_handshake server_conn;
    let control = control_channel server_conn in
    let msg_id, message = receive_request control () in
    let pairs = Hegel.Cbor_helpers.extract_dict message in
    let test_ch_id =
      Int32.of_int
        (Hegel.Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
    in
    let test_channel = connect_channel server_conn test_ch_id ~role:"Test" () in
    send_response_value control msg_id (`Bool true);
    let req_id =
      send_request test_channel (`Map [ (`Text "event", `Text "bogus_event") ])
    in
    ignore (receive_response_raw test_channel req_id ());
    ignore
      (pending_get
         (request test_channel
            (`Map
               [
                 (`Text "event", `Text "test_done");
                 ( `Text "results",
                   `Map
                     [
                       (`Text "passed", `Bool true);
                       (`Text "test_cases", `Int 0);
                       (`Text "valid_test_cases", `Int 0);
                       (`Text "invalid_test_cases", `Int 0);
                       (`Text "interesting_test_cases", `Int 0);
                     ] );
               ])))
  in
  let t = Thread.create fake_server () in
  let c = create_client client_conn in
  run_test c ~name:"test_bogus" ~test_cases:1 (fun () -> ());
  close client_conn;
  close server_conn;
  Thread.join t

(* ---- Tests using real hegel binary ---- *)

(** Helper: run a test with a specific HEGEL_TEST_MODE. Restarts the session so
    the new env var takes effect on the subprocess. *)
let with_test_mode mode f =
  Hegel.Session.restart_session ();
  Unix.putenv "HEGEL_TEST_MODE" mode;
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "HEGEL_TEST_MODE" "";
      Hegel.Session.restart_session ())
    f

let test_simple_passing_test () =
  Hegel.Session.run_hegel_test ~name:"simple_pass" ~test_cases:5 (fun () ->
      let v = generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) in
      ignore (Hegel.Cbor_helpers.extract_bool v))

let test_simple_failing_test () =
  let raised = ref false in
  (try
     Hegel.Session.run_hegel_test ~name:"simple_fail" ~test_cases:100 (fun () ->
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
         in
         let x = Hegel.Cbor_helpers.extract_int v in
         if x >= 50 then failwith "too big")
   with _ -> raised := true);
  Alcotest.(check bool) "test failed" true !raised

let test_single_test_case () =
  Hegel.Session.run_hegel_test ~name:"single_case" ~test_cases:1 (fun () ->
      let v = generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) in
      ignore (Hegel.Cbor_helpers.extract_bool v))

let test_assume_true_e2e () =
  Hegel.Session.run_hegel_test ~name:"assume_true" ~test_cases:5 (fun () ->
      assume true)

let test_assume_false_e2e () =
  Hegel.Session.run_hegel_test ~name:"assume_false" ~test_cases:5 (fun () ->
      assume false)

let test_note_not_final_e2e () =
  Hegel.Session.run_hegel_test ~name:"note_nonfinal" ~test_cases:5 (fun () ->
      note "should not appear";
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 10);
             ])
      in
      ignore (Hegel.Cbor_helpers.extract_int v))

let test_target_e2e () =
  Hegel.Session.run_hegel_test ~name:"target_test" ~test_cases:5 (fun () ->
      let v =
        generate_from_schema
          (`Map
             [
               (`Text "type", `Text "integer");
               (`Text "min_value", `Int 0);
               (`Text "max_value", `Int 100);
             ])
      in
      let x = Hegel.Cbor_helpers.extract_int v in
      target (float_of_int x) "maximize_x")

(* ---- HEGEL_TEST_MODE error injection tests ---- *)

let test_stop_test_on_generate () =
  with_test_mode "stop_test_on_generate" (fun () ->
      Hegel.Session.run_hegel_test ~name:"stop_on_gen" ~test_cases:5 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

let test_stop_test_on_mark_complete () =
  with_test_mode "stop_test_on_mark_complete" (fun () ->
      Hegel.Session.run_hegel_test ~name:"stop_on_mc" ~test_cases:5 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

let test_error_response () =
  with_test_mode "error_response" (fun () ->
      Hegel.Session.run_hegel_test ~name:"error_resp" ~test_cases:5 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

let test_empty_test () =
  with_test_mode "empty_test" (fun () ->
      Hegel.Session.run_hegel_test ~name:"empty" ~test_cases:5 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

(* ---- Mark complete status values ---- *)

let test_mark_complete_valid () =
  Hegel.Session.run_hegel_test ~name:"mc_valid" ~test_cases:3 (fun () ->
      let v = generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) in
      ignore (Hegel.Cbor_helpers.extract_bool v))

let test_mark_complete_invalid () =
  Hegel.Session.run_hegel_test ~name:"mc_invalid" ~test_cases:5 (fun () ->
      let _v =
        generate_from_schema (`Map [ (`Text "type", `Text "boolean") ])
      in
      assume false)

let test_mark_complete_interesting () =
  let raised = ref false in
  (try
     Hegel.Session.run_hegel_test ~name:"mc_interesting" ~test_cases:100
       (fun () ->
         let v =
           generate_from_schema
             (`Map
                [
                  (`Text "type", `Text "integer");
                  (`Text "min_value", `Int 0);
                  (`Text "max_value", `Int 100);
                ])
         in
         let x = Hegel.Cbor_helpers.extract_int v in
         assert (x < 50))
   with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(* ---- socketpair-based tests for hard-to-reach code paths ---- *)

(** Helper: create a fake server that runs a function, then joins the thread. *)
let with_fake_server server_fn client_fn =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t = Thread.create server_fn server_conn in
  client_fn client_conn;
  close client_conn;
  close server_conn;
  Thread.join t

(** Helper: accept a run_test command on the control channel and return the test
    channel. *)
let accept_run_test server_conn =
  receive_handshake server_conn;
  let control = control_channel server_conn in
  let msg_id, message = receive_request control () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let test_ch_id =
    Int32.of_int
      (Hegel.Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
  in
  let test_channel = connect_channel server_conn test_ch_id ~role:"Test" () in
  send_response_value control msg_id (`Bool true);
  test_channel

(** Helper: send a test_case event and return the data channel. *)
let send_test_case server_conn test_channel =
  let data_ch = new_channel server_conn ~role:"Data" () in
  let req_id =
    send_request test_channel
      (`Map
         [
           (`Text "event", `Text "test_case");
           (`Text "channel", `Int (Int32.to_int data_ch.channel_id));
         ])
  in
  ignore (receive_response_raw test_channel req_id ());
  data_ch

(** Helper: send test_done with given interesting count. *)
let send_test_done test_channel ~interesting =
  ignore
    (pending_get
       (request test_channel
          (`Map
             [
               (`Text "event", `Text "test_done");
               ( `Text "results",
                 `Map
                   [
                     (`Text "passed", `Bool (interesting = 0));
                     (`Text "test_cases", `Int 1);
                     ( `Text "valid_test_cases",
                       `Int (if interesting = 0 then 1 else 0) );
                     (`Text "invalid_test_cases", `Int 0);
                     (`Text "interesting_test_cases", `Int interesting);
                   ] );
             ])))

(** Helper: handle a generate request on data channel and respond with a boolean
    value. *)
let handle_generate data_ch =
  let msg_id, _message = receive_request data_ch () in
  send_response_value data_ch msg_id (`Bool true)

(** Helper: handle a mark_complete request. *)
let handle_mark_complete data_ch =
  let msg_id, _message = receive_request data_ch () in
  send_response_value data_ch msg_id `Null

(** Test: start_span and stop_span when NOT aborted (live connection). *)
let test_start_stop_span_live () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Handle start_span *)
      let msg_id, _ = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      (* Handle stop_span *)
      let msg_id, _ = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      (* Handle generate *)
      handle_generate data_ch;
      (* Handle mark_complete *)
      handle_mark_complete data_ch;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = create_client client_conn in
      run_test c ~name:"span_test" ~test_cases:1 (fun () ->
          start_span ();
          stop_span ();
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

(** Test: version mismatch in create_client (version too high). *)
let test_version_mismatch () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        (* Receive the handshake and respond with a bad version *)
        server_conn.connection_state <- Server;
        let ch = control_channel server_conn in
        let msg_id, _payload = receive_request_raw ch () in
        send_response_raw ch msg_id "Hegel/9.9")
      ()
  in
  let raised = ref false in
  (try ignore (create_client client_conn) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised version mismatch" true !raised;
  close client_conn;
  close server_conn;
  Thread.join t

(** Test: version mismatch in create_client (version too low). *)
let test_version_mismatch_low () =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        server_conn.connection_state <- Server;
        let ch = control_channel server_conn in
        let msg_id, _payload = receive_request_raw ch () in
        send_response_raw ch msg_id "Hegel/0.0")
      ()
  in
  let raised = ref false in
  (try ignore (create_client client_conn) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised version mismatch low" true !raised;
  close client_conn;
  close server_conn;
  Thread.join t

(** Test: Data_exhausted path (StopTest on generate). *)
let test_data_exhausted_via_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Respond to generate with StopTest *)
      let msg_id, _ = receive_request data_ch () in
      send_response_error data_ch msg_id ~error:"Stop" ~error_type:"StopTest" ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = create_client client_conn in
      run_test c ~name:"data_exhausted" ~test_cases:1 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

(** Test: StopTest on mark_complete. *)
let test_stop_test_on_mark_complete_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Handle generate normally *)
      handle_generate data_ch;
      (* Respond to mark_complete with StopTest *)
      let msg_id, _ = receive_request data_ch () in
      send_response_error data_ch msg_id ~error:"Stop" ~error_type:"StopTest" ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = create_client client_conn in
      run_test c ~name:"stop_on_mc" ~test_cases:1 (fun () ->
          ignore
            (generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]))))

(** Test: multiple interesting test cases (n_interesting > 1). *)
let test_multiple_interesting () =
  let raised = ref false in
  (try
     with_fake_server
       (fun server_conn ->
         let test_channel = accept_run_test server_conn in
         (* Send 2 test cases, both will be INTERESTING *)
         for _ = 1 to 2 do
           let data_ch = send_test_case server_conn test_channel in
           handle_generate data_ch;
           handle_mark_complete data_ch
         done;
         (* Send test_done with interesting=2 *)
         send_test_done test_channel ~interesting:2;
         (* Send 2 replay test cases *)
         for _ = 1 to 2 do
           let data_ch = send_test_case server_conn test_channel in
           handle_generate data_ch;
           handle_mark_complete data_ch
         done)
       (fun client_conn ->
         let c = create_client client_conn in
         run_test c ~name:"multi_interesting" ~test_cases:2 (fun () ->
             let v =
               generate_from_schema (`Map [ (`Text "type", `Text "boolean") ])
             in
             ignore v;
             failwith "always fails"))
   with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: multiple interesting with replay that passes (covers the "Expected
    test case to fail" branch). *)
let test_multiple_interesting_pass () =
  let raised = ref false in
  (try
     with_fake_server
       (fun server_conn ->
         let test_channel = accept_run_test server_conn in
         (* Send 2 test cases, both will be INTERESTING *)
         for _ = 1 to 2 do
           let data_ch = send_test_case server_conn test_channel in
           handle_generate data_ch;
           handle_mark_complete data_ch
         done;
         (* Send test_done with interesting=2 *)
         send_test_done test_channel ~interesting:2;
         (* Send 2 replay test cases - this time test will pass *)
         for _ = 1 to 2 do
           let data_ch = send_test_case server_conn test_channel in
           handle_generate data_ch;
           handle_mark_complete data_ch
         done)
       (fun client_conn ->
         let c = create_client client_conn in
         let count = ref 0 in
         run_test c ~name:"multi_pass" ~test_cases:2 (fun () ->
             let v =
               generate_from_schema (`Map [ (`Text "type", `Text "boolean") ])
             in
             ignore v;
             incr count;
             (* Only fail on first 2 calls (non-final), pass on replay *)
             if !count <= 2 then failwith "fail on initial run"))
   with _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: find_hegeld fails when binary not on PATH. *)
let test_find_hegeld_not_found () =
  let orig_binary =
    try Some (Sys.getenv "HEGEL_BINARY") with Not_found -> None
  in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  Unix.putenv "HEGEL_BINARY" "";
  Unix.putenv "PATH" "/nonexistent";
  let raised = ref false in
  (try ignore (Hegel.Session.find_hegeld ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  (match orig_binary with
  | Some v -> Unix.putenv "HEGEL_BINARY" v
  | None -> Unix.putenv "HEGEL_BINARY" "");
  match orig_path with
  | Some v -> Unix.putenv "PATH" v
  | None -> Unix.putenv "PATH" ""

(** Test: find_hegeld when PATH is not set at all. *)
let test_find_hegeld_no_path () =
  let orig_binary =
    try Some (Sys.getenv "HEGEL_BINARY") with Not_found -> None
  in
  let orig_path = try Some (Sys.getenv "PATH") with Not_found -> None in
  Unix.putenv "HEGEL_BINARY" "";
  (* We can't truly unset PATH in OCaml, but set to empty *)
  Unix.putenv "PATH" "";
  let raised = ref false in
  (try ignore (Hegel.Session.find_hegeld ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  (match orig_binary with
  | Some v -> Unix.putenv "HEGEL_BINARY" v
  | None -> Unix.putenv "HEGEL_BINARY" "");
  match orig_path with
  | Some v -> Unix.putenv "PATH" v
  | None -> Unix.putenv "PATH" ""

(** Test: session start timeout (unreachable hegeld). Also covers the
    Unix.Unix_error retry path by creating a non-socket file at the socket path.
*)
let test_session_start_timeout () =
  let session : Hegel.Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  (* Set HEGEL_BINARY to a script that creates a regular file (not socket)
     at the socket path, which will cause Unix.Unix_error on connect. *)
  let orig_binary =
    try Some (Sys.getenv "HEGEL_BINARY") with Not_found -> None
  in
  let script_path = Filename.temp_file "hegel-test-" ".sh" in
  let oc = open_out script_path in
  (* Create a regular file (not a socket) at the path and exit immediately.
     This covers both the Unix_error retry (connect to non-socket) and the
     SIGKILL error catch (process already dead). *)
  output_string oc "#!/bin/sh\ntouch \"$1\"\n";
  close_out oc;
  Unix.chmod script_path 0o755;
  Unix.putenv "HEGEL_BINARY" script_path;
  let raised = ref false in
  (try Hegel.Session.start session
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'Timeout'" true
       (contains_substring msg "Timeout"));
  Alcotest.(check bool) "raised timeout" true !raised;
  Hegel.Session.cleanup session;
  (try Sys.remove script_path with _ -> ());
  match orig_binary with
  | Some v -> Unix.putenv "HEGEL_BINARY" v
  | None -> Unix.putenv "HEGEL_BINARY" ""

(** Test: run_hegel_test when session is None (unreachable in normal usage, but
    covers the branch). *)
let test_session_not_started () =
  let session : Hegel.Session.hegel_session =
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
    (Hegel.Session.has_working_client session)

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
  let session : Hegel.Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      socket_path = Some socket_path;
      temp_dir = Some temp_dir;
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session;
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
  let session : Hegel.Session.hegel_session =
    {
      process = None;
      connection = Some conn;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.connection = None)

(** Test: cleanup with nonexistent socket/dir paths (covers error catches). *)
let test_session_cleanup_bad_paths () =
  let session : Hegel.Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = Some "/nonexistent/path/hegel.sock";
      temp_dir = Some "/nonexistent/path";
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session;
  Alcotest.(check bool) "socket cleaned" true (session.socket_path = None);
  Alcotest.(check bool) "temp cleaned" true (session.temp_dir = None)

(** Test: cleanup a session with a process (uses /bin/sleep for a real PID). *)
let test_session_cleanup_with_process () =
  let pid =
    Unix.create_process "/bin/sleep" [| "/bin/sleep"; "60" |] Unix.stdin
      Unix.stderr Unix.stderr
  in
  let session : Hegel.Session.hegel_session =
    {
      process = Some pid;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session;
  Alcotest.(check bool) "process cleaned" true (session.process = None)

(** Test: cleanup with already-dead process (covers kill error catch). *)
let test_session_cleanup_dead_process () =
  (* Start a process that exits immediately *)
  let pid =
    Unix.create_process "/bin/true" [| "/bin/true" |] Unix.stdin Unix.stderr
      Unix.stderr
  in
  (* Wait for it to finish *)
  ignore (Unix.waitpid [] pid);
  let session : Hegel.Session.hegel_session =
    {
      process = Some pid;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session;
  Alcotest.(check bool) "cleaned" true (session.process = None)

(** Test: has_working_client with a live connection. *)
let test_has_working_client_live () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  let session : Hegel.Session.hegel_session =
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
    (Hegel.Session.has_working_client session);
  close conn;
  Unix.close s2

(** Test: run_hegel_test with default parameters. *)
let test_run_hegel_test_defaults () =
  Hegel.Session.run_hegel_test (fun () ->
      let v = generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) in
      ignore (Hegel.Cbor_helpers.extract_bool v))

(** Test: no event field in message. *)
let test_no_event_field () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      (* Send a message with no "event" field *)
      let req_id =
        send_request test_channel (`Map [ (`Text "foo", `Text "bar") ])
      in
      ignore (receive_response_raw test_channel req_id ());
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = create_client client_conn in
      run_test c ~name:"no_event" ~test_cases:1 (fun () -> ()))

(** Test: nest test_case raises (when current_channel is already set). *)
let test_run_test_case_nest () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Test" () in
  current_channel := Some (control_channel conn);
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
  current_channel := None;
  close conn;
  Unix.close s2

(* ---- find_hegeld ---- *)

let test_find_hegeld_via_env () =
  Unix.putenv "HEGEL_BINARY" "/tmp/fake-hegel";
  let path = Hegel.Session.find_hegeld () in
  Alcotest.(check string) "path" "/tmp/fake-hegel" path;
  Unix.putenv "HEGEL_BINARY" ""

let test_find_hegeld_on_path () =
  let orig = try Some (Sys.getenv "HEGEL_BINARY") with Not_found -> None in
  Unix.putenv "HEGEL_BINARY" "";
  let path = Hegel.Session.find_hegeld () in
  Alcotest.(check bool) "found hegel" true (String.length path > 0);
  match orig with
  | Some v -> Unix.putenv "HEGEL_BINARY" v
  | None -> Unix.putenv "HEGEL_BINARY" ""

(* ---- Session lifecycle ---- *)

let test_session_cleanup () =
  let session : Hegel.Session.hegel_session =
    {
      process = None;
      connection = None;
      client = None;
      socket_path = None;
      temp_dir = None;
      lock = Mutex.create ();
    }
  in
  Hegel.Session.cleanup session

let test_session_start_and_run () =
  Hegel.Session.run_hegel_test ~name:"session_test" ~test_cases:3 (fun () ->
      let v = generate_from_schema (`Map [ (`Text "type", `Text "boolean") ]) in
      ignore (Hegel.Cbor_helpers.extract_bool v))

let tests =
  [
    (* Unit tests *)
    Alcotest.test_case "assume true" `Quick test_assume_true;
    Alcotest.test_case "assume false raises" `Quick test_assume_false_raises;
    Alcotest.test_case "get_channel outside context" `Quick
      test_get_channel_outside_context;
    Alcotest.test_case "note when not final" `Quick test_note_when_not_final;
    Alcotest.test_case "note when final" `Quick test_note_when_final;
    Alcotest.test_case "extract_origin no backtrace" `Quick
      test_extract_origin_no_backtrace;
    Alcotest.test_case "extract_origin with backtrace" `Quick
      test_extract_origin_with_backtrace;
    Alcotest.test_case "start/stop span when aborted" `Quick
      test_start_span_when_aborted;
    Alcotest.test_case "nested test raises" `Quick test_nested_test_raises;
    Alcotest.test_case "unrecognised event" `Quick test_unrecognised_event;
    (* Socketpair-based coverage tests *)
    Alcotest.test_case "start/stop span live" `Quick test_start_stop_span_live;
    Alcotest.test_case "version mismatch" `Quick test_version_mismatch;
    Alcotest.test_case "version mismatch low" `Quick test_version_mismatch_low;
    Alcotest.test_case "data exhausted via socketpair" `Quick
      test_data_exhausted_via_socketpair;
    Alcotest.test_case "StopTest on mark_complete socketpair" `Quick
      test_stop_test_on_mark_complete_socketpair;
    Alcotest.test_case "multiple interesting" `Quick test_multiple_interesting;
    Alcotest.test_case "multiple interesting pass" `Quick
      test_multiple_interesting_pass;
    (* find_hegeld *)
    Alcotest.test_case "find_hegeld via env" `Quick test_find_hegeld_via_env;
    Alcotest.test_case "find_hegeld on path" `Quick test_find_hegeld_on_path;
    Alcotest.test_case "find_hegeld not found" `Quick test_find_hegeld_not_found;
    Alcotest.test_case "find_hegeld no path" `Quick test_find_hegeld_no_path;
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
    Alcotest.test_case "no event field" `Quick test_no_event_field;
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
    (* HEGEL_TEST_MODE error injection - these restart the session *)
    Alcotest.test_case "stop test on generate" `Quick test_stop_test_on_generate;
    Alcotest.test_case "stop test on mark complete" `Quick
      test_stop_test_on_mark_complete;
    Alcotest.test_case "error response" `Quick test_error_response;
    Alcotest.test_case "empty test" `Quick test_empty_test;
  ]
