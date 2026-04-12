open! Core
module Unix = Core_unix
module Thread = Caml_threads.Thread
module Mutex = Caml_threads.Mutex
open Hegel
open Protocol
open Connection

let contains_substring = Test_helpers.contains_substring
let make_socket_pair = Test_helpers.make_socket_pair
let make_connection = Test_helpers.make_connection
let handshake_pair = Test_helpers.handshake_pair

(* ---- Connection lifecycle ---- *)

let test_connection_live () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Live" () in
  Alcotest.(check bool) "live before close" true (is_live conn);
  close conn;
  Alcotest.(check bool) "not live after close" false (is_live conn)

let test_connection_double_close () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"DoubleClose" () in
  close conn;
  close conn (* second close is a no-op *)

(* ---- Handshake ---- *)

let test_send_handshake_returns_version () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t = Thread.create Test_helpers.handshake_via_stream peer_conn in
  let version = send_handshake client_conn in
  Thread.join t;
  Alcotest.(check string) "version" "0.10" version;
  close client_conn;
  close peer_conn

let test_double_handshake_send_raises () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let raised = ref false in
  (try ignore (send_handshake client_conn)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool) "msg" true (String.length msg > 0));
  Alcotest.(check bool) "raised" true !raised;
  close client_conn;
  close peer_conn

let test_send_handshake_bad_response () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        let ch = control_stream peer_conn in
        peer_conn.connection_state <- Client;
        let msg_id, _payload = receive_request_raw ch in
        send_response_raw ch msg_id "NotOk")
      ()
  in
  let raised = ref false in
  (try ignore (send_handshake client_conn)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "bad handshake response" true
       (contains_substring msg "Bad handshake"));
  Thread.join t;
  Alcotest.(check bool) "raised" true !raised;
  close client_conn;
  close peer_conn

(* ---- Stream allocation ---- *)

let test_new_stream_before_handshake_raises () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let raised = ref false in
  (try ignore (new_stream conn ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  close conn

let test_connect_stream_before_handshake_raises () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let raised = ref false in
  (try ignore (connect_stream conn 1l ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  close conn

let test_connect_stream_already_exists_raises () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let raised = ref false in
  (try ignore (connect_stream client_conn 0l ())
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "already connected" true
       (contains_substring msg "already connected"));
  Alcotest.(check bool) "raised" true !raised;
  close client_conn;
  close peer_conn

let test_stream_ids_are_odd_for_client () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch1 = new_stream client_conn () in
  let ch2 = new_stream client_conn () in
  let ch3 = new_stream client_conn () in
  (* Client streams: (1<<1)|1=3, (2<<1)|1=5, (3<<1)|1=7 *)
  Alcotest.(check int32) "ch1 id" 3l ch1.stream_id;
  Alcotest.(check int32) "ch2 id" 5l ch2.stream_id;
  Alcotest.(check int32) "ch3 id" 7l ch3.stream_id;
  (* All odd *)
  Alcotest.(check bool) "ch1 odd" true Int32.(equal (rem ch1.stream_id 2l) 1l);
  Alcotest.(check bool) "ch2 odd" true Int32.(equal (rem ch2.stream_id 2l) 1l);
  Alcotest.(check bool) "ch3 odd" true Int32.(equal (rem ch3.stream_id 2l) 1l);
  close client_conn;
  close peer_conn

(* ---- Stream operations ---- *)

let test_stream_close () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"TestClose" () in
  close_stream ch;
  (* Closing again should be a no-op *)
  close_stream ch;
  close peer_conn;
  close client_conn

let test_stream_close_when_connection_not_live () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"TestClose" () in
  close client_conn;
  close_stream ch;
  close peer_conn

let test_stream_process_message_when_closed () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"TestClosed" () in
  close_stream ch;
  let raised = ref false in
  (try ignore (receive_request ch)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool) "is closed" true (contains_substring msg "is closed"));
  Alcotest.(check bool) "raised" true !raised;
  close peer_conn;
  close client_conn

(* ---- Stream repr and name ---- *)

let test_stream_repr () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let ch = control_stream conn in
  let r = stream_repr ch in
  Alcotest.(check bool) "Control in repr" true (contains_substring r "Control");
  close conn

let test_stream_repr_no_role () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Test" () in
  let client_conn = make_connection s2 ~name:"Test2" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn () in
  let r = stream_repr ch in
  Alcotest.(check bool) "Stream( in repr" true (contains_substring r "Stream(");
  Alcotest.(check bool)
    "no role= in repr" true
    (not (contains_substring r "role="));
  close peer_conn;
  close client_conn

let test_stream_repr_with_role () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Test" () in
  let client_conn = make_connection s2 ~name:"Test2" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"TestRole" () in
  let r = stream_repr ch in
  Alcotest.(check bool)
    "role=TestRole in repr" true
    (contains_substring r "role=TestRole");
  close peer_conn;
  close client_conn

let test_stream_name_no_role_no_conn_name () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 () in
  let client_conn = make_connection s2 () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn () in
  let expected = Printf.sprintf "Stream %ld" ch.stream_id in
  Alcotest.(check string) "name" expected (stream_name ch);
  close peer_conn;
  close client_conn

let test_stream_name_with_role_no_conn_name () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 () in
  let client_conn = make_connection s2 () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"MyRole" () in
  let n = stream_name ch in
  Alcotest.(check bool) "has MyRole" true (contains_substring n "MyRole");
  close peer_conn;
  close client_conn

let test_stream_name_no_role_with_conn_name () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Srv" () in
  let client_conn = make_connection s2 ~name:"Cli" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn () in
  let n = stream_name ch in
  Alcotest.(check bool) "has Cli" true (contains_substring n "Cli");
  Alcotest.(check bool)
    "no parens (no role)" true
    (not (contains_substring n "("));
  close peer_conn;
  close client_conn

let test_stream_name_control () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 () in
  let ch = control_stream conn in
  let n = stream_name ch in
  Alcotest.(check bool) "has Control" true (contains_substring n "Control");
  close conn

(* ---- Request/response ---- *)

let test_request_response () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let peer_done = ref false in
  let t =
    Thread.create
      (fun () ->
        (* Register the handler stream before the handshake response so
           the background reader can route the client's first request.
           Without this, there's a race: the client can send on stream 3
           before new_stream registers it on the peer. *)
        peer_conn.connection_state <- Client;
        let ch = new_stream peer_conn ~role:"Handler" () in
        Test_helpers.handshake_via_stream peer_conn;
        (try
           while true do
             let msg_id, message = receive_request ch in
             let x =
               Cbor_helpers.extract_int
                 (List.Assoc.find_exn
                    (Cbor_helpers.extract_dict message)
                    ~equal:Poly.( = ) (`Text "x"))
             in
             let y =
               Cbor_helpers.extract_int
                 (List.Assoc.find_exn
                    (Cbor_helpers.extract_dict message)
                    ~equal:Poly.( = ) (`Text "y"))
             in
             send_response_value ch msg_id
               (`Map [ (`Text "sum", `Int (x + y)) ])
           done
         with Failure _ -> ());
        peer_done := true)
      ()
  in
  ignore (send_handshake client_conn);
  let send_ch = connect_stream client_conn 3l () in
  let result =
    pending_get
      (request send_ch (`Map [ (`Text "x", `Int 2); (`Text "y", `Int 3) ]))
  in
  let sum =
    Cbor_helpers.extract_int
      (List.Assoc.find_exn
         (Cbor_helpers.extract_dict result)
         ~equal:Poly.( = ) (`Text "sum"))
  in
  Alcotest.(check int) "sum" 5 sum;
  close client_conn;
  Thread.join t;
  close peer_conn

let test_receive_response () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        (* Register stream before handshake to avoid race (see
           test_request_response comment for details). *)
        peer_conn.connection_state <- Client;
        let ch = new_stream peer_conn ~role:"RR" () in
        Test_helpers.handshake_via_stream peer_conn;
        try
          while true do
            let msg_id, _message = receive_request ch in
            send_response_value ch msg_id (`Int 42)
          done
        with Failure _ -> ())
      ()
  in
  ignore (send_handshake client_conn);
  let ch = connect_stream client_conn 3l () in
  let msg_id = send_request ch (`Map [ (`Text "test", `Bool true) ]) in
  let result = receive_response ch msg_id in
  Alcotest.(check int) "result" 42 (Cbor_helpers.extract_int result);
  close client_conn;
  Thread.join t;
  close peer_conn

(* ---- PendingRequest caching ---- *)

let test_pending_request_caching () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        peer_conn.connection_state <- Client;
        let ch = new_stream peer_conn ~role:"PR" () in
        Test_helpers.handshake_via_stream peer_conn;
        try
          while true do
            let msg_id, message = receive_request ch in
            let v =
              Cbor_helpers.extract_int
                (List.Assoc.find_exn
                   (Cbor_helpers.extract_dict message)
                   ~equal:Poly.( = ) (`Text "value"))
            in
            send_response_value ch msg_id (`Int (v * 2))
          done
        with Failure _ -> ())
      ()
  in
  ignore (send_handshake client_conn);
  let ch = connect_stream client_conn 3l () in
  let pending = request ch (`Map [ (`Text "value", `Int 21) ]) in
  let r1 = pending_get pending in
  Alcotest.(check int) "first get" 42 (Cbor_helpers.extract_int r1);
  let r2 = pending_get pending in
  Alcotest.(check int) "second get (cached)" 42 (Cbor_helpers.extract_int r2);
  close client_conn;
  Thread.join t;
  close peer_conn

(* ---- RequestError ---- *)

let test_request_error () =
  let body =
    `Map
      [
        (`Text "error", `Text "something went wrong");
        (`Text "type", `Text "TestError");
        (`Text "extra", `Text "data");
      ]
  in
  let raised = ref false in
  (try ignore (result_or_error body)
   with Request_error e ->
     raised := true;
     Alcotest.(check string) "message" "something went wrong" e.message;
     Alcotest.(check string) "error_type" "TestError" e.error_type);
  Alcotest.(check bool) "raised" true !raised

let test_result_or_error_returns_result () =
  let body = `Map [ (`Text "result", `Int 42) ] in
  let result = result_or_error body in
  Alcotest.(check int) "result" 42 (Cbor_helpers.extract_int result)

let test_result_or_error_raises () =
  let body =
    `Map [ (`Text "error", `Text "bad"); (`Text "type", `Text "TestError") ]
  in
  let raised = ref false in
  (try ignore (result_or_error body)
   with Request_error e ->
     raised := true;
     Alcotest.(check string) "msg" "bad" e.message);
  Alcotest.(check bool) "raised" true !raised

(* ---- Duplicate response ---- *)

let test_duplicate_response_error () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let ch = control_stream conn in
  (* Put a response in the responses dict directly *)
  Hashtbl.set ch.responses ~key:42l ~data:"first";
  (* Now try to process another reply with same ID *)
  Queue.enqueue ch.inbox.queue
    (Pkt
       { stream_id = 0l; message_id = 42l; is_reply = true; payload = "second" });
  let raised = ref false in
  (try process_one_message ch
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "got two responses" true
       (contains_substring msg "Got two responses"));
  Alcotest.(check bool) "raised" true !raised;
  close conn

(* ---- Shutdown in inbox ---- *)

let test_shutdown_in_inbox () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let ch = control_stream conn in
  Queue.enqueue ch.inbox.queue Shutdown;
  let raised = ref false in
  (try ignore (receive_request ch)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "connection closed" true
       (contains_substring msg "Connection closed"));
  Alcotest.(check bool) "raised" true !raised;
  close conn

(* ---- Message to nonexistent stream ---- *)

let test_message_to_nonexistent_stream () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  (* Send a request to a stream that doesn't exist on the peer *)
  send_packet client_conn
    {
      stream_id = 999l;
      message_id = 1l;
      is_reply = false;
      payload = CBOR.Simple.encode (`Map [ (`Text "command", `Text "test") ]);
    };
  (* Also send a control stream message so the peer processes packets *)
  ignore (send_request_raw (control_stream client_conn) "ping");
  ignore (receive_request_raw (control_stream peer_conn));
  close peer_conn;
  close client_conn

(* ---- Message to dead stream ---- *)

let test_message_to_dead_stream () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch_client = new_stream client_conn () in
  let ch_peer = connect_stream peer_conn ch_client.stream_id () in
  close_stream ch_client;
  (* Give time for close to be received *)
  Caml_unix.sleepf 0.1;
  (* Now send a request to the dead stream from peer *)
  ignore (send_request ch_peer (`Map [ (`Text "test", `Text "data") ]));
  Caml_unix.sleepf 0.1;
  close peer_conn;
  close client_conn

(* ---- Close stream creates dead stream in debug mode ---- *)

let test_close_stream_creates_dead_stream () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  let peer_done = ref false in
  let t =
    Thread.create
      (fun () ->
        Test_helpers.handshake_via_stream peer_conn;
        let ch = control_stream peer_conn in
        let msg_id, _ = receive_request ch in
        send_response_value ch msg_id (`Text "Ok");
        peer_done := true)
      ()
  in
  ignore (send_handshake client_conn);
  let client_ch = new_stream client_conn ~role:"ToClose" () in
  close_stream client_ch;
  let result = pending_get (request (control_stream client_conn) (`Map [])) in
  ignore result;
  Thread.join t;
  (* The stream should be dead on peer side *)
  (match Hashtbl.find peer_conn.streams client_ch.stream_id with
  | Some (Dead _) -> Alcotest.(check bool) "is dead" true true
  | Some (Live _) -> Alcotest.fail "expected dead stream"
  | None -> Alcotest.fail "expected stream entry");
  close client_conn;
  close peer_conn

let test_close_stream_creates_dead_stream_with_connect () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  let t =
    Thread.create
      (fun () ->
        Test_helpers.handshake_via_stream peer_conn;
        let ch = control_stream peer_conn in
        let msg_id, msg = receive_request ch in
        let stream_id =
          Cbor_helpers.extract_int
            (List.Assoc.find_exn
               (Cbor_helpers.extract_dict msg)
               ~equal:Poly.( = ) (`Text "stream"))
        in
        ignore
          (connect_stream peer_conn
             (Int32.of_int_exn stream_id)
             ~role:"Hello" ());
        send_response_value ch msg_id (`Text "Ok");
        let msg_id2, _ = receive_request ch in
        send_response_value ch msg_id2 (`Text "Ok"))
      ()
  in
  ignore (send_handshake client_conn);
  let client_ch = new_stream client_conn ~role:"ToClose" () in
  let r1 =
    pending_get
      (request
         (control_stream client_conn)
         (`Map [ (`Text "stream", `Int (Int32.to_int_exn client_ch.stream_id)) ]))
  in
  ignore r1;
  close_stream client_ch;
  let r2 = pending_get (request (control_stream client_conn) (`Map [])) in
  ignore r2;
  Thread.join t;
  (match Hashtbl.find peer_conn.streams client_ch.stream_id with
  | Some (Dead d) ->
      Alcotest.(check bool) "is dead" true true;
      Alcotest.(check bool)
        "name has Hello" true
        (contains_substring d.name "Hello")
  | Some (Live _) -> Alcotest.fail "expected dead stream"
  | None -> Alcotest.fail "expected stream entry");
  close client_conn;
  close peer_conn

(* ---- Reader loop clean exit ---- *)

let test_reader_loop_clean_exit () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch_client = new_stream client_conn ~role:"Test" () in
  let _ch_peer = connect_stream peer_conn ch_client.stream_id ~role:"Test" () in
  (* Send a packet from client *)
  ignore (send_request ch_client (`Map [ (`Text "test", `Text "data") ]));
  (* Background reader handles message dispatch automatically *)
  Caml_unix.sleepf 0.1;
  close client_conn;
  close peer_conn

(* ---- Packet dispatch to reply inbox ---- *)

let test_process_reply_packet () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let ch = control_stream conn in
  Queue.enqueue ch.inbox.queue
    (Pkt { stream_id = 0l; message_id = 7l; is_reply = true; payload = "ok" });
  process_one_message ch;
  let resp = receive_response_raw ch 7l in
  Alcotest.(check string) "response" "ok" resp;
  close conn

let test_process_request_packet () =
  let s1, _s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  let ch = control_stream conn in
  Queue.enqueue ch.inbox.queue
    (Pkt { stream_id = 0l; message_id = 3l; is_reply = false; payload = "req" });
  let msg_id, payload = receive_request_raw ch in
  Alcotest.(check int32) "msg_id" 3l msg_id;
  Alcotest.(check string) "payload" "req" payload;
  close conn

(* ---- Debug mode packet handling ---- *)

let test_debug_mode_recv () =
  let s1, s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"DebugTest" ~debug:true () in
  write_packet s2
    { stream_id = 0l; message_id = 1l; is_reply = false; payload = "hello" };
  Caml_unix.sleepf 0.1;
  close conn

let test_debug_close_stream_recv () =
  let s1, s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"DebugClose" ~debug:true () in
  (* Send a close stream packet for an unknown stream *)
  write_packet s2
    {
      stream_id = 999l;
      message_id = close_stream_message_id;
      is_reply = false;
      payload = close_stream_payload;
    };
  (* Background reader processes the packet automatically *)
  Caml_unix.sleepf 0.1;
  (* Check it created a dead stream *)
  (match Hashtbl.find conn.streams 999l with
  | Some (Dead d) ->
      Alcotest.(check bool) "dead stream" true true;
      Alcotest.(check string) "name" "Never opened!" d.name
  | _ -> Alcotest.fail "expected dead stream for unknown close");
  close conn

(* ---- Message to reply on nonexistent stream ---- *)

let test_reply_to_nonexistent_ignored () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  (* Send a reply to a stream that doesn't exist — should not send back error *)
  send_packet client_conn
    {
      stream_id = 999l;
      message_id = 1l;
      is_reply = true;
      payload = "reply_data";
    };
  (* Send another message on control so we can process *)
  ignore (send_request_raw (control_stream client_conn) "ping");
  ignore (receive_request_raw (control_stream peer_conn));
  close peer_conn;
  close client_conn

(* ---- send_request_raw increments message IDs ---- *)

let test_message_id_increments () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"Test" () in
  let id1 = send_request_raw ch "a" in
  let id2 = send_request_raw ch "b" in
  let id3 = send_request_raw ch "c" in
  Alcotest.(check int32) "id1" 1l id1;
  Alcotest.(check int32) "id2" 2l id2;
  Alcotest.(check int32) "id3" 3l id3;
  close peer_conn;
  close client_conn

(* ---- result_or_error edge cases ---- *)

let test_result_or_error_error_without_type () =
  (* Error response missing "type" key -> find_text returns "" *)
  let body = `Map [ (`Text "error", `Text "oops") ] in
  let raised = ref false in
  (try ignore (result_or_error body)
   with Request_error e ->
     raised := true;
     Alcotest.(check string) "message" "oops" e.message;
     Alcotest.(check string) "error_type empty" "" e.error_type);
  Alcotest.(check bool) "raised" true !raised

let test_result_or_error_neither () =
  (* Response with neither "result" nor "error" *)
  let body = `Map [ (`Text "other", `Int 99) ] in
  let raised = ref false in
  (try ignore (result_or_error body)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'neither'" true
       (contains_substring msg "neither"));
  Alcotest.(check bool) "raised" true !raised

(* ---- entry_name ---- *)

let test_entry_name_live_stream () =
  Printf.eprintf "[hegel-debug] entry_name_live: creating socketpair\n%!";
  let s1, s2 = make_socket_pair () in
  Printf.eprintf "[hegel-debug] entry_name_live: creating peer_conn\n%!";
  let peer_conn = make_connection s1 ~name:"Peer" () in
  Printf.eprintf "[hegel-debug] entry_name_live: creating client_conn\n%!";
  let client_conn = make_connection s2 ~name:"Client" () in
  Printf.eprintf "[hegel-debug] entry_name_live: handshake_pair\n%!";
  handshake_pair peer_conn client_conn;
  Printf.eprintf "[hegel-debug] entry_name_live: handshake done\n%!";
  let ch = new_stream client_conn ~role:"LiveRole" () in
  let name = entry_name client_conn ch.stream_id in
  Alcotest.(check bool) "has LiveRole" true (contains_substring name "LiveRole");
  Printf.eprintf "[hegel-debug] entry_name_live: closing client_conn\n%!";
  close client_conn;
  Printf.eprintf "[hegel-debug] entry_name_live: closing peer_conn\n%!";
  close peer_conn;
  Printf.eprintf "[hegel-debug] entry_name_live: done\n%!"

let test_entry_name_dead_stream () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"ForDead" () in
  close_stream ch;
  (* In debug mode, close_stream replaces with Dead entry *)
  let name = entry_name client_conn ch.stream_id in
  Alcotest.(check bool) "has ForDead" true (contains_substring name "ForDead");
  close client_conn;
  close peer_conn

(* ---- Debug close for already-dead stream ---- *)

let test_debug_close_already_dead_stream () =
  let s1, s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Server" ~debug:true () in
  (* Manually insert a Dead entry for stream 50 *)
  Hashtbl.set conn.streams ~key:50l
    ~data:(Dead { stream_id = 50l; name = "OldDead" });
  (* Send a close packet for stream 50 from the other end *)
  write_packet s2
    {
      stream_id = 50l;
      message_id = close_stream_message_id;
      is_reply = false;
      payload = close_stream_payload;
    };
  (* Background reader processes the packets automatically *)
  Caml_unix.sleepf 0.1;
  (match Hashtbl.find conn.streams 50l with
  | Some (Dead d) ->
      Alcotest.(check string) "dead name preserved" "OldDead" d.name
  | _ -> Alcotest.fail "expected Dead entry with preserved name");
  close conn;
  Unix.close s2

(* ---- Debug close for existing live stream ---- *)

let test_debug_close_existing_live_stream () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  handshake_pair peer_conn client_conn;
  let ch_client = new_stream client_conn ~role:"LiveCh" () in
  let _ch_peer =
    connect_stream peer_conn ch_client.stream_id ~role:"LiveChSrv" ()
  in
  (* Client closes the stream, which sends a close packet *)
  close_stream ch_client;
  (* Background reader processes the close packet in debug mode *)
  let ch_id = ch_client.stream_id in
  Caml_unix.sleepf 0.1;
  (* Check the dead entry has the Live stream's name *)
  (match Hashtbl.find peer_conn.streams ch_id with
  | Some (Dead d) ->
      Alcotest.(check bool)
        "dead name has LiveChSrv" true
        (contains_substring d.name "LiveChSrv")
  | _ -> Alcotest.fail "expected Dead entry");
  close client_conn;
  close peer_conn

(* ---- Message to dead stream in reader ---- *)

let test_message_to_dead_stream_in_reader () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  handshake_pair peer_conn client_conn;
  let ch_client = new_stream client_conn ~role:"WillDie" () in
  let ch_id = ch_client.stream_id in
  (* Manually insert a Dead entry on peer side *)
  Hashtbl.set peer_conn.streams ~key:ch_id
    ~data:(Dead { stream_id = ch_id; name = "DeadCh" });
  (* Send a non-reply packet to the dead stream from client side *)
  send_packet client_conn
    {
      stream_id = ch_id;
      message_id = 1l;
      is_reply = false;
      payload = CBOR.Simple.encode (`Map [ (`Text "test", `Text "data") ]);
    };
  (* Also send something to control so we can synchronize *)
  ignore (send_request_raw (control_stream client_conn) "sync");
  ignore (receive_request_raw (control_stream peer_conn));
  close peer_conn;
  close client_conn

(* ---- Close on socket where shutdown raises ---- *)

let test_close_with_shutdown_error () =
  (* Use a pipe fd - shutdown on a pipe raises ENOTSOCK *)
  let r, w = Unix.pipe () in
  let conn = make_connection r ~name:"Test" () in
  close conn;
  Alcotest.(check bool) "not live" false (is_live conn);
  Unix.close w

(* ---- close_stream when not registered ---- *)

let test_close_stream_not_registered () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" ~debug:true () in
  let client_conn = make_connection s2 ~name:"Client" ~debug:true () in
  handshake_pair peer_conn client_conn;
  let ch = new_stream client_conn ~role:"Unreg" () in
  (* Remove the stream from the hashtable before closing *)
  Hashtbl.remove client_conn.streams ch.stream_id;
  close_stream ch;
  Alcotest.(check bool) "is closed" true ch.closed;
  close client_conn;
  close peer_conn

(* ---- Short handshake response (< 6 bytes) ---- *)

let test_send_handshake_short_response () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        let ch = control_stream peer_conn in
        peer_conn.connection_state <- Client;
        let msg_id, _payload = receive_request_raw ch in
        send_response_raw ch msg_id "Hi")
      ()
  in
  let raised = ref false in
  (try ignore (send_handshake client_conn)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "bad handshake" true
       (contains_substring msg "Bad handshake"));
  Thread.join t;
  Alcotest.(check bool) "raised" true !raised;
  close client_conn;
  close peer_conn

(* ---- Wrong prefix handshake response (>= 6 bytes, not "Hegel/") ---- *)

let test_send_handshake_wrong_prefix () =
  let s1, s2 = make_socket_pair () in
  let peer_conn = make_connection s1 ~name:"Peer" () in
  let client_conn = make_connection s2 ~name:"Client" () in
  let t =
    Thread.create
      (fun () ->
        let ch = control_stream peer_conn in
        peer_conn.connection_state <- Client;
        let msg_id, _payload = receive_request_raw ch in
        send_response_raw ch msg_id "WrongPrefix/1.0")
      ()
  in
  let raised = ref false in
  (try ignore (send_handshake client_conn)
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "bad handshake wrong prefix" true
       (contains_substring msg "Bad handshake"));
  Thread.join t;
  Alcotest.(check bool) "raised" true !raised;
  close client_conn;
  close peer_conn

(** Test: control_stream raises when stream 0 is not Live. *)
let test_control_stream_failure () =
  let s1, s2 = make_socket_pair () in
  let conn = make_connection s1 ~name:"Test" () in
  Hashtbl.set conn.streams ~key:0l
    ~data:(Dead { stream_id = 0l; name = "Control" });
  let raised = ref false in
  (try ignore (control_stream conn) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised;
  close conn;
  Unix.close s2

let tests =
  [
    (* Connection lifecycle *)
    Alcotest.test_case "connection live" `Quick test_connection_live;
    Alcotest.test_case "connection double close" `Quick
      test_connection_double_close;
    (* Handshake *)
    Alcotest.test_case "handshake returns version" `Quick
      test_send_handshake_returns_version;
    Alcotest.test_case "double handshake send raises" `Quick
      test_double_handshake_send_raises;
    Alcotest.test_case "bad handshake response" `Quick
      test_send_handshake_bad_response;
    (* Stream allocation *)
    Alcotest.test_case "new_stream before handshake raises" `Quick
      test_new_stream_before_handshake_raises;
    Alcotest.test_case "connect_stream before handshake raises" `Quick
      test_connect_stream_before_handshake_raises;
    Alcotest.test_case "connect_stream already exists raises" `Quick
      test_connect_stream_already_exists_raises;
    Alcotest.test_case "client stream IDs are odd" `Quick
      test_stream_ids_are_odd_for_client;
    (* Stream operations *)
    Alcotest.test_case "stream close" `Quick test_stream_close;
    Alcotest.test_case "stream close when conn not live" `Quick
      test_stream_close_when_connection_not_live;
    Alcotest.test_case "process message when closed" `Quick
      test_stream_process_message_when_closed;
    (* Stream repr/name *)
    Alcotest.test_case "stream repr" `Quick test_stream_repr;
    Alcotest.test_case "stream repr no role" `Quick test_stream_repr_no_role;
    Alcotest.test_case "stream repr with role" `Quick test_stream_repr_with_role;
    Alcotest.test_case "stream name no role no conn name" `Quick
      test_stream_name_no_role_no_conn_name;
    Alcotest.test_case "stream name with role no conn name" `Quick
      test_stream_name_with_role_no_conn_name;
    Alcotest.test_case "stream name no role with conn name" `Quick
      test_stream_name_no_role_with_conn_name;
    Alcotest.test_case "stream name control" `Quick test_stream_name_control;
    (* Request/response *)
    Alcotest.test_case "request response" `Quick test_request_response;
    Alcotest.test_case "receive_response" `Quick test_receive_response;
    Alcotest.test_case "pending request caching" `Quick
      test_pending_request_caching;
    (* RequestError *)
    Alcotest.test_case "request error" `Quick test_request_error;
    Alcotest.test_case "result_or_error returns result" `Quick
      test_result_or_error_returns_result;
    Alcotest.test_case "result_or_error raises" `Quick
      test_result_or_error_raises;
    (* Duplicate response *)
    Alcotest.test_case "duplicate response error" `Quick
      test_duplicate_response_error;
    (* Shutdown *)
    Alcotest.test_case "shutdown in inbox" `Quick test_shutdown_in_inbox;
    (* Nonexistent/dead stream *)
    Alcotest.test_case "message to nonexistent stream" `Quick
      test_message_to_nonexistent_stream;
    Alcotest.test_case "message to dead stream" `Quick
      test_message_to_dead_stream;
    Alcotest.test_case "close creates dead stream" `Quick
      test_close_stream_creates_dead_stream;
    Alcotest.test_case "close creates dead stream with connect" `Quick
      test_close_stream_creates_dead_stream_with_connect;
    (* Reader loop *)
    Alcotest.test_case "reader loop clean exit" `Quick
      test_reader_loop_clean_exit;
    (* Packet dispatch *)
    Alcotest.test_case "process reply packet" `Quick test_process_reply_packet;
    Alcotest.test_case "process request packet" `Quick
      test_process_request_packet;
    (* Debug mode *)
    Alcotest.test_case "debug mode recv" `Quick test_debug_mode_recv;
    Alcotest.test_case "debug close stream recv" `Quick
      test_debug_close_stream_recv;
    (* Reply to nonexistent *)
    Alcotest.test_case "reply to nonexistent ignored" `Quick
      test_reply_to_nonexistent_ignored;
    (* Message ID increments *)
    Alcotest.test_case "message ID increments" `Quick test_message_id_increments;
    (* result_or_error edge cases *)
    Alcotest.test_case "error without type" `Quick
      test_result_or_error_error_without_type;
    Alcotest.test_case "neither result nor error" `Quick
      test_result_or_error_neither;
    (* entry_name *)
    Alcotest.test_case "entry_name live stream" `Quick
      test_entry_name_live_stream;
    Alcotest.test_case "entry_name dead stream" `Quick
      test_entry_name_dead_stream;
    (* Debug close for already-dead stream *)
    Alcotest.test_case "debug close already dead stream" `Quick
      test_debug_close_already_dead_stream;
    (* Debug close existing live stream *)
    Alcotest.test_case "debug close existing live stream" `Quick
      test_debug_close_existing_live_stream;
    (* Message to dead stream in reader *)
    Alcotest.test_case "message to dead stream in reader" `Quick
      test_message_to_dead_stream_in_reader;
    (* Close with shutdown error *)
    Alcotest.test_case "close with shutdown error" `Quick
      test_close_with_shutdown_error;
    (* close_stream not registered *)
    Alcotest.test_case "close_stream not registered" `Quick
      test_close_stream_not_registered;
    (* Short handshake response *)
    Alcotest.test_case "short handshake response" `Quick
      test_send_handshake_short_response;
    Alcotest.test_case "wrong prefix handshake response" `Quick
      test_send_handshake_wrong_prefix;
    (* control_stream failure *)
    Alcotest.test_case "control_stream failure" `Quick
      test_control_stream_failure;
  ]
