(** Multiplexed connection and stream abstractions for Hegel.

    This module implements a background-reader model: a dedicated thread reads
    packets from the input stream and dispatches them to per-stream inboxes.
    Streams wait on condition variables for new messages.

    {b Connection} manages a pair of file descriptors (one for reading, one for
    writing) with multiplexed logical streams. {b Stream} provides
    request/response messaging over a single logical stream. *)

open! Core
module Unix = Core_unix
module Mutex = Caml_threads.Mutex
module Condition = Caml_threads.Condition
module Thread = Caml_threads.Thread
open Protocol

(* Ignore SIGPIPE so that writes to broken sockets raise EPIPE instead of
   killing the process. This is standard practice for network programs and is
   required because background reader threads may attempt error-reply writes
   to sockets whose remote end has already been closed. *)
let () = Stdlib.Sys.set_signal Stdlib.Sys.sigpipe Stdlib.Sys.Signal_ignore

(** Handshake string sent by the client to initiate the protocol. *)
let handshake_string = "hegel_handshake_start"

(** Message used when the server has crashed. *)
let server_crashed_message =
  "The hegel server process has exited unexpectedly. Check .hegel/server.log \
   for details."

(** Sentinel value placed in a stream's inbox when the connection shuts down. *)
type inbox_item = Pkt of packet | Shutdown

exception
  Request_error of {
    message : string;
    error_type : string;
    data : (CBOR.Simple.t * CBOR.Simple.t) list;
  }
(** Error response from the peer. *)

(** [result_or_error body] extracts the ["result"] field from a CBOR map, or
    raises {!Request_error} if an ["error"] field is present. *)
let result_or_error body =
  let pairs = Cbor_helpers.extract_dict body in
  let find_text key =
    match List.Assoc.find pairs ~equal:Poly.( = ) (`Text key) with
    | Some v -> Cbor_helpers.extract_string v
    | None -> ""
  in
  if List.Assoc.mem pairs ~equal:Poly.( = ) (`Text "error") then begin
    let msg = find_text "error" in
    let error_type = find_text "type" in
    let data =
      List.filter pairs ~f:(fun (k, _) ->
          Poly.( <> ) k (`Text "error") && Poly.( <> ) k (`Text "type"))
    in
    raise (Request_error { message = msg; error_type; data })
  end
  else
    match List.Assoc.find pairs ~equal:Poly.( = ) (`Text "result") with
    | Some v -> v
    | None -> failwith "Response has neither 'result' nor 'error'"

(** Connection state after handshake. *)
type connection_state = Unresolved | Client

let equal_connection_state a b = Poly.( = ) a b

type stream_inbox = {
  queue : inbox_item Queue.t;
  lock : Mutex.t;
  cond : Condition.t;
}
(** Thread-safe inbox for a stream, with condition-variable signaling. *)

(** A stream entry in the connection's stream table. *)
type stream_entry =
  | Live of stream
  | Dead of { stream_id : int32; name : string }

and connection = {
  name : string option;
  read_fd : Unix.File_descr.t;
  write_fd : Unix.File_descr.t;
  mutable next_stream_id : int;
  streams : (Int32.t, stream_entry) Hashtbl.t;
  streams_lock : Mutex.t;
  mutable running : bool;
  debug : bool;
  writer_lock : Mutex.t;
  mutable connection_state : connection_state;
  mutable server_exited : bool;
  mutable reader_thread : Thread.t option;
}
(** Multiplexed connection to a Hegel peer. *)

and stream = {
  stream_id : int32;
  conn : connection;
  inbox : stream_inbox;
  requests : packet Queue.t;
  responses : (Int32.t, string) Hashtbl.t;
  role : string option;
  mutable next_message_id : int32;
  mutable closed : bool;
}
(** Logical stream for request/response messaging. *)

(** [stream_id ch] returns the stream ID of [ch]. *)
let stream_id ch = ch.stream_id

(** [stream_name ch] returns a human-readable name for stream [ch]. *)
let stream_name ch =
  match (ch.role, ch.conn.name) with
  | None, None -> sprintf "Stream %ld" ch.stream_id
  | None, Some n -> sprintf "%s stream [id=%ld]" n ch.stream_id
  | Some r, None -> sprintf "stream [id=%ld] (%s)" ch.stream_id r
  | Some r, Some n -> sprintf "%s stream [id=%ld] (%s)" n ch.stream_id r

(** [stream_repr ch] returns a debug representation of stream [ch]. *)
let stream_repr ch =
  match ch.role with
  | None -> sprintf "Stream(%ld)" ch.stream_id
  | Some r -> sprintf "Stream(%ld, role=%s)" ch.stream_id r

(** [entry_name conn stream_id] returns the name of the stream entry for
    [stream_id] in [conn], or a default. *)
let entry_name conn stream_id =
  match Hashtbl.find conn.streams stream_id with
  | Some (Live ch) -> stream_name ch
  | Some (Dead d) -> d.name
  | None -> sprintf "stream %ld" stream_id

(** [make_stream conn stream_id ~role] creates a new stream. *)
let make_stream conn stream_id ~role =
  assert (Int32.( <> ) stream_id 0l || Poly.( = ) role (Some "Control"));
  {
    stream_id;
    conn;
    inbox =
      {
        queue = Queue.create ();
        lock = Mutex.create ();
        cond = Condition.create ();
      };
    requests = Queue.create ();
    responses = Hashtbl.create (module Int32);
    role;
    next_message_id = 1l;
    closed = false;
  }

(** [push_inbox ch item] pushes an item to a stream's inbox and signals the
    condition variable. Must be called without holding [ch.inbox.lock]. *)
let push_inbox ch item =
  Mutex.lock ch.inbox.lock;
  Queue.enqueue ch.inbox.queue item;
  Condition.signal ch.inbox.cond;
  Mutex.unlock ch.inbox.lock

(** [signal_all_streams conn] pushes {!Shutdown} into all live streams' inboxes.
    Must be called while holding [conn.streams_lock]. *)
let signal_all_streams conn =
  Hashtbl.iteri conn.streams ~f:(fun ~key:_ ~data:entry ->
      match entry with Live ch -> push_inbox ch Shutdown | Dead _ -> ())

(** [send_packet conn packet] sends a packet on the connection, thread-safe. *)
let send_packet conn packet =
  Mutex.lock conn.writer_lock;
  Exn.protect
    ~finally:(fun () -> Mutex.unlock conn.writer_lock)
    ~f:(fun () -> write_packet conn.write_fd packet)

(** Background reader thread function. Reads packets from [conn.read_fd] and
    dispatches them to the appropriate stream's inbox. Exits when the stream
    closes or an error occurs. *)
let reader_loop conn =
  let dispatch_packet (pkt : packet) =
    (* Handle close_stream_payload for ANY stream (even unregistered) *)
    if String.equal pkt.payload close_stream_payload then begin
      assert (Int32.equal pkt.message_id close_stream_message_id);
      if conn.debug then begin
        Mutex.lock conn.streams_lock;
        let prev_name =
          match Hashtbl.find conn.streams pkt.stream_id with
          | Some (Live ch) -> stream_name ch
          | Some (Dead d) -> d.name
          | None -> "Never opened!"
        in
        Hashtbl.set conn.streams ~key:pkt.stream_id
          ~data:(Dead { stream_id = pkt.stream_id; name = prev_name });
        Mutex.unlock conn.streams_lock
      end
    end
    else begin
      Mutex.lock conn.streams_lock;
      let entry = Hashtbl.find conn.streams pkt.stream_id in
      match entry with
      | Some (Live ch) ->
          Mutex.unlock conn.streams_lock;
          push_inbox ch (Pkt pkt)
      | (Some (Dead _) | None) as entry ->
          Mutex.unlock conn.streams_lock;
          if not pkt.is_reply then begin
            let disposition =
              match entry with Some (Dead _) -> "closed" | _ -> "non-existent"
            in
            let error_msg =
              sprintf "Message %ld sent to %s %s" pkt.message_id disposition
                (entry_name conn pkt.stream_id)
            in
            try
              send_packet conn
                {
                  stream_id = pkt.stream_id;
                  message_id = pkt.message_id;
                  is_reply = true;
                  payload =
                    CBOR.Simple.encode
                      (`Map [ (`Text "error", `Text error_msg) ]);
                }
            with _ -> ()
          end
    end
  in
  (try
     while conn.running do
       let pkt = read_packet conn.read_fd in
       dispatch_packet pkt
     done
   with _ ->
     (* Stream closed or error — mark server as exited *)
     conn.server_exited <- true);
  (* Signal shutdown to all streams *)
  Mutex.lock conn.streams_lock;
  signal_all_streams conn;
  Mutex.unlock conn.streams_lock

(** [close conn] closes the connection and signals all live streams. *)
let close conn =
  if not conn.running then ()
  else begin
    conn.running <- false;
    (* Shutdown the read fd to unblock any reader thread blocked in read().
       On Linux, close() alone does not wake up a blocked read() in another
       thread. shutdown() ensures the reader gets EOF immediately.
       Fails harmlessly with ENOTSOCK on pipes. *)
    let shutdown_ok =
      try
        Unix.shutdown conn.read_fd ~mode:Unix.SHUTDOWN_ALL;
        true
      with Unix.Unix_error _ -> false
    in
    (try Unix.close conn.read_fd with Unix.Unix_error _ -> ());
    (* Only close write_fd if it's different from read_fd *)
    (if not (phys_equal conn.write_fd conn.read_fd) then
       try Unix.close conn.write_fd with Unix.Unix_error _ -> ());
    Mutex.lock conn.streams_lock;
    signal_all_streams conn;
    Mutex.unlock conn.streams_lock;
    (* Join the reader thread to ensure it has fully exited before returning.
       Only safe when shutdown succeeded (sockets), since shutdown guarantees
       the blocked read returns immediately. On pipes, shutdown fails with
       ENOTSOCK and close alone may not unblock the reader thread. *)
    if shutdown_ok then Option.iter conn.reader_thread ~f:Thread.join
  end

(** [create_connection ~read_fd ~write_fd ?name ?debug ()] creates a new
    connection using separate file descriptors for reading and writing. A
    control stream (stream 0) is automatically created and a background reader
    thread is spawned. *)
let create_connection ~read_fd ~write_fd ?name ?(debug = false) () =
  let conn =
    {
      name;
      read_fd;
      write_fd;
      next_stream_id = 1;
      streams = Hashtbl.create (module Int32);
      streams_lock = Mutex.create ();
      running = true;
      debug;
      writer_lock = Mutex.create ();
      connection_state = Unresolved;
      server_exited = false;
      reader_thread = None;
    }
  in
  let control = make_stream conn 0l ~role:(Some "Control") in
  Mutex.lock conn.streams_lock;
  Hashtbl.set conn.streams ~key:0l ~data:(Live control);
  Mutex.unlock conn.streams_lock;
  (* Spawn background reader thread *)
  conn.reader_thread <- Some (Thread.create reader_loop conn);
  conn

(** [is_live conn] returns [true] if the connection is still active. *)
let is_live conn = conn.running

(** [server_has_exited conn] returns [true] if the server process has exited. *)
let server_has_exited conn = conn.server_exited

(** [control_stream conn] returns the control stream (stream 0). *)
let control_stream conn =
  match Hashtbl.find conn.streams 0l with
  | Some (Live ch) -> ch
  | _ -> failwith "Internal error: no control stream"

(** [new_stream conn ?role ()] creates a new logical stream on the connection.

    Client streams use odd IDs [(counter << 1) | 1], server streams use even IDs
    [(counter << 1)]. *)
let new_stream conn ?role () =
  if equal_connection_state conn.connection_state Unresolved then
    failwith "Cannot create a new stream before handshake has been performed."
  else begin
    let stream_id = Int32.of_int_exn ((conn.next_stream_id lsl 1) lor 1) in
    conn.next_stream_id <- conn.next_stream_id + 1;
    let ch = make_stream conn stream_id ~role in
    Mutex.lock conn.streams_lock;
    Hashtbl.set conn.streams ~key:stream_id ~data:(Live ch);
    Mutex.unlock conn.streams_lock;
    ch
  end

(** [connect_stream conn stream_id ?role ()] connects to a stream created by the
    peer. *)
let connect_stream conn stream_id ?role () =
  if equal_connection_state conn.connection_state Unresolved then
    failwith "Cannot create a new stream before handshake has been performed.";
  if Hashtbl.mem conn.streams stream_id then
    failwith (sprintf "Stream already connected as %ld." stream_id);
  let ch = make_stream conn stream_id ~role in
  Mutex.lock conn.streams_lock;
  Hashtbl.set conn.streams ~key:stream_id ~data:(Live ch);
  Mutex.unlock conn.streams_lock;
  ch

(* --- Stream operations --- *)

(** [pop_inbox_item ch] waits for an item to appear in the stream's inbox.
    Blocks until a packet arrives or the connection shuts down. *)
let pop_inbox_item ch =
  Mutex.lock ch.inbox.lock;
  let rec wait () =
    if not (Queue.is_empty ch.inbox.queue) then begin
      let item = Queue.dequeue_exn ch.inbox.queue in
      Mutex.unlock ch.inbox.lock;
      item
    end
    else if ch.closed then begin
      Mutex.unlock ch.inbox.lock;
      raise (Failure (sprintf "%s is closed" (stream_name ch)))
    end
    else begin
      (* Condition.wait atomically releases the lock and blocks until
         signaled, then re-acquires the lock. push_inbox signals on every
         enqueue; signal_all_streams pushes Shutdown on connection close. *)
      Condition.wait ch.inbox.cond ch.inbox.lock;
      wait ()
    end
  in
  wait ()

(** [process_one_message ch] reads and routes one incoming message for stream
    [ch]. Dispatches replies to [ch.responses] and requests to [ch.requests]. *)
let process_one_message ch =
  let item = pop_inbox_item ch in
  match item with
  | Shutdown -> raise (Failure "Connection closed")
  | Pkt pkt ->
      if pkt.is_reply then begin
        if Hashtbl.mem ch.responses pkt.message_id then
          failwith
            (sprintf "Got two responses for message ID %ld" pkt.message_id)
        else Hashtbl.set ch.responses ~key:pkt.message_id ~data:pkt.payload
      end
      else Queue.enqueue ch.requests pkt

(** [send_request_raw ch payload] sends raw bytes as a request and returns the
    message ID for matching the response. *)
let send_request_raw ch payload =
  let message_id = ch.next_message_id in
  ch.next_message_id <- Int32.( + ) ch.next_message_id 1l;
  send_packet ch.conn
    { payload; stream_id = ch.stream_id; is_reply = false; message_id };
  message_id

(** [send_request ch message] sends a CBOR-encoded request and returns the
    message ID. *)
let send_request ch message = send_request_raw ch (CBOR.Simple.encode message)

(** [receive_response_raw ch message_id] waits for raw response bytes to a
    request with the given [message_id]. *)
let receive_response_raw ch message_id =
  while not (Hashtbl.mem ch.responses message_id) do
    process_one_message ch
  done;
  let result = Hashtbl.find_exn ch.responses message_id in
  Hashtbl.remove ch.responses message_id;
  result

(** [receive_response ch message_id] waits for and decodes a response,
    extracting the result or raising {!Request_error}. *)
let receive_response ch message_id =
  let raw = receive_response_raw ch message_id in
  result_or_error (Cbor_helpers.decode raw)

(** [receive_request_raw ch] receives raw request bytes and returns
    [(message_id, payload)]. *)
let receive_request_raw ch =
  while Queue.is_empty ch.requests do
    process_one_message ch
  done;
  let pkt = Queue.dequeue_exn ch.requests in
  (pkt.message_id, pkt.payload)

(** [receive_request ch] receives and CBOR-decodes a request, returning
    [(message_id, decoded_value)]. *)
let receive_request ch =
  let message_id, body = receive_request_raw ch in
  (message_id, Cbor_helpers.decode body)

(** [send_response_raw ch message_id payload] sends raw bytes as a reply. *)
let send_response_raw ch message_id payload =
  send_packet ch.conn
    { payload; stream_id = ch.stream_id; is_reply = true; message_id }

(** [send_response_value ch message_id value] sends a success response with
    [value] wrapped as [{"result": value}]. *)
let send_response_value ch message_id value =
  send_response_raw ch message_id
    (CBOR.Simple.encode (`Map [ (`Text "result", value) ]))

(** [close_stream ch] closes the stream and notifies the peer. Idempotent. *)
let close_stream ch =
  if ch.closed then ()
  else begin
    let is_registered =
      match Hashtbl.find ch.conn.streams ch.stream_id with
      | Some (Live ch2) -> phys_equal ch2 ch
      | _ -> false
    in
    if not is_registered then ch.closed <- true
    else begin
      ch.closed <- true;
      if ch.conn.debug then
        Hashtbl.set ch.conn.streams ~key:ch.stream_id
          ~data:(Dead { name = stream_name ch; stream_id = ch.stream_id });
      if is_live ch.conn then
        send_packet ch.conn
          {
            payload = close_stream_payload;
            message_id = close_stream_message_id;
            stream_id = ch.stream_id;
            is_reply = false;
          }
    end
  end

type pending_request = {
  pr_stream : stream;
  pr_message_id : int32;
  mutable pr_value : CBOR.Simple.t option;
}
(** Pending request handle that caches the decoded response. *)

(** [request ch message] sends a CBOR request and returns a {!pending_request}
    handle. *)
let request ch message =
  let message_id = send_request ch message in
  { pr_stream = ch; pr_message_id = message_id; pr_value = None }

(** [pending_get pr] blocks until the response arrives and returns the result.
    Caches the response so subsequent calls return the same value or raise the
    same error. *)
let pending_get pr =
  match pr.pr_value with
  | Some v -> result_or_error v
  | None ->
      let raw = receive_response_raw pr.pr_stream pr.pr_message_id in
      let v = Cbor_helpers.decode raw in
      pr.pr_value <- Some v;
      result_or_error v

(* --- Handshake --- *)

(** [send_handshake conn] initiates the handshake as a client. Returns the
    server protocol version string (e.g. ["0.1"]). *)
let send_handshake conn =
  if not (equal_connection_state conn.connection_state Unresolved) then
    failwith "Handshake already established";
  conn.connection_state <- Client;
  let ch = control_stream conn in
  let message_id = send_request_raw ch handshake_string in
  let response = receive_response_raw ch message_id in
  if
    String.length response < 6
    || not (String.equal (String.sub response ~pos:0 ~len:6) "Hegel/")
  then failwith (sprintf "Bad handshake response: %S" response)
  else String.sub response ~pos:6 ~len:(String.length response - 6)
