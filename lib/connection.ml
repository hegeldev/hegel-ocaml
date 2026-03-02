(** Multiplexed connection and channel abstractions for the Hegel SDK.

    This module implements the demand-driven reader model: when a channel needs
    a message, it calls {!run_reader} which reads packets from the socket and
    dispatches them to the correct channel's inbox until the caller's condition
    is satisfied.

    {b Connection} manages a Unix socket with multiplexed logical channels.
    {b Channel} provides request/response messaging over a single logical
    channel. *)

open Protocol

(** Protocol version sent during handshake. *)
let protocol_version = "0.3"

(** Handshake string sent by the client to initiate the protocol. *)
let handshake_string = "hegel_handshake_start"

(** Default timeout in seconds for channel operations. *)
let channel_timeout = 30.0

(** Sentinel value placed in a channel's inbox when the connection shuts down.
*)
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
    match List.assoc_opt (`Text key) pairs with
    | Some v -> Cbor_helpers.extract_string v
    | None -> ""
  in
  if List.mem_assoc (`Text "error") pairs then begin
    let msg = find_text "error" in
    let error_type = find_text "type" in
    let data =
      List.filter (fun (k, _) -> k <> `Text "error" && k <> `Text "type") pairs
    in
    raise (Request_error { message = msg; error_type; data })
  end
  else
    match List.assoc_opt (`Text "result") pairs with
    | Some v -> v
    | None -> failwith "Response has neither 'result' nor 'error'"

(** Connection state after handshake. *)
type connection_state = Unresolved | Client | Server

(** A channel entry in the connection's channel table. *)
type channel_entry =
  | Live of channel
  | Dead of { channel_id : int32; name : string }

and connection = {
  name : string option;
  socket : Unix.file_descr;
  mutable next_channel_id : int;
  channels : (int32, channel_entry) Hashtbl.t;
  mutable running : bool;
  debug : bool;
  writer_lock : Mutex.t;
  reader_lock : Mutex.t;
  mutable connection_state : connection_state;
}
(** Multiplexed socket connection to a Hegel peer. *)

and channel = {
  channel_id : int32;
  conn : connection;
  inbox : inbox_item Queue.t;
  requests : packet Queue.t;
  responses : (int32, string) Hashtbl.t;
  role : string option;
  mutable next_message_id : int32;
  mutable closed : bool;
}
(** Logical channel for request/response messaging. *)

(** [channel_id ch] returns the channel ID of [ch]. *)
let channel_id ch = ch.channel_id

(** [channel_name ch] returns a human-readable name for channel [ch]. *)
let channel_name ch =
  match (ch.role, ch.conn.name) with
  | None, None -> Printf.sprintf "Channel %ld" ch.channel_id
  | None, Some n -> Printf.sprintf "%s channel [id=%ld]" n ch.channel_id
  | Some r, None -> Printf.sprintf "channel [id=%ld] (%s)" ch.channel_id r
  | Some r, Some n ->
      Printf.sprintf "%s channel [id=%ld] (%s)" n ch.channel_id r

(** [channel_repr ch] returns a debug representation of channel [ch]. *)
let channel_repr ch =
  match ch.role with
  | None -> Printf.sprintf "Channel(%ld)" ch.channel_id
  | Some r -> Printf.sprintf "Channel(%ld, role=%s)" ch.channel_id r

(** [entry_name conn channel_id] returns the name of the channel entry for
    [channel_id] in [conn], or a default. *)
let entry_name conn channel_id =
  match Hashtbl.find_opt conn.channels channel_id with
  | Some (Live ch) -> channel_name ch
  | Some (Dead d) -> d.name
  | None -> Printf.sprintf "channel %ld" channel_id

(** [make_channel conn channel_id ~role] creates a new channel. *)
let make_channel conn channel_id ~role =
  assert (channel_id <> 0l || role = Some "Control");
  {
    channel_id;
    conn;
    inbox = Queue.create ();
    requests = Queue.create ();
    responses = Hashtbl.create 8;
    role;
    next_message_id = 1l;
    closed = false;
  }

(** [send_packet conn packet] sends a packet on the connection, thread-safe. *)
let send_packet conn packet =
  Mutex.lock conn.writer_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock conn.writer_lock)
    (fun () -> write_packet conn.socket packet)

(** [read_packet_with_timeout sock timeout] reads a packet with a socket
    timeout. Returns [Some packet] on success, [None] on timeout. *)
let read_packet_with_timeout sock timeout =
  Unix.setsockopt_float sock Unix.SO_RCVTIMEO timeout;
  match read_packet sock with
  | pkt ->
      Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.0;
      Some pkt
  | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
      Unix.setsockopt_float sock Unix.SO_RCVTIMEO 0.0;
      None

(** [run_reader conn ~until] reads packets from the socket and dispatches them
    to channel inboxes until [until ()] returns [true].

    Uses a reader lock so only one caller reads at a time. If the lock is held
    by another caller, polls until it becomes available or [until] is satisfied.
*)
let run_reader conn ~until =
  if until () then ()
  else begin
    let acquired = ref false in
    Fun.protect
      ~finally:(fun () -> if !acquired then Mutex.unlock conn.reader_lock)
      (fun () ->
        (* Try to acquire the reader lock, polling if busy *)
        let got_lock = ref false in
        while not !got_lock do
          if Mutex.try_lock conn.reader_lock then begin
            acquired := true;
            got_lock := true
          end
          else if until () then got_lock := true (* exit without lock *)
          else Unix.sleepf 0.001
        done;
        if !acquired then begin
          while conn.running && not (until ()) do
            match read_packet_with_timeout conn.socket 0.1 with
            | None -> ()
            | Some pkt ->
                if pkt.payload = close_channel_payload then begin
                  (* Handle channel close *)
                  assert (pkt.message_id = close_channel_message_id);
                  if conn.debug then begin
                    let prev_name =
                      match Hashtbl.find_opt conn.channels pkt.channel_id with
                      | Some (Live ch) -> channel_name ch
                      | Some (Dead d) -> d.name
                      | None -> "Never opened!"
                    in
                    Hashtbl.replace conn.channels pkt.channel_id
                      (Dead { channel_id = pkt.channel_id; name = prev_name })
                  end
                end
                else begin
                  match Hashtbl.find_opt conn.channels pkt.channel_id with
                  | Some (Live ch) -> Queue.push (Pkt pkt) ch.inbox
                  | (Some (Dead _) | None) as entry ->
                      if not pkt.is_reply then begin
                        let disposition =
                          match entry with
                          | Some (Dead _) -> "closed"
                          | _ -> "non-existent"
                        in
                        let error_msg =
                          Printf.sprintf "Message %ld sent to %s %s"
                            pkt.message_id disposition
                            (entry_name conn pkt.channel_id)
                        in
                        send_packet conn
                          {
                            channel_id = pkt.channel_id;
                            message_id = pkt.message_id;
                            is_reply = true;
                            payload =
                              CBOR.Simple.encode
                                (`Map [ (`Text "error", `Text error_msg) ]);
                          }
                      end
                end
          done
        end)
  end

(** [close conn] closes the connection and signals all live channels. *)
let close conn =
  if not conn.running then ()
  else begin
    conn.running <- false;
    (try Unix.shutdown conn.socket Unix.SHUTDOWN_ALL
     with Unix.Unix_error _ -> ());
    Unix.close conn.socket;
    Hashtbl.iter
      (fun _id entry ->
        match entry with
        | Live ch -> Queue.push Shutdown ch.inbox
        | Dead _ -> ())
      conn.channels
  end

(** [create_connection sock ?name ?debug ()] creates a new connection wrapping
    the Unix file descriptor [sock]. A control channel (channel 0) is
    automatically created. *)
let create_connection sock ?name ?(debug = false) () =
  let conn =
    {
      name;
      socket = sock;
      next_channel_id = 1;
      channels = Hashtbl.create 16;
      running = true;
      debug;
      writer_lock = Mutex.create ();
      reader_lock = Mutex.create ();
      connection_state = Unresolved;
    }
  in
  let control = make_channel conn 0l ~role:(Some "Control") in
  Mutex.lock conn.writer_lock;
  Hashtbl.replace conn.channels 0l (Live control);
  Mutex.unlock conn.writer_lock;
  conn

(** [is_live conn] returns [true] if the connection is still active. *)
let is_live conn = conn.running

(** [control_channel conn] returns the control channel (channel 0). *)
let control_channel conn =
  match Hashtbl.find_opt conn.channels 0l with
  | Some (Live ch) -> ch
  | _ -> failwith "Internal error: no control channel"

(** [new_channel conn ?role ()] creates a new logical channel on the connection.

    The first call returns channel 0 (control). After handshake, client channels
    use odd IDs [(counter << 1) | 1], server channels use even IDs
    [(counter << 1)]. *)
let new_channel conn ?role () =
  if conn.connection_state = Unresolved then
    failwith "Cannot create a new channel before handshake has been performed."
  else begin
    let parity = if conn.connection_state = Client then 1 else 0 in
    let channel_id = Int32.of_int ((conn.next_channel_id lsl 1) lor parity) in
    conn.next_channel_id <- conn.next_channel_id + 1;
    let ch = make_channel conn channel_id ~role in
    Mutex.lock conn.writer_lock;
    Hashtbl.replace conn.channels channel_id (Live ch);
    Mutex.unlock conn.writer_lock;
    ch
  end

(** [connect_channel conn channel_id ?role ()] connects to a channel created by
    the peer. *)
let connect_channel conn channel_id ?role () =
  if conn.connection_state = Unresolved then
    failwith "Cannot create a new channel before handshake has been performed.";
  if Hashtbl.mem conn.channels channel_id then
    failwith (Printf.sprintf "Channel already connected as %ld." channel_id);
  let ch = make_channel conn channel_id ~role in
  Mutex.lock conn.writer_lock;
  Hashtbl.replace conn.channels channel_id (Live ch);
  Mutex.unlock conn.writer_lock;
  ch

(* --- Channel operations --- *)

(** [process_one_message ch ?timeout ()] reads and routes one incoming message
    for channel [ch]. Dispatches replies to [ch.responses] and requests to
    [ch.requests]. *)
let process_one_message ch ?(timeout = channel_timeout) () =
  let start = Unix.gettimeofday () in
  run_reader ch.conn ~until:(fun () ->
      ch.closed
      || Unix.gettimeofday () -. start > timeout
      || not (Queue.is_empty ch.inbox));
  if ch.closed then
    raise (Failure (Printf.sprintf "%s is closed" (channel_name ch)));
  if Queue.is_empty ch.inbox then
    raise
      (Failure
         (Printf.sprintf "Timed out after %.1fs waiting for a message on %s"
            timeout (channel_name ch)));
  let item = Queue.pop ch.inbox in
  match item with
  | Shutdown -> raise (Failure "Connection closed")
  | Pkt pkt ->
      if pkt.is_reply then begin
        if Hashtbl.mem ch.responses pkt.message_id then
          failwith
            (Printf.sprintf "Got two responses for message ID %ld"
               pkt.message_id)
        else Hashtbl.replace ch.responses pkt.message_id pkt.payload
      end
      else Queue.push pkt ch.requests

(** [send_request_raw ch payload] sends raw bytes as a request and returns the
    message ID for matching the response. *)
let send_request_raw ch payload =
  let message_id = ch.next_message_id in
  ch.next_message_id <- Int32.add ch.next_message_id 1l;
  send_packet ch.conn
    { payload; channel_id = ch.channel_id; is_reply = false; message_id };
  message_id

(** [send_request ch message] sends a CBOR-encoded request and returns the
    message ID. *)
let send_request ch message = send_request_raw ch (CBOR.Simple.encode message)

(** [receive_response_raw ch message_id ?timeout ()] waits for raw response
    bytes to a request with the given [message_id]. *)
let receive_response_raw ch message_id ?(timeout = channel_timeout) () =
  while not (Hashtbl.mem ch.responses message_id) do
    process_one_message ch ~timeout ()
  done;
  let result = Hashtbl.find ch.responses message_id in
  Hashtbl.remove ch.responses message_id;
  result

(** [receive_response ch message_id ?timeout ()] waits for and decodes a
    response, extracting the result or raising {!Request_error}. *)
let receive_response ch message_id ?(timeout = channel_timeout) () =
  let raw = receive_response_raw ch message_id ~timeout () in
  result_or_error (CBOR.Simple.decode raw)

(** [receive_request_raw ch ?timeout ()] receives raw request bytes and returns
    [(message_id, payload)]. *)
let receive_request_raw ch ?(timeout = channel_timeout) () =
  while Queue.is_empty ch.requests do
    process_one_message ch ~timeout ()
  done;
  let pkt = Queue.pop ch.requests in
  (pkt.message_id, pkt.payload)

(** [receive_request ch ?timeout ()] receives and CBOR-decodes a request,
    returning [(message_id, decoded_value)]. *)
let receive_request ch ?(timeout = channel_timeout) () =
  let message_id, body = receive_request_raw ch ~timeout () in
  (message_id, CBOR.Simple.decode body)

(** [send_response_raw ch message_id payload] sends raw bytes as a reply. *)
let send_response_raw ch message_id payload =
  send_packet ch.conn
    { payload; channel_id = ch.channel_id; is_reply = true; message_id }

(** [send_response_value ch message_id value] sends a success response with
    [value] wrapped as [{"result": value}]. *)
let send_response_value ch message_id value =
  send_response_raw ch message_id
    (CBOR.Simple.encode (`Map [ (`Text "result", value) ]))

(** [send_response_error ch message_id ~error ~error_type ()] sends an error
    response. *)
let send_response_error ch message_id ~error ~error_type () =
  send_response_raw ch message_id
    (CBOR.Simple.encode
       (`Map [ (`Text "error", `Text error); (`Text "type", `Text error_type) ]))

(** [close_channel ch] closes the channel and notifies the peer. Idempotent. *)
let close_channel ch =
  if ch.closed then ()
  else begin
    let is_registered =
      match Hashtbl.find_opt ch.conn.channels ch.channel_id with
      | Some (Live ch2) -> ch2 == ch
      | _ -> false
    in
    if not is_registered then ch.closed <- true
    else begin
      ch.closed <- true;
      if ch.conn.debug then
        Hashtbl.replace ch.conn.channels ch.channel_id
          (Dead { name = channel_name ch; channel_id = ch.channel_id });
      if is_live ch.conn then
        send_packet ch.conn
          {
            payload = close_channel_payload;
            message_id = close_channel_message_id;
            channel_id = ch.channel_id;
            is_reply = false;
          }
    end
  end

type pending_request = {
  pr_channel : channel;
  pr_message_id : int32;
  mutable pr_value : CBOR.Simple.t option;
}
(** Pending request handle that caches the decoded response. *)

(** [request ch message] sends a CBOR request and returns a {!pending_request}
    handle. *)
let request ch message =
  let message_id = send_request ch message in
  { pr_channel = ch; pr_message_id = message_id; pr_value = None }

(** [pending_get pr] blocks until the response arrives and returns the result.
    Caches the response so subsequent calls return the same value or raise the
    same error. *)
let pending_get pr =
  match pr.pr_value with
  | Some v -> result_or_error v
  | None ->
      let raw =
        receive_response_raw pr.pr_channel pr.pr_message_id
          ~timeout:channel_timeout ()
      in
      let v = CBOR.Simple.decode raw in
      pr.pr_value <- Some v;
      result_or_error v

(** [handle_requests ch handler] processes incoming requests, calling [handler]
    for each one. Sends the return value as a success response, or sends an
    error response if [handler] raises an exception. Runs until the connection
    closes. *)
let handle_requests ch handler =
  try
    while true do
      let message_id, message = receive_request ch () in
      try
        let result = handler message in
        send_response_value ch message_id result
      with exn ->
        let error = Printexc.to_string exn in
        let error_type = Printexc.exn_slot_name exn in
        send_response_error ch message_id ~error ~error_type ()
    done
  with Failure _ -> ()

(* --- Handshake --- *)

(** [send_handshake conn] initiates the handshake as a client. Returns the
    server protocol version string (e.g. ["0.1"]). *)
let send_handshake conn =
  if conn.connection_state <> Unresolved then
    failwith "Handshake already established";
  conn.connection_state <- Client;
  let ch = control_channel conn in
  let message_id = send_request_raw ch handshake_string in
  let response = receive_response_raw ch message_id () in
  if String.length response < 6 || String.sub response 0 6 <> "Hegel/" then
    failwith (Printf.sprintf "Bad handshake response: %S" response)
  else String.sub response 6 (String.length response - 6)

(** [receive_handshake conn] accepts the handshake as a server. *)
let receive_handshake conn =
  if conn.connection_state <> Unresolved then
    failwith "Handshake already established";
  conn.connection_state <- Server;
  let ch = control_channel conn in
  let message_id, payload = receive_request_raw ch () in
  if payload <> handshake_string then
    failwith
      (Printf.sprintf "Bad handshake: expected %S, got %S" handshake_string
         payload);
  send_response_raw ch message_id ("Hegel/" ^ protocol_version)
