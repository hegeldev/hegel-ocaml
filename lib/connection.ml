(** Multiplexed connection and channel abstractions for Hegel.

    This module implements a background-reader model: a dedicated thread reads
    packets from the input stream and dispatches them to per-channel inboxes.
    Channels wait on condition variables for new messages.

    {b Connection} manages a pair of file descriptors (one for reading, one for
    writing) with multiplexed logical channels. {b Channel} provides
    request/response messaging over a single logical channel. *)

open! Core
module Unix = Core_unix
module Mutex = Caml_threads.Mutex
module Condition = Caml_threads.Condition
module Thread = Caml_threads.Thread
open Protocol

(** Handshake string sent by the client to initiate the protocol. *)
let handshake_string = "hegel_handshake_start"

(** Default timeout in seconds for channel operations. *)
let channel_timeout = 30.0

(** Message used when the server has crashed. *)
let server_crashed_message =
  "The hegel server process has exited unexpectedly. Check .hegel/server.log \
   for details."

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

type channel_inbox = {
  queue : inbox_item Queue.t;
  lock : Mutex.t;
  cond : Condition.t;
}
(** Thread-safe inbox for a channel, with condition-variable signaling. *)

(** A channel entry in the connection's channel table. *)
type channel_entry =
  | Live of channel
  | Dead of { channel_id : int32; name : string }

and connection = {
  name : string option;
  read_fd : Unix.File_descr.t;
  write_fd : Unix.File_descr.t;
  mutable next_channel_id : int;
  channels : (Int32.t, channel_entry) Hashtbl.t;
  channels_lock : Mutex.t;
  mutable running : bool;
  debug : bool;
  writer_lock : Mutex.t;
  mutable connection_state : connection_state;
  mutable server_exited : bool;
}
(** Multiplexed connection to a Hegel peer. *)

and channel = {
  channel_id : int32;
  conn : connection;
  inbox : channel_inbox;
  requests : packet Queue.t;
  responses : (Int32.t, string) Hashtbl.t;
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
  | None, None -> sprintf "Channel %ld" ch.channel_id
  | None, Some n -> sprintf "%s channel [id=%ld]" n ch.channel_id
  | Some r, None -> sprintf "channel [id=%ld] (%s)" ch.channel_id r
  | Some r, Some n -> sprintf "%s channel [id=%ld] (%s)" n ch.channel_id r

(** [channel_repr ch] returns a debug representation of channel [ch]. *)
let channel_repr ch =
  match ch.role with
  | None -> sprintf "Channel(%ld)" ch.channel_id
  | Some r -> sprintf "Channel(%ld, role=%s)" ch.channel_id r

(** [entry_name conn channel_id] returns the name of the channel entry for
    [channel_id] in [conn], or a default. *)
let entry_name conn channel_id =
  match Hashtbl.find conn.channels channel_id with
  | Some (Live ch) -> channel_name ch
  | Some (Dead d) -> d.name
  | None -> sprintf "channel %ld" channel_id

(** [make_channel conn channel_id ~role] creates a new channel. *)
let make_channel conn channel_id ~role =
  assert (Int32.( <> ) channel_id 0l || Poly.( = ) role (Some "Control"));
  {
    channel_id;
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

(** [push_inbox ch item] pushes an item to a channel's inbox and signals the
    condition variable. Must be called without holding [ch.inbox.lock]. *)
let push_inbox ch item =
  Mutex.lock ch.inbox.lock;
  Queue.enqueue ch.inbox.queue item;
  Condition.signal ch.inbox.cond;
  Mutex.unlock ch.inbox.lock

(** [signal_all_channels conn] pushes {!Shutdown} into all live channels'
    inboxes. Must be called while holding [conn.channels_lock]. *)
let signal_all_channels conn =
  Hashtbl.iteri conn.channels ~f:(fun ~key:_ ~data:entry ->
      match entry with Live ch -> push_inbox ch Shutdown | Dead _ -> ())

(** [send_packet conn packet] sends a packet on the connection, thread-safe. *)
let send_packet conn packet =
  Mutex.lock conn.writer_lock;
  Exn.protect
    ~finally:(fun () -> Mutex.unlock conn.writer_lock)
    ~f:(fun () -> write_packet conn.write_fd packet)

(** Background reader thread function. Reads packets from [conn.read_fd] and
    dispatches them to the appropriate channel's inbox. Exits when the stream
    closes or an error occurs. *)
let reader_loop conn =
  let dispatch_packet (pkt : packet) =
    (* Handle close_channel_payload for ANY channel (even unregistered) *)
    if String.equal pkt.payload close_channel_payload then begin
      assert (Int32.equal pkt.message_id close_channel_message_id);
      if conn.debug then begin
        Mutex.lock conn.channels_lock;
        let prev_name =
          match Hashtbl.find conn.channels pkt.channel_id with
          | Some (Live ch) -> channel_name ch
          | Some (Dead d) -> d.name
          | None -> "Never opened!"
        in
        Hashtbl.set conn.channels ~key:pkt.channel_id
          ~data:(Dead { channel_id = pkt.channel_id; name = prev_name });
        Mutex.unlock conn.channels_lock
      end
    end
    else begin
      Mutex.lock conn.channels_lock;
      let entry = Hashtbl.find conn.channels pkt.channel_id in
      match entry with
      | Some (Live ch) ->
          Mutex.unlock conn.channels_lock;
          push_inbox ch (Pkt pkt)
      | (Some (Dead _) | None) as entry ->
          Mutex.unlock conn.channels_lock;
          if not pkt.is_reply then begin
            let disposition =
              match entry with Some (Dead _) -> "closed" | _ -> "non-existent"
            in
            let error_msg =
              sprintf "Message %ld sent to %s %s" pkt.message_id disposition
                (entry_name conn pkt.channel_id)
            in
            try
              send_packet conn
                {
                  channel_id = pkt.channel_id;
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
  (* Signal shutdown to all channels *)
  Mutex.lock conn.channels_lock;
  signal_all_channels conn;
  Mutex.unlock conn.channels_lock

(** [close conn] closes the connection and signals all live channels. *)
let close conn =
  if not conn.running then ()
  else begin
    conn.running <- false;
    (try Unix.close conn.read_fd with Unix.Unix_error _ -> ());
    (* Only close write_fd if it's different from read_fd *)
    (if not (phys_equal conn.write_fd conn.read_fd) then
       try Unix.close conn.write_fd with Unix.Unix_error _ -> ());
    Mutex.lock conn.channels_lock;
    signal_all_channels conn;
    Mutex.unlock conn.channels_lock
  end

(** [create_connection ~read_fd ~write_fd ?name ?debug ()] creates a new
    connection using separate file descriptors for reading and writing. A
    control channel (channel 0) is automatically created and a background reader
    thread is spawned. *)
let create_connection ~read_fd ~write_fd ?name ?(debug = false) () =
  let conn =
    {
      name;
      read_fd;
      write_fd;
      next_channel_id = 1;
      channels = Hashtbl.create (module Int32);
      channels_lock = Mutex.create ();
      running = true;
      debug;
      writer_lock = Mutex.create ();
      connection_state = Unresolved;
      server_exited = false;
    }
  in
  let control = make_channel conn 0l ~role:(Some "Control") in
  Mutex.lock conn.channels_lock;
  Hashtbl.set conn.channels ~key:0l ~data:(Live control);
  Mutex.unlock conn.channels_lock;
  (* Spawn background reader thread *)
  ignore (Thread.create reader_loop conn);
  conn

(** [is_live conn] returns [true] if the connection is still active. *)
let is_live conn = conn.running

(** [server_has_exited conn] returns [true] if the server process has exited. *)
let server_has_exited conn = conn.server_exited

(** [control_channel conn] returns the control channel (channel 0). *)
let control_channel conn =
  match Hashtbl.find conn.channels 0l with
  | Some (Live ch) -> ch
  | _ -> failwith "Internal error: no control channel"

(** [new_channel conn ?role ()] creates a new logical channel on the connection.

    Client channels use odd IDs [(counter << 1) | 1], server channels use even
    IDs [(counter << 1)]. *)
let new_channel conn ?role () =
  if equal_connection_state conn.connection_state Unresolved then
    failwith "Cannot create a new channel before handshake has been performed."
  else begin
    let channel_id = Int32.of_int_exn ((conn.next_channel_id lsl 1) lor 1) in
    conn.next_channel_id <- conn.next_channel_id + 1;
    let ch = make_channel conn channel_id ~role in
    Mutex.lock conn.channels_lock;
    Hashtbl.set conn.channels ~key:channel_id ~data:(Live ch);
    Mutex.unlock conn.channels_lock;
    ch
  end

(** [connect_channel conn channel_id ?role ()] connects to a channel created by
    the peer. *)
let connect_channel conn channel_id ?role () =
  if equal_connection_state conn.connection_state Unresolved then
    failwith "Cannot create a new channel before handshake has been performed.";
  if Hashtbl.mem conn.channels channel_id then
    failwith (sprintf "Channel already connected as %ld." channel_id);
  let ch = make_channel conn channel_id ~role in
  Mutex.lock conn.channels_lock;
  Hashtbl.set conn.channels ~key:channel_id ~data:(Live ch);
  Mutex.unlock conn.channels_lock;
  ch

(* --- Channel operations --- *)

(** [pop_inbox_item ch timeout] waits for an item to appear in the channel's
    inbox, with a timeout. Returns the item or raises [Failure] on timeout or
    connection close. *)
let pop_inbox_item ch timeout =
  Mutex.lock ch.inbox.lock;
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    if not (Queue.is_empty ch.inbox.queue) then begin
      let item = Queue.dequeue_exn ch.inbox.queue in
      Mutex.unlock ch.inbox.lock;
      item
    end
    else if ch.closed then begin
      Mutex.unlock ch.inbox.lock;
      raise (Failure (sprintf "%s is closed" (channel_name ch)))
    end
    else if ch.conn.server_exited then begin
      Mutex.unlock ch.inbox.lock;
      raise (Failure server_crashed_message)
    end
    else begin
      let remaining = deadline -. Unix.gettimeofday () in
      if Float.( <= ) remaining 0.0 then begin
        Mutex.unlock ch.inbox.lock;
        raise
          (Failure
             (sprintf "Timed out after %.1fs waiting for a message on %s"
                timeout (channel_name ch)))
      end;
      (* Release lock, sleep briefly, re-acquire *)
      Mutex.unlock ch.inbox.lock;
      Caml_unix.sleepf (Float.min 0.01 remaining);
      Mutex.lock ch.inbox.lock;
      wait ()
    end
  in
  wait ()

(** [process_one_message ch ?timeout ()] reads and routes one incoming message
    for channel [ch]. Dispatches replies to [ch.responses] and requests to
    [ch.requests]. *)
let process_one_message ch ?(timeout = channel_timeout) () =
  let item = pop_inbox_item ch timeout in
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
  let result = Hashtbl.find_exn ch.responses message_id in
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
  let pkt = Queue.dequeue_exn ch.requests in
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

(** [close_channel ch] closes the channel and notifies the peer. Idempotent. *)
let close_channel ch =
  if ch.closed then ()
  else begin
    let is_registered =
      match Hashtbl.find ch.conn.channels ch.channel_id with
      | Some (Live ch2) -> phys_equal ch2 ch
      | _ -> false
    in
    if not is_registered then ch.closed <- true
    else begin
      ch.closed <- true;
      if ch.conn.debug then
        Hashtbl.set ch.conn.channels ~key:ch.channel_id
          ~data:(Dead { name = channel_name ch; channel_id = ch.channel_id });
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

(* --- Handshake --- *)

(** [send_handshake conn] initiates the handshake as a client. Returns the
    server protocol version string (e.g. ["0.1"]). *)
let send_handshake conn =
  if not (equal_connection_state conn.connection_state Unresolved) then
    failwith "Handshake already established";
  conn.connection_state <- Client;
  let ch = control_channel conn in
  let message_id = send_request_raw ch handshake_string in
  let response = receive_response_raw ch message_id () in
  if
    String.length response < 6
    || not (String.equal (String.sub response ~pos:0 ~len:6) "Hegel/")
  then failwith (sprintf "Bad handshake response: %S" response)
  else String.sub response ~pos:6 ~len:(String.length response - 6)
