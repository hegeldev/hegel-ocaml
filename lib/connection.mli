(** Multiplexed connection and stream abstractions for Hegel.

    This module implements a background-reader model: a dedicated thread reads
    packets from the input stream and dispatches them to per-stream inboxes.
    Streams wait on condition variables for new messages. *)

open! Core
module Unix = Core_unix
module Mutex = Caml_threads.Mutex
module Condition = Caml_threads.Condition

(** Sentinel value placed in a stream's inbox when the connection shuts down. *)
type inbox_item = Pkt of Protocol.packet | Shutdown

exception
  Request_error of {
    message : string;
    error_type : string;
    data : (CBOR.Simple.t * CBOR.Simple.t) list;
  }
(** Error response from the peer. *)

val result_or_error : CBOR.Simple.t -> CBOR.Simple.t
(** [result_or_error body] extracts the ["result"] field from a CBOR map, or
    raises {!Request_error} if an ["error"] field is present. *)

(** Connection state after handshake. *)
type connection_state = Unresolved | Client

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
  read_fd : Core_unix.File_descr.t;
  write_fd : Core_unix.File_descr.t;
  mutable next_stream_id : int;
  streams : (int32, stream_entry) Hashtbl.t;
  streams_lock : Mutex.t;
  mutable running : bool;
  debug : bool;
  writer_lock : Mutex.t;
  mutable connection_state : connection_state;
  mutable server_exited : bool;
}
(** Multiplexed connection to a Hegel peer. *)

and stream = {
  stream_id : int32;
  conn : connection;
  inbox : stream_inbox;
  requests : Protocol.packet Queue.t;
  responses : (int32, string) Hashtbl.t;
  role : string option;
  mutable next_message_id : int32;
  mutable closed : bool;
}
(** Logical stream for request/response messaging. *)

val create_connection :
  read_fd:Core_unix.File_descr.t ->
  write_fd:Core_unix.File_descr.t ->
  ?name:string ->
  ?debug:bool ->
  unit ->
  connection
(** [create_connection ~read_fd ~write_fd ?name ?debug ()] creates a new
    connection using separate file descriptors for reading and writing. A
    control stream (stream 0) is automatically created and a background reader
    thread is spawned. *)

val is_live : connection -> bool
(** [is_live conn] returns [true] if the connection is still active. *)

val server_has_exited : connection -> bool
(** [server_has_exited conn] returns [true] if the server process has exited. *)

val close : connection -> unit
(** [close conn] closes the connection and signals all live streams. *)

val new_stream : connection -> ?role:string -> unit -> stream
(** [new_stream conn ?role ()] creates a new logical stream on the connection.
*)

val connect_stream : connection -> int32 -> ?role:string -> unit -> stream
(** [connect_stream conn stream_id ?role ()] connects to a stream created by the
    peer. *)

val stream_id : stream -> int32
(** [stream_id ch] returns the stream ID of [ch]. *)

val stream_name : stream -> string
(** [stream_name ch] returns a human-readable name for stream [ch]. *)

val stream_repr : stream -> string
(** [stream_repr ch] returns a debug representation of stream [ch]. *)

val entry_name : connection -> int32 -> string
(** [entry_name conn stream_id] returns the name of the stream entry for
    [stream_id] in [conn], or a default. *)

val send_packet : connection -> Protocol.packet -> unit
(** [send_packet conn packet] sends a packet on the connection, thread-safe. *)

val send_request_raw : stream -> string -> int32
(** [send_request_raw ch payload] sends raw bytes as a request and returns the
    message ID for matching the response. *)

val send_request : stream -> CBOR.Simple.t -> int32
(** [send_request ch message] sends a CBOR-encoded request and returns the
    message ID. *)

val receive_response_raw : stream -> int32 -> ?timeout:float -> unit -> string
(** [receive_response_raw ch message_id ?timeout ()] waits for raw response
    bytes to a request with the given [message_id]. *)

val receive_response :
  stream -> int32 -> ?timeout:float -> unit -> CBOR.Simple.t
(** [receive_response ch message_id ?timeout ()] waits for and decodes a
    response, extracting the result or raising {!Request_error}. *)

val receive_request_raw : stream -> ?timeout:float -> unit -> int32 * string
(** [receive_request_raw ch ?timeout ()] receives raw request bytes and returns
    [(message_id, payload)]. *)

val receive_request : stream -> ?timeout:float -> unit -> int32 * CBOR.Simple.t
(** [receive_request ch ?timeout ()] receives and CBOR-decodes a request,
    returning [(message_id, decoded_value)]. *)

val send_response_raw : stream -> int32 -> string -> unit
(** [send_response_raw ch message_id payload] sends raw bytes as a reply. *)

val send_response_value : stream -> int32 -> CBOR.Simple.t -> unit
(** [send_response_value ch message_id value] sends a success response with
    [value] wrapped as [{"result": value}]. *)

val close_stream : stream -> unit
(** [close_stream ch] closes the stream and notifies the peer. Idempotent. *)

type pending_request = {
  pr_stream : stream;
  pr_message_id : int32;
  mutable pr_value : CBOR.Simple.t option;
}
(** Pending request handle that caches the decoded response. *)

val request : stream -> CBOR.Simple.t -> pending_request
(** [request ch message] sends a CBOR request and returns a {!pending_request}
    handle. *)

val pending_get : pending_request -> CBOR.Simple.t
(** [pending_get pr] blocks until the response arrives and returns the result.
    Caches the response so subsequent calls return the same value or raise the
    same error. *)

val process_one_message : stream -> ?timeout:float -> unit -> unit
(** [process_one_message ch ?timeout ()] waits for and routes one incoming
    message for stream [ch]. Dispatches replies to the stream's responses table
    and requests to the stream's request queue. *)

val server_crashed_message : string
(** Error message shown when the server process has exited unexpectedly. *)

val send_handshake : connection -> string
(** [send_handshake conn] initiates the handshake as a client. Returns the
    server protocol version string. *)

val control_stream : connection -> stream
(** [control_stream conn] returns the control stream (stream 0). *)
