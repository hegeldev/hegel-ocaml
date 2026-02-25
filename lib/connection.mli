(** Multiplexed connection and channel abstractions for the Hegel SDK.

    This module implements the demand-driven reader model: when a channel needs
    a message, it calls the reader which reads packets from the socket and
    dispatches them to the correct channel's inbox until the caller's condition
    is satisfied. *)

(** Sentinel value placed in a channel's inbox when the connection shuts down. *)
type inbox_item = Pkt of Protocol.packet | Shutdown

exception
  Request_error of {
    message : string;
    error_type : string;
    data : (CBOR.Simple.t * CBOR.Simple.t) list;
  }
(** Error response from the peer. *)

(** Connection state after handshake. *)
type connection_state = Unresolved | Client | Server

(** A channel entry in the connection's channel table. *)
type channel_entry =
  | Live of channel
  | Dead of { channel_id : int32; name : string }

(** Multiplexed socket connection to a Hegel peer. *)
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

(** Logical channel for request/response messaging. *)
and channel = {
  channel_id : int32;
  conn : connection;
  inbox : inbox_item Queue.t;
  requests : Protocol.packet Queue.t;
  responses : (int32, string) Hashtbl.t;
  role : string option;
  mutable next_message_id : int32;
  mutable closed : bool;
}

(** [create_connection sock ?name ?debug ()] creates a new connection wrapping
    the Unix file descriptor [sock]. A control channel (channel 0) is
    automatically created. *)
val create_connection :
  Unix.file_descr -> ?name:string -> ?debug:bool -> unit -> connection

(** [is_live conn] returns [true] if the connection is still active. *)
val is_live : connection -> bool

(** [close conn] closes the connection and signals all live channels. *)
val close : connection -> unit

(** [new_channel conn ?role ()] creates a new logical channel on the connection. *)
val new_channel : connection -> ?role:string -> unit -> channel

(** [connect_channel conn channel_id ?role ()] connects to a channel created by
    the peer. *)
val connect_channel : connection -> int32 -> ?role:string -> unit -> channel

(** [channel_id ch] returns the channel ID of [ch]. *)
val channel_id : channel -> int32

(** [channel_name ch] returns a human-readable name for channel [ch]. *)
val channel_name : channel -> string

(** [channel_repr ch] returns a debug representation of channel [ch]. *)
val channel_repr : channel -> string

(** [entry_name conn channel_id] returns the name of the channel entry for
    [channel_id] in [conn], or a default. *)
val entry_name : connection -> int32 -> string

(** [send_packet conn packet] sends a packet on the connection, thread-safe. *)
val send_packet : connection -> Protocol.packet -> unit

(** [send_request_raw ch payload] sends raw bytes as a request and returns the
    message ID for matching the response. *)
val send_request_raw : channel -> string -> int32

(** [send_request ch message] sends a CBOR-encoded request and returns the
    message ID. *)
val send_request : channel -> CBOR.Simple.t -> int32

(** [receive_response_raw ch message_id ?timeout ()] waits for raw response
    bytes to a request with the given [message_id]. *)
val receive_response_raw : channel -> int32 -> ?timeout:float -> unit -> string

(** [receive_response ch message_id ?timeout ()] waits for and decodes a
    response, extracting the result or raising {!Request_error}. *)
val receive_response : channel -> int32 -> ?timeout:float -> unit -> CBOR.Simple.t

(** [receive_request_raw ch ?timeout ()] receives raw request bytes and returns
    [(message_id, payload)]. *)
val receive_request_raw : channel -> ?timeout:float -> unit -> int32 * string

(** [receive_request ch ?timeout ()] receives and CBOR-decodes a request,
    returning [(message_id, decoded_value)]. *)
val receive_request : channel -> ?timeout:float -> unit -> int32 * CBOR.Simple.t

(** [send_response_raw ch message_id payload] sends raw bytes as a reply. *)
val send_response_raw : channel -> int32 -> string -> unit

(** [send_response_value ch message_id value] sends a success response with
    [value] wrapped as [{"result": value}]. *)
val send_response_value : channel -> int32 -> CBOR.Simple.t -> unit

(** [send_response_error ch message_id ~error ~error_type ()] sends an error
    response. *)
val send_response_error : channel -> int32 -> error:string -> error_type:string -> unit -> unit

(** [close_channel ch] closes the channel and notifies the peer. Idempotent. *)
val close_channel : channel -> unit

(** Pending request handle that caches the decoded response. *)
type pending_request = {
  pr_channel : channel;
  pr_message_id : int32;
  mutable pr_value : CBOR.Simple.t option;
}

(** [request ch message] sends a CBOR request and returns a {!pending_request}
    handle. *)
val request : channel -> CBOR.Simple.t -> pending_request

(** [pending_get pr] blocks until the response arrives and returns the result.
    Caches the response so subsequent calls return the same value or raise the
    same error. *)
val pending_get : pending_request -> CBOR.Simple.t

(** [handle_requests ch handler] processes incoming requests, calling [handler]
    for each one. Runs until the connection closes. *)
val handle_requests : channel -> (CBOR.Simple.t -> CBOR.Simple.t) -> unit

(** [send_handshake conn] initiates the handshake as a client. Returns the
    server protocol version string. *)
val send_handshake : connection -> string

(** [receive_handshake conn] accepts the handshake as a server. *)
val receive_handshake : connection -> unit

(** [control_channel conn] returns the control channel (channel 0). *)
val control_channel : connection -> channel
