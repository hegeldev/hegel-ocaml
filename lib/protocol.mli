type packet = {
  channel : int;
  message_id : int;
  is_reply : bool;
  payload : string;
}

val write_packet : Unix.file_descr -> packet -> unit
val read_packet : Unix.file_descr -> packet

module Channel : sig
  type t

  val channel_id : t -> int
  val send_request : t -> string -> int
  val send_response : t -> int -> string -> unit
  val receive_response : t -> int -> string
  val receive_request : t -> int * string
  val request_cbor : t -> Cbor.t -> Cbor.t
  val close : t -> unit
end

module Connection : sig
  type t

  val create : Unix.file_descr -> t
  val control_channel : t -> Channel.t
  val new_channel : t -> Channel.t
  val connect_channel : t -> int -> Channel.t
  val close : t -> unit
end

val supported_protocol_versions : float * float
val handshake_string : string
