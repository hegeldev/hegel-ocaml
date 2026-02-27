(** Wire protocol for the Hegel SDK.

    This module defines the binary packet format used for communication between
    the Hegel SDK and the Hegel server. *)

val magic : int32
(** Magic number identifying Hegel packets ("HEGL" in hex). *)

val header_size : int
(** Size of the packet header in bytes (5 big-endian uint32 fields). *)

val terminator : int
(** Terminator byte appended after the payload. *)

val reply_bit : int32
(** Bit flag indicating a reply message. *)

val close_channel_message_id : int32
(** Special message ID used when closing a channel. *)

val close_channel_payload : string
(** Special payload byte sent when closing a channel. *)

type packet = {
  channel_id : int32;  (** The logical channel this packet belongs to. *)
  message_id : int32;  (** Identifier for request/response correlation. *)
  is_reply : bool;  (** Whether this is a reply to a previous request. *)
  payload : string;  (** The raw payload bytes (typically CBOR-encoded). *)
}
(** A single message in the wire protocol. *)

exception Partial_packet of string
(** Exception raised when the connection is closed partway through reading a
    packet. *)

exception Connection_closed of string
(** Exception raised when the connection is closed while reading data and some
    bytes have already been received. *)

val equal_packet : packet -> packet -> bool
(** [equal_packet a b] returns [true] if packets [a] and [b] have identical
    fields. *)

val pp_packet : Format.formatter -> packet -> unit
(** [pp_packet fmt p] pretty-prints packet [p] to formatter [fmt]. *)

val compute_crc32 : string -> int32
(** [compute_crc32 data] computes the CRC32 checksum of [data] and returns it as
    an [int32]. *)

val compute_crc32_parts : string -> string -> int32
(** [compute_crc32_parts a b] computes the CRC32 checksum over the concatenation
    of strings [a] and [b] without allocating the concatenated string. *)

val pack_uint32_be : bytes -> int -> int32 -> unit
(** [pack_uint32_be buf offset value] writes [value] as a big-endian unsigned
    32-bit integer at [offset] in [buf]. *)

val unpack_uint32_be : bytes -> int -> int32
(** [unpack_uint32_be data offset] reads a big-endian unsigned 32-bit integer
    from [data] at [offset]. *)

val recv_exact : Unix.file_descr -> int -> bytes
(** [recv_exact sock n] reads exactly [n] bytes from Unix file descriptor
    [sock]. Raises {!Partial_packet} if the connection closes with no data read,
    or {!Connection_closed} if it closes after partial data. *)

val read_packet : Unix.file_descr -> packet
(** [read_packet sock] reads and parses a single packet from Unix file
    descriptor [sock]. *)

val write_packet : Unix.file_descr -> packet -> unit
(** [write_packet sock packet] serializes and writes [packet] to Unix file
    descriptor [sock]. *)
