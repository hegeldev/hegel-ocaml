(** Wire protocol for the Hegel SDK.

    This module defines the binary packet format used for communication between
    the Hegel SDK and the Hegel server. Messages are serialized as packets with
    a 20-byte header, a CBOR-encoded payload, and a terminator byte.

    The header consists of 5 big-endian unsigned 32-bit integers:
    - Magic number (0x4845474C, "HEGL")
    - CRC32 checksum
    - Channel ID
    - Message ID (high bit = reply flag)
    - Payload length *)

(** Magic number identifying Hegel packets ("HEGL" in hex). *)
let magic = 0x4845474Cl

(** Size of the packet header in bytes (5 big-endian uint32 fields). *)
let header_size = 20

(** Terminator byte appended after the payload. *)
let terminator = 0x0A

(** Bit flag indicating a reply message. When set in the message ID field, the
    message is a response to a previous request. *)
let reply_bit = Int32.shift_left 1l 31

(** Special message ID used when closing a channel. *)
let close_channel_message_id = Int32.sub (Int32.shift_left 1l 31) 1l

(** Special payload byte sent when closing a channel. Chosen to be invalid CBOR
    per RFC 8949 (reserved tag byte 0xFE). *)
let close_channel_payload = Bytes.make 1 '\xFE' |> Bytes.to_string

type packet = {
  channel_id : int32;  (** The logical channel this packet belongs to. *)
  message_id : int32;  (** Identifier for request/response correlation. *)
  is_reply : bool;  (** Whether this is a reply to a previous request. *)
  payload : string;  (** The raw payload bytes (typically CBOR-encoded). *)
}
(** A single message in the wire protocol. *)

(** [equal_packet a b] returns [true] if packets [a] and [b] have identical
    fields. *)
let equal_packet a b =
  a.channel_id = b.channel_id
  && a.message_id = b.message_id
  && a.is_reply = b.is_reply && a.payload = b.payload

(** [pp_packet fmt p] pretty-prints packet [p] to formatter [fmt]. *)
let pp_packet fmt p =
  Format.fprintf fmt
    "{channel_id=%ld; message_id=%ld; is_reply=%b; payload=<%d bytes>}"
    p.channel_id p.message_id p.is_reply (String.length p.payload)

(** [compute_crc32 data] computes the CRC32 checksum of [data] and returns it as
    an [int32]. *)
let compute_crc32 data =
  let crc =
    Checkseum.Crc32.digest_string data 0 (String.length data)
      Checkseum.Crc32.default
  in
  Optint.to_int32 crc

(** [compute_crc32_parts a b] computes the CRC32 checksum over the concatenation
    of strings [a] and [b] without allocating the concatenated string. *)
let compute_crc32_parts a b =
  let crc =
    Checkseum.Crc32.digest_string a 0 (String.length a) Checkseum.Crc32.default
  in
  let crc = Checkseum.Crc32.digest_string b 0 (String.length b) crc in
  Optint.to_int32 crc

(** [pack_uint32_be buf offset value] writes [value] as a big-endian unsigned
    32-bit integer at [offset] in [buf]. *)
let pack_uint32_be buf offset value =
  Bytes.set buf offset
    (Char.chr (Int32.to_int (Int32.shift_right_logical value 24) land 0xFF));
  Bytes.set buf (offset + 1)
    (Char.chr (Int32.to_int (Int32.shift_right_logical value 16) land 0xFF));
  Bytes.set buf (offset + 2)
    (Char.chr (Int32.to_int (Int32.shift_right_logical value 8) land 0xFF));
  Bytes.set buf (offset + 3) (Char.chr (Int32.to_int value land 0xFF))

(** [unpack_uint32_be data offset] reads a big-endian unsigned 32-bit integer
    from [data] at [offset]. *)
let unpack_uint32_be data offset =
  let b0 = Int32.of_int (Char.code (Bytes.get data offset)) in
  let b1 = Int32.of_int (Char.code (Bytes.get data (offset + 1))) in
  let b2 = Int32.of_int (Char.code (Bytes.get data (offset + 2))) in
  let b3 = Int32.of_int (Char.code (Bytes.get data (offset + 3))) in
  Int32.logor
    (Int32.logor (Int32.shift_left b0 24) (Int32.shift_left b1 16))
    (Int32.logor (Int32.shift_left b2 8) b3)

exception Partial_packet of string
(** Exception raised when the connection is closed partway through reading a
    packet. *)

exception Connection_closed of string
(** Exception raised when the connection is closed while reading data and some
    bytes have already been received. *)

(** [recv_exact sock n] reads exactly [n] bytes from Unix file descriptor
    [sock]. Raises {!Partial_packet} if the connection closes with no data read,
    or {!Connection_closed} if it closes after partial data. *)
let recv_exact sock n =
  if n = 0 then Bytes.empty
  else begin
    let buf = Bytes.create n in
    let rec read_all pos =
      if pos < n then begin
        let chunk = Unix.read sock buf pos (n - pos) in
        if chunk = 0 then begin
          if pos > 0 then
            raise (Connection_closed "Connection closed while reading data")
          else
            raise
              (Partial_packet
                 "Connection closed partway through reading packet.")
        end;
        read_all (pos + chunk)
      end
    in
    read_all 0;
    buf
  end

(** [read_packet sock] reads and parses a single packet from Unix file
    descriptor [sock].

    Raises [Failure] if the magic number is invalid, the terminator byte is
    invalid, or the CRC32 checksum does not match. *)
let read_packet sock =
  let header_buf = recv_exact sock header_size in
  let pkt_magic = unpack_uint32_be header_buf 0 in
  let checksum = unpack_uint32_be header_buf 4 in
  let channel_id = unpack_uint32_be header_buf 8 in
  let raw_message_id = unpack_uint32_be header_buf 12 in
  let length = unpack_uint32_be header_buf 16 in
  let is_reply = Int32.logand raw_message_id reply_bit <> 0l in
  let message_id =
    if is_reply then Int32.logxor raw_message_id reply_bit else raw_message_id
  in
  if pkt_magic <> magic then
    failwith
      (Printf.sprintf "Invalid magic number: expected 0x%08lX, got 0x%08lX"
         magic pkt_magic);
  let payload_buf = recv_exact sock (Int32.to_int length) in
  let term_buf = recv_exact sock 1 in
  let term_byte = Char.code (Bytes.get term_buf 0) in
  if term_byte <> terminator then
    failwith
      (Printf.sprintf "Invalid terminator: expected 0x%02X, got 0x%02X"
         terminator term_byte);
  (* Verify checksum: CRC32 over header with checksum field zeroed + payload *)
  let header_for_check = Bytes.copy header_buf in
  pack_uint32_be header_for_check 4 0l;
  let computed_crc =
    compute_crc32_parts
      (Bytes.to_string header_for_check)
      (Bytes.to_string payload_buf)
  in
  if computed_crc <> checksum then
    failwith
      (Printf.sprintf "Checksum mismatch: expected 0x%08lX, got 0x%08lX"
         checksum computed_crc);
  { channel_id; message_id; is_reply; payload = Bytes.to_string payload_buf }

(** [write_packet sock packet] serializes and writes [packet] to Unix file
    descriptor [sock]. *)
let write_packet sock packet =
  let wire_message_id =
    if packet.is_reply then Int32.logor packet.message_id reply_bit
    else packet.message_id
  in
  let payload_len = String.length packet.payload in
  (* Build header with zeroed checksum for CRC computation *)
  let header_buf = Bytes.create header_size in
  pack_uint32_be header_buf 0 magic;
  pack_uint32_be header_buf 4 0l;
  pack_uint32_be header_buf 8 packet.channel_id;
  pack_uint32_be header_buf 12 wire_message_id;
  pack_uint32_be header_buf 16 (Int32.of_int payload_len);
  let checksum =
    compute_crc32_parts (Bytes.to_string header_buf) packet.payload
  in
  pack_uint32_be header_buf 4 checksum;
  (* Assemble into a single buffer: header + payload + terminator *)
  let total_len = header_size + payload_len + 1 in
  let buf = Bytes.create total_len in
  Bytes.blit header_buf 0 buf 0 header_size;
  Bytes.blit_string packet.payload 0 buf header_size payload_len;
  Bytes.set buf (header_size + payload_len) (Char.chr terminator);
  let rec write_all pos =
    if pos < total_len then
      let written = Unix.write sock buf pos (total_len - pos) in
      write_all (pos + written)
  in
  write_all 0
