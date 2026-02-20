let magic = 0x4845474C (* "HEGL" *)
let header_size = 20
let reply_bit = 1 lsl 31
let terminator = 0x0A
let close_channel_payload = "\xFE"
let close_channel_message_id = (1 lsl 31) - 1
let supported_protocol_versions = (0.1, 0.1)
let handshake_string = "hegel_handshake_start"

type packet = {
  channel : int;
  message_id : int;
  is_reply : bool;
  payload : string;
}

(* ---- Low-level I/O ---- *)

let write_all fd s =
  let len = String.length s in
  let written = ref 0 in
  while !written < len do
    let n = Unix.write_substring fd s !written (len - !written) in
    (* Blocking sockets never return 0 from write — they write or raise *)
    assert (n > 0);
    written := !written + n
  done

let read_exact fd n =
  let buf = Bytes.create n in
  let read = ref 0 in
  while !read < n do
    let got = Unix.read fd buf !read (n - !read) in
    if got = 0 then failwith "read_exact: connection closed";
    read := !read + got
  done;
  Bytes.to_string buf

(* ---- Packet I/O ---- *)

let put_u32_be buf off v =
  Bytes.set buf off (Char.chr ((v lsr 24) land 0xff));
  Bytes.set buf (off + 1) (Char.chr ((v lsr 16) land 0xff));
  Bytes.set buf (off + 2) (Char.chr ((v lsr 8) land 0xff));
  Bytes.set buf (off + 3) (Char.chr (v land 0xff))

let get_u32_be s off =
  (Char.code s.[off] lsl 24)
  lor (Char.code s.[off + 1] lsl 16)
  lor (Char.code s.[off + 2] lsl 8)
  lor Char.code s.[off + 3]
  land 0xFFFFFFFF

let write_packet fd pkt =
  let message_id_wire =
    if pkt.is_reply then pkt.message_id lor reply_bit else pkt.message_id
  in
  let header = Bytes.create header_size in
  put_u32_be header 0 magic;
  (* checksum placeholder at 4..8 *)
  Bytes.fill header 4 4 '\x00';
  put_u32_be header 8 pkt.channel;
  put_u32_be header 12 message_id_wire;
  put_u32_be header 16 (String.length pkt.payload);
  (* compute CRC32 over header (with zeroed checksum) + payload *)
  let header_str = Bytes.to_string header in
  let checksum = Crc32.compute (header_str ^ pkt.payload) in
  put_u32_be header 4 (Int32.to_int checksum);
  write_all fd (Bytes.to_string header);
  write_all fd pkt.payload;
  write_all fd (String.make 1 (Char.chr terminator))

let read_packet fd =
  let header = read_exact fd header_size in
  let pkt_magic = get_u32_be header 0 in
  if pkt_magic <> magic then
    failwith
      (Printf.sprintf "Invalid magic: expected 0x%08X, got 0x%08X" magic
         pkt_magic);
  let checksum = get_u32_be header 4 in
  let channel = get_u32_be header 8 in
  let message_id_raw = get_u32_be header 12 in
  let length = get_u32_be header 16 in
  let is_reply = message_id_raw land reply_bit <> 0 in
  let message_id = message_id_raw land lnot reply_bit in
  let payload = read_exact fd length in
  let term = read_exact fd 1 in
  if Char.code term.[0] <> terminator then
    failwith
      (Printf.sprintf "Invalid terminator: expected 0x%02X, got 0x%02X"
         terminator
         (Char.code term.[0]));
  (* verify checksum *)
  let header_for_check =
    String.sub header 0 4 ^ "\x00\x00\x00\x00" ^ String.sub header 8 12
  in
  let computed =
    Int32.to_int (Crc32.compute (header_for_check ^ payload)) land 0xFFFFFFFF
  in
  if computed <> checksum then
    failwith
      (Printf.sprintf "Checksum mismatch: expected 0x%08X, got 0x%08X" checksum
         computed);
  { channel; message_id; is_reply; payload }

(* ---- Connection ---- *)

module Connection = struct
  type t = {
    fd : Unix.file_descr;
    lock : Mutex.t;
    mutable next_channel_id : int;
    pending : (int, packet Queue.t) Hashtbl.t;
  }

  let create fd =
    {
      fd;
      lock = Mutex.create ();
      next_channel_id = 1;
      pending = Hashtbl.create 16;
    }

  let send_packet conn pkt =
    Mutex.lock conn.lock;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock conn.lock)
      (fun () -> write_packet conn.fd pkt)

  let receive_packet_for_channel conn channel_id =
    (* check pending first *)
    (match Hashtbl.find_opt conn.pending channel_id with
    | Some q when not (Queue.is_empty q) -> Some (Queue.pop q)
    | _ -> None)
    |> function
    | Some pkt -> pkt
    | None ->
        let rec loop () =
          let pkt = read_packet conn.fd in
          if pkt.channel = channel_id then pkt
          else
            let q =
              match Hashtbl.find_opt conn.pending pkt.channel with
              | Some q -> q
              | None ->
                  let q = Queue.create () in
                  Hashtbl.replace conn.pending pkt.channel q;
                  q
            in
            Queue.push pkt q;
            loop ()
        in
        loop ()

  let control_channel conn = (conn, 0)

  let new_channel conn =
    let next = conn.next_channel_id in
    conn.next_channel_id <- next + 1;
    let channel_id = (next lsl 1) lor 1 in
    (conn, channel_id)

  let connect_channel conn channel_id = (conn, channel_id)

  let close conn =
    (try Unix.shutdown conn.fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    Unix.close conn.fd
end

(* ---- Channel ---- *)

module Channel = struct
  type t = Connection.t * int

  let channel_id (_, id) = id
  let next_msg_id = ref 1

  let send_request (conn, chan_id) payload =
    let msg_id = !next_msg_id in
    next_msg_id := msg_id + 1;
    Connection.send_packet conn
      { channel = chan_id; message_id = msg_id; is_reply = false; payload };
    msg_id

  let send_response (conn, chan_id) msg_id payload =
    Connection.send_packet conn
      { channel = chan_id; message_id = msg_id; is_reply = true; payload }

  let receive_response (conn, _chan_id) msg_id =
    let pkt = Connection.receive_packet_for_channel conn _chan_id in
    if pkt.is_reply && pkt.message_id = msg_id then pkt.payload
    else
      (* receive_packet_for_channel always returns same-channel packets,
         so re-queuing and looping would be infinite. Fail fast instead. *)
      failwith
        (Printf.sprintf
           "receive_response: unexpected packet (is_reply=%b, msg_id=%d, \
            expected=%d)"
           pkt.is_reply pkt.message_id msg_id)

  let receive_request (conn, _chan_id) =
    let pkt = Connection.receive_packet_for_channel conn _chan_id in
    if not pkt.is_reply then (pkt.message_id, pkt.payload)
    else
      (* receive_packet_for_channel always returns same-channel packets,
         so re-queuing a reply and looping would be infinite. Fail fast. *)
      failwith
        (Printf.sprintf
           "receive_request: unexpected reply (msg_id=%d) on channel %d"
           pkt.message_id _chan_id)

  let request_cbor ((_, _) as ch) message =
    let payload = Cbor.encode_to_string message in
    let msg_id = send_request ch payload in
    let response_bytes = receive_response ch msg_id in
    let response = Cbor.decode_string response_bytes in
    (* check for error response *)
    (match Cbor.map_get response "error" with
    | Some err ->
        let error_type =
          match Cbor.map_get response "type" with
          | Some (Cbor.Text s) -> s
          | _ -> "unknown"
        in
        let err_str =
          match err with Cbor.Text s -> s | _ -> Cbor.encode_to_string err
        in
        failwith (Printf.sprintf "Server error (%s): %s" error_type err_str)
    | None -> ());
    (* extract result field if present *)
    match Cbor.map_get response "result" with
    | Some result -> result
    | None -> response

  let close (conn, chan_id) =
    Connection.send_packet conn
      {
        channel = chan_id;
        message_id = close_channel_message_id;
        is_reply = false;
        payload = close_channel_payload;
      }
end
