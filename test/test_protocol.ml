open! Core
module Unix = Core_unix
module Thread = Caml_threads.Thread
open Hegel
open Protocol

let contains_substring = Test_helpers.contains_substring

(** Helper: create a socketpair for testing. Returns (reader, writer). *)
let with_socket_pair f =
  let reader, writer =
    Core_unix.socketpair ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()
  in
  Exn.protect
    ~f:(fun () -> f reader writer)
    ~finally:(fun () ->
      Core_unix.close reader;
      Core_unix.close writer)

(** Helper: build raw packet bytes for testing read_packet edge cases. Mirrors
    the Python _make_packet helper. *)
let make_raw_packet ?(pkt_magic = magic) ?checksum ?(channel_id = 0l)
    ?(message_id = 1l) ?(payload = "payload") ?(term = terminator) () =
  let length = Int32.of_int_exn (String.length payload) in
  (* Build header with zeroed checksum for CRC computation *)
  let header_for_check = Bytes.create header_size in
  pack_uint32_be header_for_check 0 pkt_magic;
  pack_uint32_be header_for_check 4 0l;
  pack_uint32_be header_for_check 8 channel_id;
  pack_uint32_be header_for_check 12 message_id;
  pack_uint32_be header_for_check 16 length;
  let computed_checksum =
    match checksum with
    | Some c -> c
    | None -> compute_crc32_parts (Bytes.to_string header_for_check) payload
  in
  (* Build final header with real checksum *)
  let header = Bytes.create header_size in
  pack_uint32_be header 0 pkt_magic;
  pack_uint32_be header 4 computed_checksum;
  pack_uint32_be header 8 channel_id;
  pack_uint32_be header 12 message_id;
  pack_uint32_be header 16 length;
  Bytes.to_string header ^ payload ^ String.make 1 (Char.of_int_exn term)

let sendall fd data =
  let bytes = Bytes.of_string data in
  let len = Bytes.length bytes in
  let rec write_all pos =
    if pos < len then
      let written = Core_unix.write fd ~buf:bytes ~pos ~len:(len - pos) in
      write_all (pos + written)
  in
  write_all 0

(* --- Constants tests --- *)

let test_magic () = Alcotest.(check int32) "MAGIC" 0x4845474Cl magic

let test_reply_bit () =
  Alcotest.(check int32) "REPLY_BIT" (Int32.shift_left 1l 31) reply_bit

let test_terminator () = Alcotest.(check int) "TERMINATOR" 0x0A terminator

let test_close_channel_message_id () =
  Alcotest.(check int32)
    "CLOSE_CHANNEL_MESSAGE_ID"
    Int32.(shift_left 1l 31 - 1l)
    close_channel_message_id

let test_close_channel_payload () =
  Alcotest.(check string)
    "CLOSE_CHANNEL_PAYLOAD" (String.make 1 '\xFE') close_channel_payload

(* --- CRC32 tests --- *)

let test_crc32_known_vector () =
  (* "hello world" -> 0x0D4A1185, matches Python's zlib.crc32 *)
  let crc = compute_crc32 "hello world" in
  Alcotest.(check int32) "CRC32 hello world" 0x0D4A1185l crc

let test_crc32_empty () =
  let crc = compute_crc32 "" in
  Alcotest.(check int32) "CRC32 empty" 0x00000000l crc

let test_crc32_single_byte () =
  (* Python: zlib.crc32(b'\x00') & 0xFFFFFFFF = 0xD202EF8D *)
  let crc = compute_crc32 "\x00" in
  Alcotest.(check int32) "CRC32 zero byte" 0xD202EF8Dl crc

let test_crc32_parts_matches_single () =
  let a = "hello " and b = "world" in
  let single = compute_crc32 (a ^ b) in
  let parts = compute_crc32_parts a b in
  Alcotest.(check int32) "parts matches single" single parts

let test_crc32_parts_empty_segments () =
  let crc1 = compute_crc32_parts "" "hello" in
  let crc2 = compute_crc32_parts "hello" "" in
  let crc3 = compute_crc32 "hello" in
  Alcotest.(check int32) "empty first" crc3 crc1;
  Alcotest.(check int32) "empty second" crc3 crc2

(* --- Packet round-trip tests --- *)

let packet_testable = Alcotest.testable pp_packet equal_packet

let test_roundtrip_simple () =
  with_socket_pair (fun reader writer ->
      let pkt =
        {
          channel_id = 1l;
          message_id = 2l;
          is_reply = false;
          payload = "hello";
        }
      in
      write_packet writer pkt;
      let pkt2 = read_packet reader in
      Alcotest.(check packet_testable) "round-trip simple" pkt pkt2)

let test_roundtrip_empty_payload () =
  with_socket_pair (fun reader writer ->
      let pkt =
        { channel_id = 0l; message_id = 1l; is_reply = false; payload = "" }
      in
      write_packet writer pkt;
      let pkt2 = read_packet reader in
      Alcotest.(check packet_testable) "round-trip empty payload" pkt pkt2)

let test_roundtrip_large_payload () =
  with_socket_pair (fun reader writer ->
      let payload = String.make 65536 'X' in
      let pkt =
        { channel_id = 42l; message_id = 99l; is_reply = false; payload }
      in
      (* Write in a separate thread to avoid deadlock: the 65KB payload exceeds
         the OS socket buffer, so write_packet blocks until the reader drains
         some data. *)
      let writer_thread =
        Thread.create (fun () -> write_packet writer pkt) ()
      in
      let pkt2 = read_packet reader in
      Thread.join writer_thread;
      Alcotest.(check packet_testable) "round-trip large payload" pkt pkt2)

let test_roundtrip_max_channel_id () =
  with_socket_pair (fun reader writer ->
      let pkt =
        {
          channel_id = 0xFFFFFFFFl;
          message_id = 1l;
          is_reply = false;
          payload = "test";
        }
      in
      write_packet writer pkt;
      let pkt2 = read_packet reader in
      Alcotest.(check packet_testable) "round-trip max channel" pkt pkt2)

let test_roundtrip_reply_bit () =
  with_socket_pair (fun reader writer ->
      let pkt =
        {
          channel_id = 5l;
          message_id = 10l;
          is_reply = true;
          payload = "reply";
        }
      in
      write_packet writer pkt;
      let pkt2 = read_packet reader in
      Alcotest.(check packet_testable) "round-trip reply" pkt pkt2)

let test_roundtrip_reply_bit_unset () =
  with_socket_pair (fun reader writer ->
      let pkt =
        {
          channel_id = 5l;
          message_id = 10l;
          is_reply = false;
          payload = "request";
        }
      in
      write_packet writer pkt;
      let pkt2 = read_packet reader in
      Alcotest.(check packet_testable) "round-trip non-reply" pkt pkt2)

(* --- Error case tests --- *)

let test_invalid_magic () =
  with_socket_pair (fun reader writer ->
      let raw = make_raw_packet ~pkt_magic:0xDEADBEEFl () in
      sendall writer raw;
      let exn_raised = ref false in
      (try
         let _ = read_packet reader in
         ()
       with Failure msg ->
         exn_raised := true;
         Alcotest.(check bool)
           "contains 'Invalid magic number'" true
           (contains_substring msg "Invalid magic number"));
      Alcotest.(check bool) "exception raised" true !exn_raised)

let test_invalid_terminator () =
  with_socket_pair (fun reader writer ->
      let raw = make_raw_packet ~term:0xFF () in
      sendall writer raw;
      let exn_raised = ref false in
      (try
         let _ = read_packet reader in
         ()
       with Failure msg ->
         exn_raised := true;
         Alcotest.(check bool)
           "contains 'Invalid terminator'" true
           (contains_substring msg "Invalid terminator"));
      Alcotest.(check bool) "exception raised" true !exn_raised)

let test_bad_checksum () =
  with_socket_pair (fun reader writer ->
      let raw = make_raw_packet ~checksum:0x12345678l () in
      sendall writer raw;
      let exn_raised = ref false in
      (try
         let _ = read_packet reader in
         ()
       with Failure msg ->
         exn_raised := true;
         Alcotest.(check bool)
           "contains 'Checksum mismatch'" true
           (contains_substring msg "Checksum mismatch"));
      Alcotest.(check bool) "exception raised" true !exn_raised)

(* --- recv_exact tests --- *)

let test_recv_exact_zero () =
  with_socket_pair (fun reader _writer ->
      let result = recv_exact reader 0 in
      Alcotest.(check int) "empty bytes" 0 (Bytes.length result))

let test_recv_exact_partial_close () =
  let reader, writer =
    Core_unix.socketpair ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()
  in
  sendall writer "abc";
  Core_unix.close writer;
  let exn_raised = ref false in
  (try
     let _ = recv_exact reader 10 in
     ()
   with Connection_closed _ -> exn_raised := true);
  Core_unix.close reader;
  Alcotest.(check bool) "Connection_closed raised" true !exn_raised

let test_recv_exact_no_data_close () =
  let reader, writer =
    Core_unix.socketpair ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()
  in
  Core_unix.close writer;
  let exn_raised = ref false in
  (try
     let _ = recv_exact reader 10 in
     ()
   with Partial_packet _ -> exn_raised := true);
  Core_unix.close reader;
  Alcotest.(check bool) "Partial_packet raised" true !exn_raised

(* --- pp_packet and equal_packet tests --- *)

let test_pp_packet () =
  let pkt =
    { channel_id = 1l; message_id = 2l; is_reply = false; payload = "hi" }
  in
  let s = Format.asprintf "%a" pp_packet pkt in
  Alcotest.(check bool) "pp_packet output non-empty" true (String.length s > 0)

let test_equal_packet_different () =
  let pkt1 =
    { channel_id = 1l; message_id = 2l; is_reply = false; payload = "hi" }
  in
  let pkt2 =
    { channel_id = 1l; message_id = 2l; is_reply = true; payload = "hi" }
  in
  let pkt3 =
    { channel_id = 1l; message_id = 3l; is_reply = false; payload = "hi" }
  in
  let pkt4 =
    { channel_id = 2l; message_id = 2l; is_reply = false; payload = "hi" }
  in
  let pkt5 =
    { channel_id = 1l; message_id = 2l; is_reply = false; payload = "bye" }
  in
  Alcotest.(check bool) "diff is_reply" false (equal_packet pkt1 pkt2);
  Alcotest.(check bool) "diff message_id" false (equal_packet pkt1 pkt3);
  Alcotest.(check bool) "diff channel_id" false (equal_packet pkt1 pkt4);
  Alcotest.(check bool) "diff payload" false (equal_packet pkt1 pkt5);
  Alcotest.(check bool) "same" true (equal_packet pkt1 pkt1)

(* --- Pack/unpack tests --- *)

let test_pack_unpack_uint32 () =
  let values = [ 0l; 1l; 0x7FFFFFFFl; 0x80000000l; 0xFFFFFFFFl ] in
  List.iter values ~f:(fun v ->
      let buf = Bytes.create 4 in
      pack_uint32_be buf 0 v;
      let result = unpack_uint32_be buf 0 in
      Alcotest.(check int32) (sprintf "pack/unpack 0x%lX" v) v result)

let tests =
  [
    (* Constants *)
    Alcotest.test_case "magic constant" `Quick test_magic;
    Alcotest.test_case "reply_bit constant" `Quick test_reply_bit;
    Alcotest.test_case "terminator constant" `Quick test_terminator;
    Alcotest.test_case "close_channel_message_id" `Quick
      test_close_channel_message_id;
    Alcotest.test_case "close_channel_payload" `Quick test_close_channel_payload;
    (* CRC32 *)
    Alcotest.test_case "CRC32 known vector" `Quick test_crc32_known_vector;
    Alcotest.test_case "CRC32 empty string" `Quick test_crc32_empty;
    Alcotest.test_case "CRC32 single byte" `Quick test_crc32_single_byte;
    Alcotest.test_case "CRC32 parts matches single" `Quick
      test_crc32_parts_matches_single;
    Alcotest.test_case "CRC32 parts empty segments" `Quick
      test_crc32_parts_empty_segments;
    (* Packet round-trip *)
    Alcotest.test_case "round-trip simple" `Quick test_roundtrip_simple;
    Alcotest.test_case "round-trip empty payload" `Quick
      test_roundtrip_empty_payload;
    Alcotest.test_case "round-trip large payload" `Quick
      test_roundtrip_large_payload;
    Alcotest.test_case "round-trip max channel" `Quick
      test_roundtrip_max_channel_id;
    Alcotest.test_case "round-trip reply bit set" `Quick
      test_roundtrip_reply_bit;
    Alcotest.test_case "round-trip reply bit unset" `Quick
      test_roundtrip_reply_bit_unset;
    (* Error cases *)
    Alcotest.test_case "invalid magic" `Quick test_invalid_magic;
    Alcotest.test_case "invalid terminator" `Quick test_invalid_terminator;
    Alcotest.test_case "bad checksum" `Quick test_bad_checksum;
    (* recv_exact *)
    Alcotest.test_case "recv_exact zero bytes" `Quick test_recv_exact_zero;
    Alcotest.test_case "recv_exact partial close" `Quick
      test_recv_exact_partial_close;
    Alcotest.test_case "recv_exact no data close" `Quick
      test_recv_exact_no_data_close;
    (* pp_packet, equal_packet *)
    Alcotest.test_case "pp_packet" `Quick test_pp_packet;
    Alcotest.test_case "equal_packet different fields" `Quick
      test_equal_packet_different;
    (* pack/unpack *)
    Alcotest.test_case "pack/unpack uint32" `Quick test_pack_unpack_uint32;
  ]
