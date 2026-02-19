let expect_failure msg f =
  match f () with
  | _ -> Alcotest.fail (msg ^ ": expected Failure exception")
  | exception Failure _ -> ()
  | exception exn ->
      Alcotest.failf "%s: expected Failure, got %s" msg (Printexc.to_string exn)

let expect_exn msg f =
  match f () with
  | _ -> Alcotest.fail (msg ^ ": expected exception")
  | exception _ -> ()

(* ================================================================ *)
(* CBOR encoding/decoding tests                                     *)
(* ================================================================ *)

let roundtrip v =
  let encoded = Hegel.Cbor.encode_to_string v in
  let decoded = Hegel.Cbor.decode_string encoded in
  decoded

let test_cbor_unsigned () =
  assert (roundtrip (Hegel.Cbor.Unsigned 0) = Hegel.Cbor.Unsigned 0);
  assert (roundtrip (Hegel.Cbor.Unsigned 1) = Hegel.Cbor.Unsigned 1);
  assert (roundtrip (Hegel.Cbor.Unsigned 23) = Hegel.Cbor.Unsigned 23);
  assert (roundtrip (Hegel.Cbor.Unsigned 24) = Hegel.Cbor.Unsigned 24);
  assert (roundtrip (Hegel.Cbor.Unsigned 255) = Hegel.Cbor.Unsigned 255);
  assert (roundtrip (Hegel.Cbor.Unsigned 256) = Hegel.Cbor.Unsigned 256);
  assert (roundtrip (Hegel.Cbor.Unsigned 65535) = Hegel.Cbor.Unsigned 65535);
  assert (roundtrip (Hegel.Cbor.Unsigned 65536) = Hegel.Cbor.Unsigned 65536);
  assert (roundtrip (Hegel.Cbor.Unsigned 1000000) = Hegel.Cbor.Unsigned 1000000)

let test_cbor_negative () =
  assert (roundtrip (Hegel.Cbor.Negative (-1)) = Hegel.Cbor.Negative (-1));
  assert (roundtrip (Hegel.Cbor.Negative (-10)) = Hegel.Cbor.Negative (-10));
  assert (roundtrip (Hegel.Cbor.Negative (-100)) = Hegel.Cbor.Negative (-100));
  assert (roundtrip (Hegel.Cbor.Negative (-1000)) = Hegel.Cbor.Negative (-1000))

let test_cbor_text () =
  assert (roundtrip (Hegel.Cbor.Text "") = Hegel.Cbor.Text "");
  assert (roundtrip (Hegel.Cbor.Text "hello") = Hegel.Cbor.Text "hello");
  let long_str = String.make 300 'x' in
  assert (roundtrip (Hegel.Cbor.Text long_str) = Hegel.Cbor.Text long_str)

let test_cbor_bytes () =
  assert (roundtrip (Hegel.Cbor.Bytes "") = Hegel.Cbor.Bytes "");
  assert (
    roundtrip (Hegel.Cbor.Bytes "\x00\x01\x02")
    = Hegel.Cbor.Bytes "\x00\x01\x02")

let test_cbor_array () =
  assert (roundtrip (Hegel.Cbor.Array []) = Hegel.Cbor.Array []);
  assert (
    roundtrip
      (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ])
    = Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ]);
  assert (
    roundtrip
      (Hegel.Cbor.Array [ Hegel.Cbor.Array [ Hegel.Cbor.Text "nested" ] ])
    = Hegel.Cbor.Array [ Hegel.Cbor.Array [ Hegel.Cbor.Text "nested" ] ])

let test_cbor_map () =
  assert (roundtrip (Hegel.Cbor.Map []) = Hegel.Cbor.Map []);
  assert (
    roundtrip
      (Hegel.Cbor.Map [ (Hegel.Cbor.Text "key", Hegel.Cbor.Unsigned 42) ])
    = Hegel.Cbor.Map [ (Hegel.Cbor.Text "key", Hegel.Cbor.Unsigned 42) ])

let test_cbor_bool () =
  assert (roundtrip (Hegel.Cbor.Bool true) = Hegel.Cbor.Bool true);
  assert (roundtrip (Hegel.Cbor.Bool false) = Hegel.Cbor.Bool false)

let test_cbor_null () = assert (roundtrip Hegel.Cbor.Null = Hegel.Cbor.Null)

let test_cbor_float () =
  assert (roundtrip (Hegel.Cbor.Float 0.0) = Hegel.Cbor.Float 0.0);
  assert (roundtrip (Hegel.Cbor.Float 1.5) = Hegel.Cbor.Float 1.5);
  assert (roundtrip (Hegel.Cbor.Float (-1.5)) = Hegel.Cbor.Float (-1.5));
  assert (roundtrip (Hegel.Cbor.Float infinity) = Hegel.Cbor.Float infinity);
  assert (
    roundtrip (Hegel.Cbor.Float neg_infinity) = Hegel.Cbor.Float neg_infinity);
  let nan_rt = roundtrip (Hegel.Cbor.Float nan) in
  assert (match nan_rt with Hegel.Cbor.Float f -> Float.is_nan f | _ -> false)

let test_cbor_tag () =
  assert (
    roundtrip (Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))
    = Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))

let test_cbor_encode_head_sizes () =
  let rt_text n =
    let s = String.make n 'a' in
    roundtrip (Hegel.Cbor.Text s) = Hegel.Cbor.Text s
  in
  assert (rt_text 5);
  assert (rt_text 100);
  assert (rt_text 300);
  assert (rt_text 70000)

let test_cbor_half_float () =
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x3C);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (v = Hegel.Cbor.Float 1.0);
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0xC0);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (v = Hegel.Cbor.Float (-2.0));
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x7C);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (v = Hegel.Cbor.Float infinity);
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0xFC);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (v = Hegel.Cbor.Float neg_infinity);
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x7E);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (match v with Hegel.Cbor.Float f -> Float.is_nan f | _ -> false);
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x00);
  Buffer.add_char buf (Char.chr 0x01);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (match v with Hegel.Cbor.Float f -> f > 0.0 && f < 0.001 | _ -> false);
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x80);
  Buffer.add_char buf (Char.chr 0x01);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (
    match v with Hegel.Cbor.Float f -> f < 0.0 && f > -0.001 | _ -> false)

let test_cbor_single_float () =
  let buf = Buffer.create 5 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 26));
  Buffer.add_char buf (Char.chr 0x3F);
  Buffer.add_char buf (Char.chr 0x80);
  Buffer.add_char buf (Char.chr 0x00);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  assert (v = Hegel.Cbor.Float 1.0)

let test_cbor_decode_errors () =
  let buf = Buffer.create 1 in
  Buffer.add_char buf (Char.chr ((0 lsl 5) lor 28));
  expect_failure "unsupported additional info" (fun () ->
      Hegel.Cbor.decode_string (Buffer.contents buf));
  let buf = Buffer.create 1 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 24));
  Buffer.add_char buf (Char.chr 0);
  expect_failure "unsupported simple value" (fun () ->
      Hegel.Cbor.decode_string (Buffer.contents buf))

(* ================================================================ *)
(* CBOR helpers tests                                               *)
(* ================================================================ *)

let test_cbor_map_get () =
  let m =
    Hegel.Cbor.Map
      [
        (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1);
        (Hegel.Cbor.Text "b", Hegel.Cbor.Unsigned 2);
      ]
  in
  assert (Hegel.Cbor.map_get m "a" = Some (Hegel.Cbor.Unsigned 1));
  assert (Hegel.Cbor.map_get m "b" = Some (Hegel.Cbor.Unsigned 2));
  assert (Hegel.Cbor.map_get m "c" = None);
  assert (Hegel.Cbor.map_get (Hegel.Cbor.Unsigned 5) "a" = None);
  let m2 =
    Hegel.Cbor.Map
      [
        (Hegel.Cbor.Unsigned 1, Hegel.Cbor.Text "num");
        (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1);
      ]
  in
  assert (Hegel.Cbor.map_get m2 "a" = Some (Hegel.Cbor.Unsigned 1))

let test_cbor_map_get_exn () =
  let m = Hegel.Cbor.Map [ (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1) ] in
  assert (Hegel.Cbor.map_get_exn m "a" = Hegel.Cbor.Unsigned 1);
  expect_failure "map_get_exn missing" (fun () ->
      ignore (Hegel.Cbor.map_get_exn m "b"))

let test_cbor_as_helpers () =
  assert (Hegel.Cbor.as_text (Hegel.Cbor.Text "hi") = Some "hi");
  assert (Hegel.Cbor.as_text (Hegel.Cbor.Unsigned 1) = None);
  assert (Hegel.Cbor.as_bool (Hegel.Cbor.Bool true) = Some true);
  assert (Hegel.Cbor.as_bool (Hegel.Cbor.Bool false) = Some false);
  assert (Hegel.Cbor.as_bool (Hegel.Cbor.Unsigned 1) = None);
  assert (Hegel.Cbor.as_float (Hegel.Cbor.Float 1.5) = Some 1.5);
  assert (Hegel.Cbor.as_float (Hegel.Cbor.Unsigned 5) = Some 5.0);
  assert (Hegel.Cbor.as_float (Hegel.Cbor.Negative (-3)) = Some (-3.0));
  assert (Hegel.Cbor.as_float (Hegel.Cbor.Text "x") = None);
  assert (Hegel.Cbor.as_array (Hegel.Cbor.Array []) = Some []);
  assert (Hegel.Cbor.as_array (Hegel.Cbor.Unsigned 1) = None);
  assert (Hegel.Cbor.as_map (Hegel.Cbor.Map []) = Some []);
  assert (Hegel.Cbor.as_map (Hegel.Cbor.Unsigned 1) = None);
  assert (Hegel.Cbor.as_int (Hegel.Cbor.Unsigned 42) = Some 42);
  assert (Hegel.Cbor.as_int (Hegel.Cbor.Negative (-5)) = Some (-5));
  assert (Hegel.Cbor.as_int (Hegel.Cbor.Float 3.0) = Some 3);
  assert (Hegel.Cbor.as_int (Hegel.Cbor.Float 3.5) = None);
  assert (Hegel.Cbor.as_int (Hegel.Cbor.Text "x") = None)

let test_cbor_to_diagnostic () =
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Unsigned 42) = "42");
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Negative (-5)) = "-5");
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Text "hi") = "\"hi\"");
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bytes "\x00\xff") = "h'00ff'");
  assert (
    Hegel.Cbor.to_diagnostic
      (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ])
    = "[1, 2]");
  assert (
    Hegel.Cbor.to_diagnostic
      (Hegel.Cbor.Map [ (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1) ])
    = "{\"a\": 1}");
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bool true) = "true");
  assert (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bool false) = "false");
  assert (Hegel.Cbor.to_diagnostic Hegel.Cbor.Null = "null");
  assert (
    Hegel.Cbor.to_diagnostic (Hegel.Cbor.Tag (258, Hegel.Cbor.Array []))
    = "258([])");
  let _ = Hegel.Cbor.to_diagnostic (Hegel.Cbor.Float 1.5) in
  ()

(* ================================================================ *)
(* CRC32 tests                                                      *)
(* ================================================================ *)

let test_crc32 () =
  assert (Hegel.Crc32.compute "" = 0x00000000l);
  assert (Hegel.Crc32.compute "123456789" = 0xCBF43926l);
  let _ = Hegel.Crc32.compute "\x00" in
  let _ = Hegel.Crc32.compute "a" in
  ()

(* ================================================================ *)
(* State tests                                                      *)
(* ================================================================ *)

let test_state_last_run () =
  Hegel.State.set_last_run false;
  assert (not (Hegel.State.get_last_run ()));
  Hegel.State.set_last_run true;
  assert (Hegel.State.get_last_run ());
  Hegel.State.set_last_run false;
  assert (not (Hegel.State.get_last_run ()))

let test_state_test_aborted () =
  Hegel.State.set_test_aborted false;
  assert (not (Hegel.State.get_test_aborted ()));
  Hegel.State.set_test_aborted true;
  assert (Hegel.State.get_test_aborted ());
  Hegel.State.set_test_aborted false;
  assert (not (Hegel.State.get_test_aborted ()))

let test_state_generated_values () =
  let vs = Hegel.State.take_generated_values () in
  assert (vs = []);
  Hegel.State.buffer_generated_value "a";
  Hegel.State.buffer_generated_value "b";
  let vs = Hegel.State.take_generated_values () in
  assert (vs = [ "a"; "b" ]);
  let vs2 = Hegel.State.take_generated_values () in
  assert (vs2 = [])

let test_state_connection () =
  assert (Hegel.State.get_connection () = None);
  expect_failure "get_channel with no connection" (fun () ->
      ignore (Hegel.State.get_channel ()));
  expect_failure "increment_span_depth no connection" (fun () ->
      Hegel.State.increment_span_depth ());
  expect_failure "decrement_span_depth no connection" (fun () ->
      Hegel.State.decrement_span_depth ())

(* ================================================================ *)
(* Gen helper tests (things that don't need the server)             *)
(* ================================================================ *)

let test_gen_string_contains () =
  assert (Hegel.Gen.string_contains "hello world" "world");
  assert (not (Hegel.Gen.string_contains "hello world" "xyz"));
  assert (not (Hegel.Gen.string_contains "hi" "hello"));
  assert (Hegel.Gen.string_contains "hello" "");
  assert (Hegel.Gen.string_contains "hello" "hel");
  assert (Hegel.Gen.string_contains "hello" "llo");
  assert (Hegel.Gen.string_contains "hello" "hello");
  assert (Hegel.Gen.string_contains "" "")

let test_gen_base64_decode () =
  assert (Hegel.Gen.base64_decode "SGVsbG8=" = "Hello");
  assert (Hegel.Gen.base64_decode "" = "");
  assert (Hegel.Gen.base64_decode "YWJj" = "abc");
  assert (Hegel.Gen.base64_decode "YWI=" = "ab");
  assert (Hegel.Gen.base64_decode "YQ==" = "a");
  assert (Hegel.Gen.base64_decode "TWFu" = "Man")

let test_gen_cbor_helpers () =
  assert (Hegel.Gen.cbor_int_value (Hegel.Cbor.Unsigned 42) = 42);
  assert (Hegel.Gen.cbor_int_value (Hegel.Cbor.Negative (-5)) = -5);
  assert (Hegel.Gen.cbor_int_value (Hegel.Cbor.Float 3.0) = 3);
  expect_failure "cbor_int_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_int_value (Hegel.Cbor.Text "x")));
  assert (Hegel.Gen.cbor_float_value (Hegel.Cbor.Float 1.5) = 1.5);
  assert (Hegel.Gen.cbor_float_value (Hegel.Cbor.Unsigned 5) = 5.0);
  assert (Hegel.Gen.cbor_float_value (Hegel.Cbor.Negative (-3)) = -3.0);
  expect_failure "cbor_float_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_float_value (Hegel.Cbor.Text "x")));
  assert (Hegel.Gen.cbor_text_value (Hegel.Cbor.Text "hi") = "hi");
  expect_failure "cbor_text_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_text_value (Hegel.Cbor.Unsigned 1)));
  assert (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Bool true) = true);
  assert (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Bool false) = false);
  expect_failure "cbor_bool_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Unsigned 1)));
  assert (Hegel.Gen.cbor_array_value (Hegel.Cbor.Array []) = []);
  assert (
    Hegel.Gen.cbor_array_value (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ])
    = [ Hegel.Cbor.Unsigned 1 ]);
  assert (
    Hegel.Gen.cbor_array_value
      (Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))
    = [ Hegel.Cbor.Unsigned 1 ]);
  expect_failure "cbor_array_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_array_value (Hegel.Cbor.Text "x")))

let test_gen_one_of_empty () =
  expect_failure "one_of empty list" (fun () -> ignore (Hegel.Gen.one_of []))

let test_gen_sampled_from_empty () =
  expect_failure "sampled_from empty list" (fun () ->
      ignore (Hegel.Gen.sampled_from []))

(* ================================================================ *)
(* Protocol encoding tests (packet write/read via socket pair)      *)
(* ================================================================ *)

let test_protocol_packet_roundtrip () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let pkt =
        {
          Hegel.Protocol.channel = 42;
          message_id = 7;
          is_reply = false;
          payload = "hello";
        }
      in
      Hegel.Protocol.write_packet wr pkt;
      let got = Hegel.Protocol.read_packet rd in
      assert (got.channel = 42);
      assert (got.message_id = 7);
      assert (not got.is_reply);
      assert (got.payload = "hello"))

let test_protocol_packet_reply () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let pkt =
        {
          Hegel.Protocol.channel = 1;
          message_id = 3;
          is_reply = true;
          payload = "world";
        }
      in
      Hegel.Protocol.write_packet wr pkt;
      let got = Hegel.Protocol.read_packet rd in
      assert (got.channel = 1);
      assert (got.message_id = 3);
      assert got.is_reply;
      assert (got.payload = "world"))

let test_protocol_packet_empty_payload () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let pkt =
        {
          Hegel.Protocol.channel = 0;
          message_id = 0;
          is_reply = false;
          payload = "";
        }
      in
      Hegel.Protocol.write_packet wr pkt;
      let got = Hegel.Protocol.read_packet rd in
      assert (got.payload = ""))

let test_protocol_connection () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let control = Hegel.Protocol.Connection.control_channel conn in
      assert (Hegel.Protocol.Channel.channel_id control = 0);
      let ch1 = Hegel.Protocol.Connection.new_channel conn in
      let ch1_id = Hegel.Protocol.Channel.channel_id ch1 in
      assert (ch1_id land 1 = 1);
      let ch2 = Hegel.Protocol.Connection.new_channel conn in
      let ch2_id = Hegel.Protocol.Channel.channel_id ch2 in
      assert (ch2_id <> ch1_id);
      assert (ch2_id land 1 = 1))

let test_protocol_channel_send_receive () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn_send = Hegel.Protocol.Connection.create wr in
      let ch_send = Hegel.Protocol.Connection.control_channel conn_send in
      let msg_id = Hegel.Protocol.Channel.send_request ch_send "test payload" in
      assert (msg_id > 0);
      let got = Hegel.Protocol.read_packet rd in
      assert (not got.is_reply);
      assert (got.payload = "test payload");
      assert (got.message_id = msg_id))

let test_protocol_connect_channel () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.connect_channel conn 99 in
      assert (Hegel.Protocol.Channel.channel_id ch = 99))

let test_protocol_channel_close () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.Protocol.Channel.close ch;
      let got = Hegel.Protocol.read_packet rd in
      assert (got.payload = "\xFE"))

let test_protocol_connection_close () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Hegel.Protocol.Connection.create wr in
  Hegel.Protocol.Connection.close conn;
  let buf = Bytes.create 1 in
  let got =
    try
      let n = Unix.read rd buf 0 1 in
      n = 0
    with Unix.Unix_error _ -> true
  in
  assert got;
  Unix.close rd

let test_protocol_send_response () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.Protocol.Channel.send_response ch 42 "response data";
      let got = Hegel.Protocol.read_packet rd in
      assert got.is_reply;
      assert (got.message_id = 42);
      assert (got.payload = "response data"))

let test_protocol_receive_response () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn_recv = Hegel.Protocol.Connection.create rd1 in
      let ch_recv = Hegel.Protocol.Connection.control_channel conn_recv in
      let reply_pkt =
        {
          Hegel.Protocol.channel = 0;
          message_id = 99;
          is_reply = true;
          payload = "the reply";
        }
      in
      Hegel.Protocol.write_packet wr1 reply_pkt;
      let got = Hegel.Protocol.Channel.receive_response ch_recv 99 in
      assert (got = "the reply"))

let test_protocol_receive_request () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn_recv = Hegel.Protocol.Connection.create rd1 in
      let ch_recv = Hegel.Protocol.Connection.control_channel conn_recv in
      let request_pkt =
        {
          Hegel.Protocol.channel = 0;
          message_id = 20;
          is_reply = false;
          payload = "the request";
        }
      in
      Hegel.Protocol.write_packet wr1 request_pkt;
      let msg_id, payload = Hegel.Protocol.Channel.receive_request ch_recv in
      assert (payload = "the request");
      assert (msg_id = 20))

let test_protocol_channel_routing () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd1 in
      let pkt_ch5 =
        {
          Hegel.Protocol.channel = 5;
          message_id = 1;
          is_reply = false;
          payload = "for ch5";
        }
      in
      Hegel.Protocol.write_packet wr1 pkt_ch5;
      let pkt_ch0 =
        {
          Hegel.Protocol.channel = 0;
          message_id = 2;
          is_reply = false;
          payload = "for ch0";
        }
      in
      Hegel.Protocol.write_packet wr1 pkt_ch0;
      let ch0 = Hegel.Protocol.Connection.control_channel conn in
      let msg_id, payload = Hegel.Protocol.Channel.receive_request ch0 in
      assert (payload = "for ch0");
      assert (msg_id = 2);
      let ch5 = Hegel.Protocol.Connection.connect_channel conn 5 in
      let msg_id5, payload5 = Hegel.Protocol.Channel.receive_request ch5 in
      assert (payload5 = "for ch5");
      assert (msg_id5 = 1))

let test_protocol_pending_queue () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd1 in
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 3;
          message_id = 1;
          is_reply = false;
          payload = "first for ch3";
        };
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 3;
          message_id = 2;
          is_reply = false;
          payload = "second for ch3";
        };
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 0;
          message_id = 3;
          is_reply = false;
          payload = "for ch0";
        };
      let ch0 = Hegel.Protocol.Connection.control_channel conn in
      let _, p = Hegel.Protocol.Channel.receive_request ch0 in
      assert (p = "for ch0");
      let ch3 = Hegel.Protocol.Connection.connect_channel conn 3 in
      let _, p1 = Hegel.Protocol.Channel.receive_request ch3 in
      assert (p1 = "first for ch3");
      let _, p2 = Hegel.Protocol.Channel.receive_request ch3 in
      assert (p2 = "second for ch3"))

(* ================================================================ *)
(* Gen.Labels tests                                                 *)
(* ================================================================ *)

let test_labels () =
  assert (Hegel.Gen.Labels.list = 1);
  assert (Hegel.Gen.Labels.list_element = 2);
  assert (Hegel.Gen.Labels.set = 3);
  assert (Hegel.Gen.Labels.set_element = 4);
  assert (Hegel.Gen.Labels.map = 5);
  assert (Hegel.Gen.Labels.map_entry = 6);
  assert (Hegel.Gen.Labels.tuple = 7);
  assert (Hegel.Gen.Labels.one_of = 8);
  assert (Hegel.Gen.Labels.optional = 9);
  assert (Hegel.Gen.Labels.fixed_dict = 10);
  assert (Hegel.Gen.Labels.flat_map = 11);
  assert (Hegel.Gen.Labels.filter = 12);
  assert (Hegel.Gen.Labels.mapped = 13);
  assert (Hegel.Gen.Labels.sampled_from = 14)

(* ================================================================ *)
(* Protocol error path tests                                        *)
(* ================================================================ *)

let test_protocol_invalid_magic () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let header = Bytes.create 21 in
      Bytes.fill header 0 21 '\x00';
      Bytes.set header 0 (Char.chr 0xDE);
      Bytes.set header 1 (Char.chr 0xAD);
      Bytes.set header 2 (Char.chr 0xBE);
      Bytes.set header 3 (Char.chr 0xEF);
      let s = Bytes.to_string header in
      let len = String.length s in
      let written = ref 0 in
      while !written < len do
        let n = Unix.write_substring wr s !written (len - !written) in
        written := !written + n
      done;
      expect_failure "read_packet invalid magic" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_invalid_terminator () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let payload = "" in
      let header = Bytes.create 20 in
      Bytes.set header 0 (Char.chr 0x48);
      Bytes.set header 1 (Char.chr 0x45);
      Bytes.set header 2 (Char.chr 0x47);
      Bytes.set header 3 (Char.chr 0x4C);
      Bytes.fill header 4 4 '\x00';
      Bytes.fill header 8 4 '\x00';
      Bytes.fill header 12 4 '\x00';
      Bytes.fill header 16 4 '\x00';
      let header_str = Bytes.to_string header in
      let checksum = Hegel.Crc32.compute (header_str ^ payload) in
      let cksum_i = Int32.to_int checksum in
      Bytes.set header 4 (Char.chr ((cksum_i lsr 24) land 0xff));
      Bytes.set header 5 (Char.chr ((cksum_i lsr 16) land 0xff));
      Bytes.set header 6 (Char.chr ((cksum_i lsr 8) land 0xff));
      Bytes.set header 7 (Char.chr (cksum_i land 0xff));
      let full = Bytes.to_string header ^ "\xFF" in
      let len = String.length full in
      let written = ref 0 in
      while !written < len do
        let n = Unix.write_substring wr full !written (len - !written) in
        written := !written + n
      done;
      expect_failure "read_packet invalid terminator" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_checksum_mismatch () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let header = Bytes.create 20 in
      Bytes.set header 0 (Char.chr 0x48);
      Bytes.set header 1 (Char.chr 0x45);
      Bytes.set header 2 (Char.chr 0x47);
      Bytes.set header 3 (Char.chr 0x4C);
      Bytes.set header 4 (Char.chr 0xFF);
      Bytes.set header 5 (Char.chr 0xFF);
      Bytes.set header 6 (Char.chr 0xFF);
      Bytes.set header 7 (Char.chr 0xFF);
      Bytes.fill header 8 12 '\x00';
      let full = Bytes.to_string header ^ "\x0A" in
      let len = String.length full in
      let written = ref 0 in
      while !written < len do
        let n = Unix.write_substring wr full !written (len - !written) in
        written := !written + n
      done;
      expect_failure "read_packet checksum mismatch" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_read_closed_fd () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.close wr;
  expect_failure "read_packet on closed connection" (fun () ->
      ignore (Hegel.Protocol.read_packet rd));
  Unix.close rd

let test_protocol_write_closed_fd () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.close rd;
  Unix.close wr;
  expect_exn "write_packet on closed fd" (fun () ->
      Hegel.Protocol.write_packet wr
        {
          Hegel.Protocol.channel = 0;
          message_id = 0;
          is_reply = false;
          payload = "test";
        })

let test_protocol_request_cbor_error () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn_send = Hegel.Protocol.Connection.create wr in
      let ch_send = Hegel.Protocol.Connection.control_channel conn_send in
      match Unix.fork () with
      | 0 ->
          let pkt = Hegel.Protocol.read_packet rd in
          let error_response =
            Hegel.Cbor.encode_to_string
              (Hegel.Cbor.Map
                 [
                   ( Hegel.Cbor.Text "error",
                     Hegel.Cbor.Text "something went wrong" );
                   (Hegel.Cbor.Text "type", Hegel.Cbor.Text "TestError");
                 ])
          in
          Hegel.Protocol.write_packet rd
            {
              Hegel.Protocol.channel = pkt.channel;
              message_id = pkt.message_id;
              is_reply = true;
              payload = error_response;
            };
          exit 0
      | pid ->
          expect_failure "request_cbor error response" (fun () ->
              ignore
                (Hegel.Protocol.Channel.request_cbor ch_send (Hegel.Cbor.Map [])));
          ignore (Unix.waitpid [] pid))

let test_protocol_request_cbor_non_text_error () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn_send = Hegel.Protocol.Connection.create wr in
      let ch_send = Hegel.Protocol.Connection.control_channel conn_send in
      match Unix.fork () with
      | 0 ->
          let pkt = Hegel.Protocol.read_packet rd in
          let error_response =
            Hegel.Cbor.encode_to_string
              (Hegel.Cbor.Map
                 [
                   (Hegel.Cbor.Text "error", Hegel.Cbor.Unsigned 42);
                   (Hegel.Cbor.Text "type", Hegel.Cbor.Text "TestError");
                 ])
          in
          Hegel.Protocol.write_packet rd
            {
              Hegel.Protocol.channel = pkt.channel;
              message_id = pkt.message_id;
              is_reply = true;
              payload = error_response;
            };
          exit 0
      | pid ->
          expect_failure "request_cbor non-text error" (fun () ->
              ignore
                (Hegel.Protocol.Channel.request_cbor ch_send (Hegel.Cbor.Map [])));
          ignore (Unix.waitpid [] pid))

let test_protocol_request_cbor_error_no_type () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn_send = Hegel.Protocol.Connection.create wr in
      let ch_send = Hegel.Protocol.Connection.control_channel conn_send in
      match Unix.fork () with
      | 0 ->
          let pkt = Hegel.Protocol.read_packet rd in
          let error_response =
            Hegel.Cbor.encode_to_string
              (Hegel.Cbor.Map
                 [
                   (Hegel.Cbor.Text "error", Hegel.Cbor.Text "something broke");
                 ])
          in
          Hegel.Protocol.write_packet rd
            {
              Hegel.Protocol.channel = pkt.channel;
              message_id = pkt.message_id;
              is_reply = true;
              payload = error_response;
            };
          exit 0
      | pid ->
          expect_failure "request_cbor error no type" (fun () ->
              ignore
                (Hegel.Protocol.Channel.request_cbor ch_send (Hegel.Cbor.Map [])));
          ignore (Unix.waitpid [] pid))

let test_protocol_request_cbor_no_result () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn_send = Hegel.Protocol.Connection.create wr in
      let ch_send = Hegel.Protocol.Connection.control_channel conn_send in
      match Unix.fork () with
      | 0 ->
          let pkt = Hegel.Protocol.read_packet rd in
          let response =
            Hegel.Cbor.encode_to_string
              (Hegel.Cbor.Map
                 [ (Hegel.Cbor.Text "data", Hegel.Cbor.Unsigned 99) ])
          in
          Hegel.Protocol.write_packet rd
            {
              Hegel.Protocol.channel = pkt.channel;
              message_id = pkt.message_id;
              is_reply = true;
              payload = response;
            };
          exit 0
      | pid ->
          let result =
            Hegel.Protocol.Channel.request_cbor ch_send (Hegel.Cbor.Map [])
          in
          assert (
            Hegel.Cbor.map_get result "data" = Some (Hegel.Cbor.Unsigned 99));
          ignore (Unix.waitpid [] pid))

(* ================================================================ *)
(* State connection lifecycle tests                                  *)
(* ================================================================ *)

let test_state_connection_lifecycle () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.State.set_connection ch;
      assert (Hegel.State.get_connection () <> None);
      let _ = Hegel.State.get_channel () in
      Hegel.State.increment_span_depth ();
      Hegel.State.decrement_span_depth ();
      Hegel.State.clear_connection ();
      assert (Hegel.State.get_connection () = None))

(* ================================================================ *)
(* Runner edge case tests                                           *)
(* ================================================================ *)

let test_extract_channel_id () =
  let valid =
    Hegel.Cbor.Map [ (Hegel.Cbor.Text "channel", Hegel.Cbor.Unsigned 42) ]
  in
  assert (Hegel.extract_channel_id valid = 42);
  let missing = Hegel.Cbor.Map [] in
  expect_failure "extract_channel_id missing" (fun () ->
      ignore (Hegel.extract_channel_id missing));
  let wrong_type =
    Hegel.Cbor.Map [ (Hegel.Cbor.Text "channel", Hegel.Cbor.Text "not an int") ]
  in
  expect_failure "extract_channel_id wrong type" (fun () ->
      ignore (Hegel.extract_channel_id wrong_type))

let test_find_hegel_path () =
  let _ = Hegel.find_hegel_path () in
  ()

let test_find_hegel_path_not_found () =
  let old_path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  Unix.putenv "PATH" "/nonexistent";
  assert (Hegel.find_hegel_path () = None);
  Unix.putenv "PATH" old_path

let test_run_no_hegel_path () =
  let old_path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  Unix.putenv "PATH" "/nonexistent";
  expect_failure "run without hegel path" (fun () -> Hegel.run (fun () -> ()));
  Unix.putenv "PATH" old_path

(* ================================================================ *)
(* Gen error path tests                                              *)
(* ================================================================ *)

let test_generate_raw_aborted () =
  Hegel.State.set_test_aborted true;
  expect_exn "generate_raw when aborted" (fun () ->
      ignore (Hegel.Gen.generate_raw (Hegel.Cbor.Map [])));
  Hegel.State.set_test_aborted false

let test_start_span_aborted () =
  Hegel.State.set_test_aborted true;
  expect_exn "start_span when aborted" (fun () -> Hegel.Gen.start_span 1);
  Hegel.State.set_test_aborted false

let test_base64_invalid_char () =
  expect_failure "base64 invalid char" (fun () ->
      ignore (Hegel.Gen.base64_decode "!!!!"))

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let with_mock_server test_fn server_fn =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Hegel.Protocol.Connection.create wr in
  let ch = Hegel.Protocol.Connection.control_channel conn in
  Hegel.State.set_connection ch;
  Hegel.State.set_test_aborted false;
  match Unix.fork () with
  | 0 ->
      Unix.close wr;
      (try server_fn rd with _ -> ());
      (try
         while true do
           let pkt = Hegel.Protocol.read_packet rd in
           let ok =
             Hegel.Cbor.encode_to_string
               (Hegel.Cbor.Map [ (Hegel.Cbor.Text "result", Hegel.Cbor.Null) ])
           in
           Hegel.Protocol.write_packet rd
             {
               Hegel.Protocol.channel = pkt.channel;
               message_id = pkt.message_id;
               is_reply = true;
               payload = ok;
             }
         done
       with _ -> ());
      Unix.close rd;
      exit 0
  | pid ->
      Unix.close rd;
      Fun.protect
        ~finally:(fun () ->
          (try Unix.close wr with _ -> ());
          ignore (Unix.waitpid [] pid);
          Hegel.State.set_test_aborted false;
          Hegel.State.clear_connection ())
        test_fn

let respond_with_stop_test rd =
  let pkt = Hegel.Protocol.read_packet rd in
  let error_response =
    Hegel.Cbor.encode_to_string
      (Hegel.Cbor.Map
         [
           (Hegel.Cbor.Text "error", Hegel.Cbor.Text "stopped");
           (Hegel.Cbor.Text "type", Hegel.Cbor.Text "StopTest");
         ])
  in
  Hegel.Protocol.write_packet rd
    {
      Hegel.Protocol.channel = pkt.channel;
      message_id = pkt.message_id;
      is_reply = true;
      payload = error_response;
    }

let test_generate_raw_stop_test () =
  with_mock_server
    (fun () ->
      expect_exn "generate_raw StopTest" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      assert (Hegel.State.get_test_aborted ()))
    respond_with_stop_test

let test_start_span_stop_test () =
  with_mock_server
    (fun () ->
      Hegel.State.increment_span_depth ();
      expect_exn "start_span StopTest" (fun () -> Hegel.Gen.start_span 1);
      assert (Hegel.State.get_test_aborted ()))
    respond_with_stop_test

let test_stop_span_error () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Hegel.Protocol.Connection.create wr in
  let ch = Hegel.Protocol.Connection.control_channel conn in
  Hegel.State.set_connection ch;
  Hegel.State.set_test_aborted false;
  Hegel.State.increment_span_depth ();
  Unix.close rd;
  Unix.close wr;
  Hegel.Gen.stop_span false;
  Hegel.State.clear_connection ()

let respond_with_error rd error_type error_msg =
  let pkt = Hegel.Protocol.read_packet rd in
  let error_response =
    Hegel.Cbor.encode_to_string
      (Hegel.Cbor.Map
         [
           (Hegel.Cbor.Text "error", Hegel.Cbor.Text error_msg);
           (Hegel.Cbor.Text "type", Hegel.Cbor.Text error_type);
         ])
  in
  Hegel.Protocol.write_packet rd
    {
      Hegel.Protocol.channel = pkt.channel;
      message_id = pkt.message_id;
      is_reply = true;
      payload = error_response;
    }

let test_generate_raw_overflow () =
  with_mock_server
    (fun () ->
      expect_exn "generate_raw overflow" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      assert (Hegel.State.get_test_aborted ()))
    (fun rd -> respond_with_error rd "overflow" "buffer overflow")

let test_generate_raw_overrun () =
  with_mock_server
    (fun () ->
      expect_exn "generate_raw overrun" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      assert (Hegel.State.get_test_aborted ()))
    (fun rd -> respond_with_error rd "Overrun" "data Overrun")

let respond_ok_then_custom rd n_ok custom_result =
  let ok =
    Hegel.Cbor.encode_to_string
      (Hegel.Cbor.Map [ (Hegel.Cbor.Text "result", Hegel.Cbor.Null) ])
  in
  for _ = 1 to n_ok do
    let pkt = Hegel.Protocol.read_packet rd in
    Hegel.Protocol.write_packet rd
      {
        Hegel.Protocol.channel = pkt.channel;
        message_id = pkt.message_id;
        is_reply = true;
        payload = ok;
      }
  done;
  let pkt = Hegel.Protocol.read_packet rd in
  let resp =
    Hegel.Cbor.encode_to_string
      (Hegel.Cbor.Map [ (Hegel.Cbor.Text "result", custom_result) ])
  in
  Hegel.Protocol.write_packet rd
    {
      Hegel.Protocol.channel = pkt.channel;
      message_id = pkt.message_id;
      is_reply = true;
      payload = resp;
    }

let test_new_collection_non_text () =
  with_mock_server
    (fun () ->
      expect_failure "new_collection non-text" (fun () ->
          let gen =
            Hegel.Gen.list ~min_size:1 ~max_size:3
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))
          in
          ignore (gen.generate ())))
    (fun rd -> respond_ok_then_custom rd 1 (Hegel.Cbor.Unsigned 42))

let test_collection_more_non_bool () =
  with_mock_server
    (fun () ->
      expect_failure "collection_more non-bool" (fun () ->
          let gen =
            Hegel.Gen.list ~min_size:1 ~max_size:3
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))
          in
          ignore (gen.generate ())))
    (fun rd ->
      let respond (pkt : Hegel.Protocol.packet) result =
        let resp =
          Hegel.Cbor.encode_to_string
            (Hegel.Cbor.Map [ (Hegel.Cbor.Text "result", result) ])
        in
        Hegel.Protocol.write_packet rd
          {
            Hegel.Protocol.channel = pkt.channel;
            message_id = pkt.message_id;
            is_reply = true;
            payload = resp;
          }
      in
      respond (Hegel.Protocol.read_packet rd) Hegel.Cbor.Null;
      respond (Hegel.Protocol.read_packet rd) (Hegel.Cbor.Text "test_coll");
      respond (Hegel.Protocol.read_packet rd) (Hegel.Cbor.Unsigned 42))

let test_non_stop_test_error_raises () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.close rd with _ -> ());
      try Unix.close wr with _ -> ())
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.State.clear_connection ();
      Hegel.State.set_connection ch;
      Hegel.State.set_test_aborted false;
      match Unix.fork () with
      | 0 ->
          let pkt = Hegel.Protocol.read_packet rd in
          let error_response =
            Hegel.Cbor.encode_to_string
              (Hegel.Cbor.Map
                 [
                   ( Hegel.Cbor.Text "error",
                     Hegel.Cbor.Text "something unexpected" );
                   (Hegel.Cbor.Text "type", Hegel.Cbor.Text "UnknownError");
                 ])
          in
          Hegel.Protocol.write_packet rd
            {
              Hegel.Protocol.channel = pkt.channel;
              message_id = pkt.message_id;
              is_reply = true;
              payload = error_response;
            };
          exit 0
      | pid ->
          expect_failure "non-StopTest error raises" (fun () ->
              ignore
                (Hegel.Gen.generate_raw
                   (Hegel.Cbor.Map
                      [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
          Hegel.State.clear_connection ();
          ignore (Unix.waitpid [] pid))

(* ================================================================ *)
(* receive_response with non-matching packets buffered               *)
(* ================================================================ *)

let test_protocol_receive_response_with_buffering () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd1 in
      let ch0 = Hegel.Protocol.Connection.control_channel conn in
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 5;
          message_id = 50;
          is_reply = true;
          payload = "for other channel";
        };
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 0;
          message_id = 99;
          is_reply = true;
          payload = "the reply";
        };
      let got = Hegel.Protocol.Channel.receive_response ch0 99 in
      assert (got = "the reply"))

(* ================================================================ *)
(* Protocol failwith path tests                                     *)
(* ================================================================ *)

let test_protocol_receive_response_wrong_type () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.Protocol.write_packet wr
        {
          Hegel.Protocol.channel = 0;
          message_id = 99;
          is_reply = false;
          payload = "not a reply";
        };
      expect_failure "receive_response non-reply" (fun () ->
          ignore (Hegel.Protocol.Channel.receive_response ch 99)))

let test_protocol_receive_request_unexpected_reply () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.Protocol.write_packet wr
        {
          Hegel.Protocol.channel = 0;
          message_id = 1;
          is_reply = true;
          payload = "unexpected reply";
        };
      expect_failure "receive_request unexpected reply" (fun () ->
          ignore (Hegel.Protocol.Channel.receive_request ch)))

(* ================================================================ *)
(* Mock hegel runner tests                                          *)
(* ================================================================ *)

let mock_hegel_path () =
  let candidates =
    [
      "_build/default/test/mock_hegel.exe";
      Filename.concat (Sys.getcwd ()) "_build/default/test/mock_hegel.exe";
      "../test/mock_hegel.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None -> failwith "mock_hegel.exe not found"

let run_with_mock_mode mode test_fn =
  let path = mock_hegel_path () in
  Unix.putenv "MOCK_HEGEL_MODE" mode;
  Fun.protect
    ~finally:(fun () -> Unix.putenv "MOCK_HEGEL_MODE" "")
    (fun () -> Hegel.run ~test_cases:1 ~hegel_path:path test_fn)

let test_runner_normal () = run_with_mock_mode "normal" (fun () -> ())

let test_runner_version_fail () =
  expect_failure "runner version fail" (fun () ->
      run_with_mock_mode "version_fail" (fun () -> ()))

let test_runner_unknown_event () =
  run_with_mock_mode "unknown_event" (fun () -> ())

let test_runner_no_results () = run_with_mock_mode "no_results" (fun () -> ())

let test_runner_failed () =
  expect_failure "runner failed test" (fun () ->
      run_with_mock_mode "failed" (fun () -> ()))

let test_runner_with_replay () =
  expect_failure "runner with replay" (fun () ->
      run_with_mock_mode "with_replay" (fun () -> ()))

let test_runner_no_event_key () =
  run_with_mock_mode "no_event_key" (fun () -> ())

let test_runner_non_int_interesting () =
  run_with_mock_mode "non_int_interesting" (fun () -> ())

let test_runner_early_close () = run_with_mock_mode "early_close" (fun () -> ())
let test_runner_slow_start () = run_with_mock_mode "slow_start" (fun () -> ())

let test_runner_interesting_pass () =
  expect_failure "runner interesting pass" (fun () ->
      run_with_mock_mode "interesting_pass" (fun () -> failwith "test error"))

(* ================================================================ *)
(* CBOR encoding: test large u64 values                             *)
(* ================================================================ *)

let test_cbor_large_values () =
  let v = Hegel.Cbor.Unsigned 100000 in
  assert (roundtrip v = v);
  let v = Hegel.Cbor.Unsigned 5000000000 in
  assert (roundtrip v = v);
  let v = Hegel.Cbor.Negative (-100000) in
  assert (roundtrip v = v)

let test_cbor_decode_head_u64 () =
  let v = Hegel.Cbor.Unsigned 5000000000 in
  let encoded = Hegel.Cbor.encode_to_string v in
  let decoded = Hegel.Cbor.decode_string encoded in
  assert (decoded = v)

(* ================================================================ *)
(* Main                                                             *)
(* ================================================================ *)

let () =
  Alcotest.run "Hegel Unit"
    [
      ( "CBOR encoding",
        [
          Alcotest.test_case "unsigned" `Quick test_cbor_unsigned;
          Alcotest.test_case "negative" `Quick test_cbor_negative;
          Alcotest.test_case "text" `Quick test_cbor_text;
          Alcotest.test_case "bytes" `Quick test_cbor_bytes;
          Alcotest.test_case "array" `Quick test_cbor_array;
          Alcotest.test_case "map" `Quick test_cbor_map;
          Alcotest.test_case "bool" `Quick test_cbor_bool;
          Alcotest.test_case "null" `Quick test_cbor_null;
          Alcotest.test_case "float" `Quick test_cbor_float;
          Alcotest.test_case "tag" `Quick test_cbor_tag;
          Alcotest.test_case "encode head sizes" `Quick
            test_cbor_encode_head_sizes;
          Alcotest.test_case "half float" `Quick test_cbor_half_float;
          Alcotest.test_case "single float" `Quick test_cbor_single_float;
          Alcotest.test_case "decode errors" `Quick test_cbor_decode_errors;
          Alcotest.test_case "large values" `Quick test_cbor_large_values;
          Alcotest.test_case "decode head u64" `Quick test_cbor_decode_head_u64;
        ] );
      ( "CBOR helpers",
        [
          Alcotest.test_case "map_get" `Quick test_cbor_map_get;
          Alcotest.test_case "map_get_exn" `Quick test_cbor_map_get_exn;
          Alcotest.test_case "as helpers" `Quick test_cbor_as_helpers;
          Alcotest.test_case "to_diagnostic" `Quick test_cbor_to_diagnostic;
        ] );
      ("CRC32", [ Alcotest.test_case "crc32" `Quick test_crc32 ]);
      ( "State",
        [
          Alcotest.test_case "last_run" `Quick test_state_last_run;
          Alcotest.test_case "test_aborted" `Quick test_state_test_aborted;
          Alcotest.test_case "generated_values" `Quick
            test_state_generated_values;
          Alcotest.test_case "connection" `Quick test_state_connection;
          Alcotest.test_case "connection lifecycle" `Quick
            test_state_connection_lifecycle;
        ] );
      ( "Gen helpers",
        [
          Alcotest.test_case "string_contains" `Quick test_gen_string_contains;
          Alcotest.test_case "base64_decode" `Quick test_gen_base64_decode;
          Alcotest.test_case "cbor helpers" `Quick test_gen_cbor_helpers;
          Alcotest.test_case "one_of empty" `Quick test_gen_one_of_empty;
          Alcotest.test_case "sampled_from empty" `Quick
            test_gen_sampled_from_empty;
          Alcotest.test_case "labels" `Quick test_labels;
        ] );
      ( "Protocol",
        [
          Alcotest.test_case "packet roundtrip" `Quick
            test_protocol_packet_roundtrip;
          Alcotest.test_case "packet reply" `Quick test_protocol_packet_reply;
          Alcotest.test_case "packet empty payload" `Quick
            test_protocol_packet_empty_payload;
          Alcotest.test_case "connection" `Quick test_protocol_connection;
          Alcotest.test_case "channel send/receive" `Quick
            test_protocol_channel_send_receive;
          Alcotest.test_case "connect channel" `Quick
            test_protocol_connect_channel;
          Alcotest.test_case "channel close" `Quick test_protocol_channel_close;
          Alcotest.test_case "connection close" `Quick
            test_protocol_connection_close;
          Alcotest.test_case "send response" `Quick test_protocol_send_response;
          Alcotest.test_case "receive response" `Quick
            test_protocol_receive_response;
          Alcotest.test_case "receive request" `Quick
            test_protocol_receive_request;
          Alcotest.test_case "channel routing" `Quick
            test_protocol_channel_routing;
          Alcotest.test_case "pending queue" `Quick test_protocol_pending_queue;
          Alcotest.test_case "receive response buffering" `Quick
            test_protocol_receive_response_with_buffering;
        ] );
      ( "Protocol errors",
        [
          Alcotest.test_case "invalid magic" `Quick test_protocol_invalid_magic;
          Alcotest.test_case "invalid terminator" `Quick
            test_protocol_invalid_terminator;
          Alcotest.test_case "checksum mismatch" `Quick
            test_protocol_checksum_mismatch;
          Alcotest.test_case "read closed fd" `Quick
            test_protocol_read_closed_fd;
          Alcotest.test_case "write closed fd" `Quick
            test_protocol_write_closed_fd;
          Alcotest.test_case "request_cbor error" `Quick
            test_protocol_request_cbor_error;
          Alcotest.test_case "request_cbor non-text error" `Quick
            test_protocol_request_cbor_non_text_error;
          Alcotest.test_case "request_cbor error no type" `Quick
            test_protocol_request_cbor_error_no_type;
          Alcotest.test_case "request_cbor no result" `Quick
            test_protocol_request_cbor_no_result;
          Alcotest.test_case "receive response wrong type" `Quick
            test_protocol_receive_response_wrong_type;
          Alcotest.test_case "receive request unexpected reply" `Quick
            test_protocol_receive_request_unexpected_reply;
        ] );
      ( "Runner edge cases",
        [
          Alcotest.test_case "extract_channel_id" `Quick test_extract_channel_id;
          Alcotest.test_case "find_hegel_path" `Quick test_find_hegel_path;
          Alcotest.test_case "find_hegel_path not found" `Quick
            test_find_hegel_path_not_found;
          Alcotest.test_case "run no hegel path" `Quick test_run_no_hegel_path;
        ] );
      ( "Gen error paths",
        [
          Alcotest.test_case "generate_raw aborted" `Quick
            test_generate_raw_aborted;
          Alcotest.test_case "start_span aborted" `Quick test_start_span_aborted;
          Alcotest.test_case "base64 invalid char" `Quick
            test_base64_invalid_char;
          Alcotest.test_case "generate_raw stop_test" `Quick
            test_generate_raw_stop_test;
          Alcotest.test_case "generate_raw overflow" `Quick
            test_generate_raw_overflow;
          Alcotest.test_case "generate_raw overrun" `Quick
            test_generate_raw_overrun;
          Alcotest.test_case "start_span stop_test" `Quick
            test_start_span_stop_test;
          Alcotest.test_case "stop_span error" `Quick test_stop_span_error;
          Alcotest.test_case "new_collection non-text" `Quick
            test_new_collection_non_text;
          Alcotest.test_case "collection_more non-bool" `Quick
            test_collection_more_non_bool;
          Alcotest.test_case "non-stop_test error raises" `Quick
            test_non_stop_test_error_raises;
        ] );
      ( "Mock runner",
        [
          Alcotest.test_case "normal" `Quick test_runner_normal;
          Alcotest.test_case "version fail" `Quick test_runner_version_fail;
          Alcotest.test_case "unknown event" `Quick test_runner_unknown_event;
          Alcotest.test_case "no results" `Quick test_runner_no_results;
          Alcotest.test_case "failed" `Quick test_runner_failed;
          Alcotest.test_case "with replay" `Quick test_runner_with_replay;
          Alcotest.test_case "no event key" `Quick test_runner_no_event_key;
          Alcotest.test_case "non-int interesting" `Quick
            test_runner_non_int_interesting;
          Alcotest.test_case "early close" `Quick test_runner_early_close;
          Alcotest.test_case "slow start" `Quick test_runner_slow_start;
          Alcotest.test_case "interesting pass" `Quick
            test_runner_interesting_pass;
        ] );
    ]
