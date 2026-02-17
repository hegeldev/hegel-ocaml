let failures = ref 0

let check name cond =
  if not cond then (
    Printf.eprintf "FAIL: %s\n%!" name;
    incr failures)

let check_raises_failure name f =
  match f () with
  | _ ->
      Printf.eprintf "FAIL: %s (no exception)\n%!" name;
      incr failures
  | exception Failure _ -> ()
  | exception exn ->
      Printf.eprintf "FAIL: %s (wrong exception: %s)\n%!" name
        (Printexc.to_string exn);
      incr failures

let check_raises name f =
  match f () with
  | _ ->
      Printf.eprintf "FAIL: %s (no exception)\n%!" name;
      incr failures
  | exception _ -> ()

(* ================================================================ *)
(* CBOR encoding/decoding tests                                     *)
(* ================================================================ *)

let roundtrip v =
  let encoded = Hegel.Cbor.encode_to_string v in
  let decoded = Hegel.Cbor.decode_string encoded in
  decoded

let test_cbor_unsigned () =
  check "unsigned 0" (roundtrip (Hegel.Cbor.Unsigned 0) = Hegel.Cbor.Unsigned 0);
  check "unsigned 1" (roundtrip (Hegel.Cbor.Unsigned 1) = Hegel.Cbor.Unsigned 1);
  check "unsigned 23"
    (roundtrip (Hegel.Cbor.Unsigned 23) = Hegel.Cbor.Unsigned 23);
  check "unsigned 24"
    (roundtrip (Hegel.Cbor.Unsigned 24) = Hegel.Cbor.Unsigned 24);
  check "unsigned 255"
    (roundtrip (Hegel.Cbor.Unsigned 255) = Hegel.Cbor.Unsigned 255);
  check "unsigned 256"
    (roundtrip (Hegel.Cbor.Unsigned 256) = Hegel.Cbor.Unsigned 256);
  check "unsigned 65535"
    (roundtrip (Hegel.Cbor.Unsigned 65535) = Hegel.Cbor.Unsigned 65535);
  check "unsigned 65536"
    (roundtrip (Hegel.Cbor.Unsigned 65536) = Hegel.Cbor.Unsigned 65536);
  check "unsigned large"
    (roundtrip (Hegel.Cbor.Unsigned 1000000) = Hegel.Cbor.Unsigned 1000000)

let test_cbor_negative () =
  check "negative -1"
    (roundtrip (Hegel.Cbor.Negative (-1)) = Hegel.Cbor.Negative (-1));
  check "negative -10"
    (roundtrip (Hegel.Cbor.Negative (-10)) = Hegel.Cbor.Negative (-10));
  check "negative -100"
    (roundtrip (Hegel.Cbor.Negative (-100)) = Hegel.Cbor.Negative (-100));
  check "negative -1000"
    (roundtrip (Hegel.Cbor.Negative (-1000)) = Hegel.Cbor.Negative (-1000))

let test_cbor_text () =
  check "text empty" (roundtrip (Hegel.Cbor.Text "") = Hegel.Cbor.Text "");
  check "text hello"
    (roundtrip (Hegel.Cbor.Text "hello") = Hegel.Cbor.Text "hello");
  let long_str = String.make 300 'x' in
  check "text long"
    (roundtrip (Hegel.Cbor.Text long_str) = Hegel.Cbor.Text long_str)

let test_cbor_bytes () =
  check "bytes empty" (roundtrip (Hegel.Cbor.Bytes "") = Hegel.Cbor.Bytes "");
  check "bytes data"
    (roundtrip (Hegel.Cbor.Bytes "\x00\x01\x02")
    = Hegel.Cbor.Bytes "\x00\x01\x02")

let test_cbor_array () =
  check "array empty" (roundtrip (Hegel.Cbor.Array []) = Hegel.Cbor.Array []);
  check "array ints"
    (roundtrip
       (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ])
    = Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ]);
  check "array nested"
    (roundtrip
       (Hegel.Cbor.Array [ Hegel.Cbor.Array [ Hegel.Cbor.Text "nested" ] ])
    = Hegel.Cbor.Array [ Hegel.Cbor.Array [ Hegel.Cbor.Text "nested" ] ])

let test_cbor_map () =
  check "map empty" (roundtrip (Hegel.Cbor.Map []) = Hegel.Cbor.Map []);
  check "map one pair"
    (roundtrip
       (Hegel.Cbor.Map [ (Hegel.Cbor.Text "key", Hegel.Cbor.Unsigned 42) ])
    = Hegel.Cbor.Map [ (Hegel.Cbor.Text "key", Hegel.Cbor.Unsigned 42) ])

let test_cbor_bool () =
  check "bool true" (roundtrip (Hegel.Cbor.Bool true) = Hegel.Cbor.Bool true);
  check "bool false" (roundtrip (Hegel.Cbor.Bool false) = Hegel.Cbor.Bool false)

let test_cbor_null () =
  check "null" (roundtrip Hegel.Cbor.Null = Hegel.Cbor.Null)

let test_cbor_float () =
  check "float 0.0" (roundtrip (Hegel.Cbor.Float 0.0) = Hegel.Cbor.Float 0.0);
  check "float 1.5" (roundtrip (Hegel.Cbor.Float 1.5) = Hegel.Cbor.Float 1.5);
  check "float -1.5"
    (roundtrip (Hegel.Cbor.Float (-1.5)) = Hegel.Cbor.Float (-1.5));
  check "float infinity"
    (roundtrip (Hegel.Cbor.Float infinity) = Hegel.Cbor.Float infinity);
  check "float neg_infinity"
    (roundtrip (Hegel.Cbor.Float neg_infinity) = Hegel.Cbor.Float neg_infinity);
  let nan_rt = roundtrip (Hegel.Cbor.Float nan) in
  check "float nan"
    (match nan_rt with Hegel.Cbor.Float f -> Float.is_nan f | _ -> false)

let test_cbor_tag () =
  check "tag 258"
    (roundtrip
       (Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))
    = Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))

let test_cbor_encode_head_sizes () =
  (* Test all size categories of encode_head by encoding text of various lengths *)
  let rt_text n =
    let s = String.make n 'a' in
    roundtrip (Hegel.Cbor.Text s) = Hegel.Cbor.Text s
  in
  check "encode_head inline (len<24)" (rt_text 5);
  check "encode_head 1-byte (24<=len<256)" (rt_text 100);
  check "encode_head 2-byte (256<=len<65536)" (rt_text 300);
  check "encode_head 4-byte (65536<=len)" (rt_text 70000)

(* Test decoding half-precision floats *)
let test_cbor_half_float () =
  (* Encode a half-precision float manually: major 7, additional 25, 2 bytes *)
  (* Half-precision 1.0 = 0x3C00: sign=0, exp=15, mant=0 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x3C);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float 1.0" (v = Hegel.Cbor.Float 1.0);
  (* Half-precision -2.0 = 0xC000: sign=1, exp=16, mant=0 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0xC0);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float -2.0" (v = Hegel.Cbor.Float (-2.0));
  (* Half-precision +infinity = 0x7C00: sign=0, exp=31, mant=0 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x7C);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float +inf" (v = Hegel.Cbor.Float infinity);
  (* Half-precision -infinity = 0xFC00: sign=1, exp=31, mant=0 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0xFC);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float -inf" (v = Hegel.Cbor.Float neg_infinity);
  (* Half-precision NaN = 0x7E00: sign=0, exp=31, mant!=0 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x7E);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float nan"
    (match v with Hegel.Cbor.Float f -> Float.is_nan f | _ -> false);
  (* Half-precision subnormal: sign=0, exp=0, mant=1 = 0x0001 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x00);
  Buffer.add_char buf (Char.chr 0x01);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float subnormal positive"
    (match v with Hegel.Cbor.Float f -> f > 0.0 && f < 0.001 | _ -> false);
  (* Half-precision negative subnormal: sign=1, exp=0, mant=1 = 0x8001 *)
  let buf = Buffer.create 3 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 25));
  Buffer.add_char buf (Char.chr 0x80);
  Buffer.add_char buf (Char.chr 0x01);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "half float subnormal negative"
    (match v with Hegel.Cbor.Float f -> f < 0.0 && f > -0.001 | _ -> false)

(* Test single-precision float decoding *)
let test_cbor_single_float () =
  (* Single-precision 1.0 = 3F800000 *)
  let buf = Buffer.create 5 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 26));
  Buffer.add_char buf (Char.chr 0x3F);
  Buffer.add_char buf (Char.chr 0x80);
  Buffer.add_char buf (Char.chr 0x00);
  Buffer.add_char buf (Char.chr 0x00);
  let s = Buffer.contents buf in
  let v, _ = Hegel.Cbor.decode s 0 in
  check "single float 1.0" (v = Hegel.Cbor.Float 1.0)

(* Test CBOR decode error cases *)
let test_cbor_decode_errors () =
  (* Unsupported additional info (28-30 are reserved) *)
  let buf = Buffer.create 1 in
  Buffer.add_char buf (Char.chr ((0 lsl 5) lor 28));
  check_raises_failure "unsupported additional info" (fun () ->
      Hegel.Cbor.decode_string (Buffer.contents buf));
  (* Unsupported simple value *)
  let buf = Buffer.create 1 in
  Buffer.add_char buf (Char.chr ((7 lsl 5) lor 24));
  Buffer.add_char buf (Char.chr 0);
  check_raises_failure "unsupported simple value" (fun () ->
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
  check "map_get existing"
    (Hegel.Cbor.map_get m "a" = Some (Hegel.Cbor.Unsigned 1));
  check "map_get second"
    (Hegel.Cbor.map_get m "b" = Some (Hegel.Cbor.Unsigned 2));
  check "map_get missing" (Hegel.Cbor.map_get m "c" = None);
  check "map_get not a map"
    (Hegel.Cbor.map_get (Hegel.Cbor.Unsigned 5) "a" = None);
  (* map_get should skip non-Text keys *)
  let m2 =
    Hegel.Cbor.Map
      [
        (Hegel.Cbor.Unsigned 1, Hegel.Cbor.Text "num");
        (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1);
      ]
  in
  check "map_get skips non-text keys"
    (Hegel.Cbor.map_get m2 "a" = Some (Hegel.Cbor.Unsigned 1))

let test_cbor_map_get_exn () =
  let m = Hegel.Cbor.Map [ (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1) ] in
  check "map_get_exn existing"
    (Hegel.Cbor.map_get_exn m "a" = Hegel.Cbor.Unsigned 1);
  check_raises_failure "map_get_exn missing" (fun () ->
      ignore (Hegel.Cbor.map_get_exn m "b"))

let test_cbor_as_helpers () =
  check "as_text some" (Hegel.Cbor.as_text (Hegel.Cbor.Text "hi") = Some "hi");
  check "as_text none" (Hegel.Cbor.as_text (Hegel.Cbor.Unsigned 1) = None);
  check "as_bool some true"
    (Hegel.Cbor.as_bool (Hegel.Cbor.Bool true) = Some true);
  check "as_bool some false"
    (Hegel.Cbor.as_bool (Hegel.Cbor.Bool false) = Some false);
  check "as_bool none" (Hegel.Cbor.as_bool (Hegel.Cbor.Unsigned 1) = None);
  check "as_float from float"
    (Hegel.Cbor.as_float (Hegel.Cbor.Float 1.5) = Some 1.5);
  check "as_float from unsigned"
    (Hegel.Cbor.as_float (Hegel.Cbor.Unsigned 5) = Some 5.0);
  check "as_float from negative"
    (Hegel.Cbor.as_float (Hegel.Cbor.Negative (-3)) = Some (-3.0));
  check "as_float none" (Hegel.Cbor.as_float (Hegel.Cbor.Text "x") = None);
  check "as_array some" (Hegel.Cbor.as_array (Hegel.Cbor.Array []) = Some []);
  check "as_array none" (Hegel.Cbor.as_array (Hegel.Cbor.Unsigned 1) = None);
  check "as_map some" (Hegel.Cbor.as_map (Hegel.Cbor.Map []) = Some []);
  check "as_map none" (Hegel.Cbor.as_map (Hegel.Cbor.Unsigned 1) = None);
  check "as_int unsigned" (Hegel.Cbor.as_int (Hegel.Cbor.Unsigned 42) = Some 42);
  check "as_int negative"
    (Hegel.Cbor.as_int (Hegel.Cbor.Negative (-5)) = Some (-5));
  check "as_int float integer"
    (Hegel.Cbor.as_int (Hegel.Cbor.Float 3.0) = Some 3);
  check "as_int float non-integer"
    (Hegel.Cbor.as_int (Hegel.Cbor.Float 3.5) = None);
  check "as_int text" (Hegel.Cbor.as_int (Hegel.Cbor.Text "x") = None)

let test_cbor_to_diagnostic () =
  check "diag unsigned"
    (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Unsigned 42) = "42");
  check "diag negative"
    (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Negative (-5)) = "-5");
  check "diag text" (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Text "hi") = "\"hi\"");
  check "diag bytes"
    (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bytes "\x00\xff") = "h'00ff'");
  check "diag array"
    (Hegel.Cbor.to_diagnostic
       (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1; Hegel.Cbor.Unsigned 2 ])
    = "[1, 2]");
  check "diag map"
    (Hegel.Cbor.to_diagnostic
       (Hegel.Cbor.Map [ (Hegel.Cbor.Text "a", Hegel.Cbor.Unsigned 1) ])
    = "{\"a\": 1}");
  check "diag true" (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bool true) = "true");
  check "diag false" (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Bool false) = "false");
  check "diag null" (Hegel.Cbor.to_diagnostic Hegel.Cbor.Null = "null");
  check "diag tag"
    (Hegel.Cbor.to_diagnostic (Hegel.Cbor.Tag (258, Hegel.Cbor.Array []))
    = "258([])");
  (* Float diagnostic just needs to not crash *)
  let _ = Hegel.Cbor.to_diagnostic (Hegel.Cbor.Float 1.5) in
  ()

(* ================================================================ *)
(* CRC32 tests                                                      *)
(* ================================================================ *)

let test_crc32 () =
  (* CRC32 of empty string is 0x00000000 *)
  check "crc32 empty" (Hegel.Crc32.compute "" = 0x00000000l);
  (* CRC32 of "123456789" is 0xCBF43926 (standard test vector) *)
  check "crc32 123456789" (Hegel.Crc32.compute "123456789" = 0xCBF43926l);
  (* CRC32 of single byte *)
  let _ = Hegel.Crc32.compute "\x00" in
  let _ = Hegel.Crc32.compute "a" in
  ()

(* ================================================================ *)
(* State tests                                                      *)
(* ================================================================ *)

let test_state_last_run () =
  Hegel.State.set_last_run false;
  check "last_run initially false" (not (Hegel.State.get_last_run ()));
  Hegel.State.set_last_run true;
  check "last_run set true" (Hegel.State.get_last_run ());
  Hegel.State.set_last_run false;
  check "last_run set false" (not (Hegel.State.get_last_run ()))

let test_state_test_aborted () =
  Hegel.State.set_test_aborted false;
  check "test_aborted initially false" (not (Hegel.State.get_test_aborted ()));
  Hegel.State.set_test_aborted true;
  check "test_aborted set true" (Hegel.State.get_test_aborted ());
  Hegel.State.set_test_aborted false;
  check "test_aborted set false" (not (Hegel.State.get_test_aborted ()))

let test_state_generated_values () =
  let vs = Hegel.State.take_generated_values () in
  check "take_generated_values initially empty" (vs = []);
  Hegel.State.buffer_generated_value "a";
  Hegel.State.buffer_generated_value "b";
  let vs = Hegel.State.take_generated_values () in
  check "take_generated_values returns in order" (vs = [ "a"; "b" ]);
  let vs2 = Hegel.State.take_generated_values () in
  check "take_generated_values clears" (vs2 = [])

let test_state_connection () =
  check "get_connection initially none" (Hegel.State.get_connection () = None);
  check_raises_failure "get_channel with no connection" (fun () ->
      ignore (Hegel.State.get_channel ()));
  check_raises_failure "increment_span_depth no connection" (fun () ->
      Hegel.State.increment_span_depth ());
  check_raises_failure "decrement_span_depth no connection" (fun () ->
      Hegel.State.decrement_span_depth ())

(* ================================================================ *)
(* Gen helper tests (things that don't need the server)             *)
(* ================================================================ *)

let test_gen_string_contains () =
  check "string_contains found"
    (Hegel.Gen.string_contains "hello world" "world");
  check "string_contains not found"
    (not (Hegel.Gen.string_contains "hello world" "xyz"));
  check "string_contains needle longer"
    (not (Hegel.Gen.string_contains "hi" "hello"));
  check "string_contains empty needle" (Hegel.Gen.string_contains "hello" "");
  check "string_contains at start" (Hegel.Gen.string_contains "hello" "hel");
  check "string_contains at end" (Hegel.Gen.string_contains "hello" "llo");
  check "string_contains exact match"
    (Hegel.Gen.string_contains "hello" "hello");
  check "string_contains both empty" (Hegel.Gen.string_contains "" "")

let test_gen_base64_decode () =
  check "base64 Hello" (Hegel.Gen.base64_decode "SGVsbG8=" = "Hello");
  check "base64 empty" (Hegel.Gen.base64_decode "" = "");
  check "base64 no padding" (Hegel.Gen.base64_decode "YWJj" = "abc");
  check "base64 one padding" (Hegel.Gen.base64_decode "YWI=" = "ab");
  check "base64 two padding" (Hegel.Gen.base64_decode "YQ==" = "a");
  check "base64 Man" (Hegel.Gen.base64_decode "TWFu" = "Man")

let test_gen_cbor_helpers () =
  (* cbor_int_value *)
  check "cbor_int_value unsigned"
    (Hegel.Gen.cbor_int_value (Hegel.Cbor.Unsigned 42) = 42);
  check "cbor_int_value negative"
    (Hegel.Gen.cbor_int_value (Hegel.Cbor.Negative (-5)) = -5);
  check "cbor_int_value float-as-int"
    (Hegel.Gen.cbor_int_value (Hegel.Cbor.Float 3.0) = 3);
  check_raises_failure "cbor_int_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_int_value (Hegel.Cbor.Text "x")));
  (* cbor_float_value *)
  check "cbor_float_value float"
    (Hegel.Gen.cbor_float_value (Hegel.Cbor.Float 1.5) = 1.5);
  check "cbor_float_value unsigned"
    (Hegel.Gen.cbor_float_value (Hegel.Cbor.Unsigned 5) = 5.0);
  check "cbor_float_value negative"
    (Hegel.Gen.cbor_float_value (Hegel.Cbor.Negative (-3)) = -3.0);
  check_raises_failure "cbor_float_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_float_value (Hegel.Cbor.Text "x")));
  (* cbor_text_value *)
  check "cbor_text_value text"
    (Hegel.Gen.cbor_text_value (Hegel.Cbor.Text "hi") = "hi");
  check_raises_failure "cbor_text_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_text_value (Hegel.Cbor.Unsigned 1)));
  (* cbor_bool_value *)
  check "cbor_bool_value true"
    (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Bool true) = true);
  check "cbor_bool_value false"
    (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Bool false) = false);
  check_raises_failure "cbor_bool_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_bool_value (Hegel.Cbor.Unsigned 1)));
  (* cbor_array_value *)
  check "cbor_array_value empty"
    (Hegel.Gen.cbor_array_value (Hegel.Cbor.Array []) = []);
  check "cbor_array_value list"
    (Hegel.Gen.cbor_array_value (Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ])
    = [ Hegel.Cbor.Unsigned 1 ]);
  check "cbor_array_value set tag"
    (Hegel.Gen.cbor_array_value
       (Hegel.Cbor.Tag (258, Hegel.Cbor.Array [ Hegel.Cbor.Unsigned 1 ]))
    = [ Hegel.Cbor.Unsigned 1 ]);
  check_raises_failure "cbor_array_value invalid" (fun () ->
      ignore (Hegel.Gen.cbor_array_value (Hegel.Cbor.Text "x")))

let test_gen_one_of_empty () =
  check_raises_failure "one_of empty list" (fun () ->
      ignore (Hegel.Gen.one_of []))

let test_gen_sampled_from_empty () =
  check_raises_failure "sampled_from empty list" (fun () ->
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
      check "packet channel" (got.channel = 42);
      check "packet message_id" (got.message_id = 7);
      check "packet is_reply" (not got.is_reply);
      check "packet payload" (got.payload = "hello"))

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
      check "reply packet channel" (got.channel = 1);
      check "reply packet message_id" (got.message_id = 3);
      check "reply packet is_reply" got.is_reply;
      check "reply packet payload" (got.payload = "world"))

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
      check "empty payload" (got.payload = ""))

let test_protocol_connection () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let control = Hegel.Protocol.Connection.control_channel conn in
      check "control channel id" (Hegel.Protocol.Channel.channel_id control = 0);
      let ch1 = Hegel.Protocol.Connection.new_channel conn in
      let ch1_id = Hegel.Protocol.Channel.channel_id ch1 in
      check "first new channel odd" (ch1_id land 1 = 1);
      let ch2 = Hegel.Protocol.Connection.new_channel conn in
      let ch2_id = Hegel.Protocol.Channel.channel_id ch2 in
      check "second channel different" (ch2_id <> ch1_id);
      check "second channel odd" (ch2_id land 1 = 1))

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
      check "msg_id positive" (msg_id > 0);
      (* Read the raw packet on the other side *)
      let got = Hegel.Protocol.read_packet rd in
      check "sent request not reply" (not got.is_reply);
      check "sent request payload" (got.payload = "test payload");
      check "sent request message_id" (got.message_id = msg_id))

let test_protocol_connect_channel () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.connect_channel conn 99 in
      check "connect_channel id" (Hegel.Protocol.Channel.channel_id ch = 99))

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
      check "close packet payload" (got.payload = "\xFE"))

let test_protocol_connection_close () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (* Don't use Fun.protect since close will close the fds *)
  let conn = Hegel.Protocol.Connection.create wr in
  Hegel.Protocol.Connection.close conn;
  (* rd should now be closed or at least wr is shutdown *)
  let buf = Bytes.create 1 in
  let got =
    try
      let n = Unix.read rd buf 0 1 in
      n = 0
    with Unix.Unix_error _ -> true
  in
  check "connection close shuts down" got;
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
      check "send_response is_reply" got.is_reply;
      check "send_response message_id" (got.message_id = 42);
      check "send_response payload" (got.payload = "response data"))

(* Test receive_response: direct reply *)
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
      check "receive_response direct" (got = "the reply"))

(* Test receive_request: direct request *)
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
      check "receive_request payload" (payload = "the request");
      check "receive_request msg_id" (msg_id = 20))

(* Test receive_packet_for_channel with packets for different channels *)
let test_protocol_channel_routing () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd1 in
      (* Write packets for channel 5, then channel 0 *)
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
      (* Reading from channel 0 should skip the ch5 packet and buffer it *)
      let msg_id, payload = Hegel.Protocol.Channel.receive_request ch0 in
      check "channel routing payload" (payload = "for ch0");
      check "channel routing msg_id" (msg_id = 2);
      (* Now reading from channel 5 should get the buffered packet *)
      let ch5 = Hegel.Protocol.Connection.connect_channel conn 5 in
      let msg_id5, payload5 = Hegel.Protocol.Channel.receive_request ch5 in
      check "buffered channel routing payload" (payload5 = "for ch5");
      check "buffered channel routing msg_id" (msg_id5 = 1))

(* Test reading from pending queue *)
let test_protocol_pending_queue () =
  let rd1, wr1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd1;
      Unix.close wr1)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd1 in
      (* Write two packets for channel 3, then one for channel 0 *)
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
      (* Read from channel 0 - should buffer both ch3 packets *)
      let ch0 = Hegel.Protocol.Connection.control_channel conn in
      let _, p = Hegel.Protocol.Channel.receive_request ch0 in
      check "pending queue ch0" (p = "for ch0");
      (* Now read from channel 3 - should get both from pending queue *)
      let ch3 = Hegel.Protocol.Connection.connect_channel conn 3 in
      let _, p1 = Hegel.Protocol.Channel.receive_request ch3 in
      check "pending queue ch3 first" (p1 = "first for ch3");
      let _, p2 = Hegel.Protocol.Channel.receive_request ch3 in
      check "pending queue ch3 second" (p2 = "second for ch3"))

(* ================================================================ *)
(* Gen.Labels tests                                                 *)
(* ================================================================ *)

let test_labels () =
  check "label list" (Hegel.Gen.Labels.list = 1);
  check "label list_element" (Hegel.Gen.Labels.list_element = 2);
  check "label set" (Hegel.Gen.Labels.set = 3);
  check "label set_element" (Hegel.Gen.Labels.set_element = 4);
  check "label map" (Hegel.Gen.Labels.map = 5);
  check "label map_entry" (Hegel.Gen.Labels.map_entry = 6);
  check "label tuple" (Hegel.Gen.Labels.tuple = 7);
  check "label one_of" (Hegel.Gen.Labels.one_of = 8);
  check "label optional" (Hegel.Gen.Labels.optional = 9);
  check "label fixed_dict" (Hegel.Gen.Labels.fixed_dict = 10);
  check "label flat_map" (Hegel.Gen.Labels.flat_map = 11);
  check "label filter" (Hegel.Gen.Labels.filter = 12);
  check "label mapped" (Hegel.Gen.Labels.mapped = 13);
  check "label sampled_from" (Hegel.Gen.Labels.sampled_from = 14)

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
      (* Write 20 bytes of header with wrong magic, then 1 byte terminator *)
      let header = Bytes.create 21 in
      Bytes.fill header 0 21 '\x00';
      (* Set wrong magic: 0xDEADBEEF *)
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
      check_raises_failure "read_packet invalid magic" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_invalid_terminator () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      (* Write a valid packet but with wrong terminator *)
      (* First, build a proper header *)
      let payload = "" in
      let header = Bytes.create 20 in
      (* magic = 0x4845474C *)
      Bytes.set header 0 (Char.chr 0x48);
      Bytes.set header 1 (Char.chr 0x45);
      Bytes.set header 2 (Char.chr 0x47);
      Bytes.set header 3 (Char.chr 0x4C);
      (* checksum placeholder *)
      Bytes.fill header 4 4 '\x00';
      (* channel = 0 *)
      Bytes.fill header 8 4 '\x00';
      (* message_id = 0 *)
      Bytes.fill header 12 4 '\x00';
      (* length = 0 *)
      Bytes.fill header 16 4 '\x00';
      (* Compute checksum *)
      let header_str = Bytes.to_string header in
      let checksum = Hegel.Crc32.compute (header_str ^ payload) in
      let cksum_i = Int32.to_int checksum in
      Bytes.set header 4 (Char.chr ((cksum_i lsr 24) land 0xff));
      Bytes.set header 5 (Char.chr ((cksum_i lsr 16) land 0xff));
      Bytes.set header 6 (Char.chr ((cksum_i lsr 8) land 0xff));
      Bytes.set header 7 (Char.chr (cksum_i land 0xff));
      let full = Bytes.to_string header ^ "\xFF" (* wrong terminator *) in
      let len = String.length full in
      let written = ref 0 in
      while !written < len do
        let n = Unix.write_substring wr full !written (len - !written) in
        written := !written + n
      done;
      check_raises_failure "read_packet invalid terminator" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_checksum_mismatch () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      (* Write a packet with wrong checksum *)
      let header = Bytes.create 20 in
      (* magic = 0x4845474C *)
      Bytes.set header 0 (Char.chr 0x48);
      Bytes.set header 1 (Char.chr 0x45);
      Bytes.set header 2 (Char.chr 0x47);
      Bytes.set header 3 (Char.chr 0x4C);
      (* Deliberately wrong checksum *)
      Bytes.set header 4 (Char.chr 0xFF);
      Bytes.set header 5 (Char.chr 0xFF);
      Bytes.set header 6 (Char.chr 0xFF);
      Bytes.set header 7 (Char.chr 0xFF);
      (* channel = 0, message_id = 0, length = 0 *)
      Bytes.fill header 8 12 '\x00';
      let full = Bytes.to_string header ^ "\x0A" (* correct terminator *) in
      let len = String.length full in
      let written = ref 0 in
      while !written < len do
        let n = Unix.write_substring wr full !written (len - !written) in
        written := !written + n
      done;
      check_raises_failure "read_packet checksum mismatch" (fun () ->
          ignore (Hegel.Protocol.read_packet rd)))

let test_protocol_read_closed_fd () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.close wr;
  check_raises_failure "read_packet on closed connection" (fun () ->
      ignore (Hegel.Protocol.read_packet rd));
  Unix.close rd

let test_protocol_write_closed_fd () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.close rd;
  Unix.close wr;
  check_raises "write_packet on closed fd" (fun () ->
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
          (* Child: read request, send error response *)
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
          check_raises_failure "request_cbor error response" (fun () ->
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
          check_raises_failure "request_cbor non-text error" (fun () ->
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
          (* Child: read request, send error response with no type field *)
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
          check_raises_failure "request_cbor error no type" (fun () ->
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
          (* Child: read request, send response with no result or error field *)
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
          (* Should return the whole response since there's no "result" field *)
          check "request_cbor no result returns response"
            (Hegel.Cbor.map_get result "data" = Some (Hegel.Cbor.Unsigned 99));
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
      (* Set connection *)
      Hegel.State.set_connection ch;
      check "connection is set" (Hegel.State.get_connection () <> None);
      (* get_channel works *)
      let _ = Hegel.State.get_channel () in
      (* span depth operations *)
      Hegel.State.increment_span_depth ();
      Hegel.State.decrement_span_depth ();
      (* clear connection *)
      Hegel.State.clear_connection ();
      check "connection is cleared" (Hegel.State.get_connection () = None))

(* ================================================================ *)
(* Runner edge case tests                                           *)
(* ================================================================ *)

let test_extract_channel_id () =
  let valid =
    Hegel.Cbor.Map [ (Hegel.Cbor.Text "channel", Hegel.Cbor.Unsigned 42) ]
  in
  check "extract_channel_id valid" (Hegel.extract_channel_id valid = 42);
  let missing = Hegel.Cbor.Map [] in
  check_raises_failure "extract_channel_id missing" (fun () ->
      ignore (Hegel.extract_channel_id missing));
  let wrong_type =
    Hegel.Cbor.Map [ (Hegel.Cbor.Text "channel", Hegel.Cbor.Text "not an int") ]
  in
  check_raises_failure "extract_channel_id wrong type" (fun () ->
      ignore (Hegel.extract_channel_id wrong_type))

let test_find_hegel_path () =
  (* Just exercise the function - it searches PATH *)
  let _ = Hegel.find_hegel_path () in
  ()

let test_find_hegel_path_not_found () =
  let old_path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  Unix.putenv "PATH" "/nonexistent";
  check "hegel not on path" (Hegel.find_hegel_path () = None);
  Unix.putenv "PATH" old_path

let test_run_no_hegel_path () =
  let old_path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  Unix.putenv "PATH" "/nonexistent";
  check_raises_failure "run without hegel path" (fun () ->
      Hegel.run (fun () -> ()));
  Unix.putenv "PATH" old_path

(* ================================================================ *)
(* Gen error path tests                                              *)
(* ================================================================ *)

let test_generate_raw_aborted () =
  Hegel.State.set_test_aborted true;
  check_raises "generate_raw when aborted" (fun () ->
      ignore (Hegel.Gen.generate_raw (Hegel.Cbor.Map [])));
  Hegel.State.set_test_aborted false

let test_start_span_aborted () =
  Hegel.State.set_test_aborted true;
  check_raises "start_span when aborted" (fun () -> Hegel.Gen.start_span 1);
  Hegel.State.set_test_aborted false

let test_base64_invalid_char () =
  check_raises_failure "base64 invalid char" (fun () ->
      ignore (Hegel.Gen.base64_decode "!!!!"))

(* Ignore SIGPIPE so broken mock connections raise EPIPE instead of killing us *)
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Helper: set up a mock connection where the child process acts as server *)
let with_mock_server test_fn server_fn =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Hegel.Protocol.Connection.create wr in
  let ch = Hegel.Protocol.Connection.control_channel conn in
  Hegel.State.set_connection ch;
  Hegel.State.set_test_aborted false;
  match Unix.fork () with
  | 0 ->
      (* Child: close unused end so parent's close causes EOF on our reads *)
      Unix.close wr;
      (* Handle specific server protocol *)
      (try server_fn rd with _ -> ());
      (* Drain remaining requests (e.g., stop_span cleanup) until parent closes *)
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
      (* Parent: close unused end so child's reads get EOF when we close wr *)
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
      check_raises "generate_raw StopTest" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      check "test_aborted set after StopTest" (Hegel.State.get_test_aborted ()))
    respond_with_stop_test

let test_start_span_stop_test () =
  with_mock_server
    (fun () ->
      Hegel.State.increment_span_depth ();
      (* start_span increments, then catches Assume_rejected and decrements *)
      check_raises "start_span StopTest" (fun () -> Hegel.Gen.start_span 1);
      check "test_aborted set after start_span StopTest"
        (Hegel.State.get_test_aborted ()))
    respond_with_stop_test

let test_stop_span_error () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Hegel.Protocol.Connection.create wr in
  let ch = Hegel.Protocol.Connection.control_channel conn in
  Hegel.State.set_connection ch;
  Hegel.State.set_test_aborted false;
  Hegel.State.increment_span_depth ();
  (* Close both ends to break the connection *)
  Unix.close rd;
  Unix.close wr;
  (* stop_span should catch the write error silently (with _ -> Cbor.Null) *)
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
      check_raises "generate_raw overflow" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      check "test_aborted after overflow" (Hegel.State.get_test_aborted ()))
    (fun rd -> respond_with_error rd "overflow" "buffer overflow")

let test_generate_raw_overrun () =
  with_mock_server
    (fun () ->
      check_raises "generate_raw overrun" (fun () ->
          ignore
            (Hegel.Gen.generate_raw
               (Hegel.Cbor.Map
                  [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ])));
      check "test_aborted after overrun" (Hegel.State.get_test_aborted ()))
    (fun rd -> respond_with_error rd "Overrun" "data Overrun")

(* Helper: respond to N requests with ok, then respond with a custom result *)
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

(* new_collection returns non-text: exercises failwith "Expected text response" *)
let test_new_collection_non_text () =
  with_mock_server
    (fun () ->
      check_raises_failure "new_collection non-text" (fun () ->
          let gen =
            Hegel.Gen.list ~min_size:1 ~max_size:3
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))
          in
          ignore (gen.generate ())))
    (fun rd ->
      (* 1st request: start_span (from group Labels.list) → ok *)
      (* 2nd request: new_collection → respond with non-text *)
      respond_ok_then_custom rd 1 (Hegel.Cbor.Unsigned 42))

(* collection_more returns non-bool: exercises failwith "Expected bool" *)
let test_collection_more_non_bool () =
  with_mock_server
    (fun () ->
      check_raises_failure "collection_more non-bool" (fun () ->
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
      (* 1st: start_span → ok *)
      respond (Hegel.Protocol.read_packet rd) Hegel.Cbor.Null;
      (* 2nd: new_collection → text name *)
      respond (Hegel.Protocol.read_packet rd) (Hegel.Cbor.Text "test_coll");
      (* 3rd: collection_more → non-bool *)
      respond (Hegel.Protocol.read_packet rd) (Hegel.Cbor.Unsigned 42))

let test_non_stop_test_error_exits_134 () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  match Unix.fork () with
  | 0 ->
      (* Child: set up connection and call generate_raw *)
      Unix.close rd;
      let conn = Hegel.Protocol.Connection.create wr in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      Hegel.State.clear_connection ();
      Hegel.State.set_connection ch;
      Hegel.State.set_test_aborted false;
      (* generate_raw → send_request_no_fail → gets non-StopTest error → exit 134 *)
      ignore
        (Hegel.Gen.generate_raw
           (Hegel.Cbor.Map
              [ (Hegel.Cbor.Text "type", Hegel.Cbor.Text "integer") ]));
      (* Should not reach here *)
      exit 1
  | pid -> (
      (* Parent: mock server, respond with non-StopTest error *)
      Unix.close wr;
      let pkt = Hegel.Protocol.read_packet rd in
      let error_response =
        Hegel.Cbor.encode_to_string
          (Hegel.Cbor.Map
             [
               (Hegel.Cbor.Text "error", Hegel.Cbor.Text "something unexpected");
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
      let _, status = Unix.waitpid [] pid in
      Unix.close rd;
      match status with
      | Unix.WEXITED 134 -> ()
      | Unix.WEXITED n ->
          Printf.eprintf
            "FAIL: non-StopTest error expected exit 134, got %d\n%!" n;
          incr failures
      | _ ->
          Printf.eprintf "FAIL: non-StopTest error got signal\n%!";
          incr failures)

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
      (* Write a packet for a different channel first (will be buffered) *)
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 5;
          message_id = 50;
          is_reply = true;
          payload = "for other channel";
        };
      (* Then write the actual reply for channel 0 *)
      Hegel.Protocol.write_packet wr1
        {
          Hegel.Protocol.channel = 0;
          message_id = 99;
          is_reply = true;
          payload = "the reply";
        };
      let got = Hegel.Protocol.Channel.receive_response ch0 99 in
      check "receive_response skips other channel" (got = "the reply"))

(* ================================================================ *)
(* Protocol failwith path tests                                     *)
(* ================================================================ *)

(* receive_response gets a non-reply on the same channel *)
let test_protocol_receive_response_wrong_type () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      (* Send a non-reply on channel 0 — receive_response expects a reply *)
      Hegel.Protocol.write_packet wr
        {
          Hegel.Protocol.channel = 0;
          message_id = 99;
          is_reply = false;
          payload = "not a reply";
        };
      check_raises_failure "receive_response non-reply" (fun () ->
          ignore (Hegel.Protocol.Channel.receive_response ch 99)))

(* receive_request gets a reply on the same channel *)
let test_protocol_receive_request_unexpected_reply () =
  let rd, wr = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close rd;
      Unix.close wr)
    (fun () ->
      let conn = Hegel.Protocol.Connection.create rd in
      let ch = Hegel.Protocol.Connection.control_channel conn in
      (* Send a reply on channel 0 — receive_request expects a non-reply *)
      Hegel.Protocol.write_packet wr
        {
          Hegel.Protocol.channel = 0;
          message_id = 1;
          is_reply = true;
          payload = "unexpected reply";
        };
      check_raises_failure "receive_request unexpected reply" (fun () ->
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

let test_runner_normal () =
  (* Normal flow: 1 test case passes, test_done with empty results *)
  run_with_mock_mode "normal" (fun () -> ());
  ()

let test_runner_version_fail () =
  check_raises_failure "runner version fail" (fun () ->
      run_with_mock_mode "version_fail" (fun () -> ()))

let test_runner_unknown_event () =
  (* Unknown event is acked and skipped, then normal test_case runs *)
  run_with_mock_mode "unknown_event" (fun () -> ());
  ()

let test_runner_no_results () =
  (* test_done without results field — result_data stays Null *)
  run_with_mock_mode "no_results" (fun () -> ());
  ()

let test_runner_failed () =
  check_raises_failure "runner failed test" (fun () ->
      run_with_mock_mode "failed" (fun () -> ()))

let test_runner_with_replay () =
  check_raises_failure "runner with replay" (fun () ->
      run_with_mock_mode "with_replay" (fun () -> ()))

let test_runner_no_event_key () =
  (* Event with no "event" key → event_type defaults to "" → unknown event ack *)
  run_with_mock_mode "no_event_key" (fun () -> ());
  ()

let test_runner_non_int_interesting () =
  (* interesting_test_cases is non-integer → as_int returns None → 0 replays *)
  run_with_mock_mode "non_int_interesting" (fun () -> ());
  ()

let test_runner_early_close () =
  (* Mock closes connection before runner can close channels →
     exercises the (try ... with _ -> ()) error handlers *)
  run_with_mock_mode "early_close" (fun () -> ());
  ()

let test_runner_slow_start () =
  (* Mock creates a regular file first before real socket, triggering
     the connect retry loop (Unix.Unix_error catch in the for loop) *)
  run_with_mock_mode "slow_start" (fun () -> ());
  ()

let test_runner_interesting_pass () =
  (* test_fn raises but server says passed=true →
     exercises the || !got_interesting branch where got_interesting=true *)
  check_raises_failure "runner interesting pass" (fun () ->
      run_with_mock_mode "interesting_pass" (fun () -> failwith "test error"))

(* ================================================================ *)
(* CBOR encoding: test large u64 values                             *)
(* ================================================================ *)

let test_cbor_large_values () =
  (* Test value that requires 4-byte encoding: > 65535 *)
  let v = Hegel.Cbor.Unsigned 100000 in
  check "large unsigned 100000" (roundtrip v = v);
  (* Test value that requires 8-byte encoding: > 2^32 *)
  let v = Hegel.Cbor.Unsigned 5000000000 in
  check "large unsigned 5 billion" (roundtrip v = v);
  (* Negative that requires multi-byte *)
  let v = Hegel.Cbor.Negative (-100000) in
  check "large negative -100000" (roundtrip v = v)

(* ================================================================ *)
(* CBOR decode_head with u64                                        *)
(* ================================================================ *)

let test_cbor_decode_head_u64 () =
  (* Create a value that needs 8-byte encoding *)
  let v = Hegel.Cbor.Unsigned 5000000000 in
  let encoded = Hegel.Cbor.encode_to_string v in
  let decoded = Hegel.Cbor.decode_string encoded in
  check "u64 decode roundtrip" (decoded = v)

(* ================================================================ *)
(* Main                                                             *)
(* ================================================================ *)

let () =
  (* CBOR *)
  test_cbor_unsigned ();
  test_cbor_negative ();
  test_cbor_text ();
  test_cbor_bytes ();
  test_cbor_array ();
  test_cbor_map ();
  test_cbor_bool ();
  test_cbor_null ();
  test_cbor_float ();
  test_cbor_tag ();
  test_cbor_encode_head_sizes ();
  test_cbor_half_float ();
  test_cbor_single_float ();
  test_cbor_decode_errors ();
  test_cbor_map_get ();
  test_cbor_map_get_exn ();
  test_cbor_as_helpers ();
  test_cbor_to_diagnostic ();
  test_cbor_large_values ();
  test_cbor_decode_head_u64 ();
  (* CRC32 *)
  test_crc32 ();
  (* State *)
  test_state_last_run ();
  test_state_test_aborted ();
  test_state_generated_values ();
  test_state_connection ();
  (* Gen helpers *)
  test_gen_string_contains ();
  test_gen_base64_decode ();
  test_gen_cbor_helpers ();
  test_gen_one_of_empty ();
  test_gen_sampled_from_empty ();
  test_labels ();
  (* Protocol *)
  test_protocol_packet_roundtrip ();
  test_protocol_packet_reply ();
  test_protocol_packet_empty_payload ();
  test_protocol_connection ();
  test_protocol_channel_send_receive ();
  test_protocol_connect_channel ();
  test_protocol_channel_close ();
  test_protocol_connection_close ();
  test_protocol_send_response ();
  test_protocol_receive_response ();
  test_protocol_receive_request ();
  test_protocol_channel_routing ();
  test_protocol_pending_queue ();
  (* Protocol error paths *)
  test_protocol_invalid_magic ();
  test_protocol_invalid_terminator ();
  test_protocol_checksum_mismatch ();
  test_protocol_read_closed_fd ();
  test_protocol_write_closed_fd ();
  test_protocol_request_cbor_error ();
  test_protocol_request_cbor_non_text_error ();
  test_protocol_request_cbor_error_no_type ();
  test_protocol_request_cbor_no_result ();
  (* State connection lifecycle *)
  test_state_connection_lifecycle ();
  (* Runner edge cases *)
  test_extract_channel_id ();
  test_find_hegel_path ();
  test_find_hegel_path_not_found ();
  test_run_no_hegel_path ();
  (* Protocol receive_response buffering *)
  test_protocol_receive_response_with_buffering ();
  (* Gen error paths *)
  test_generate_raw_aborted ();
  test_start_span_aborted ();
  test_base64_invalid_char ();
  test_generate_raw_stop_test ();
  test_generate_raw_overflow ();
  test_generate_raw_overrun ();
  test_start_span_stop_test ();
  test_stop_span_error ();
  test_new_collection_non_text ();
  test_collection_more_non_bool ();
  test_non_stop_test_error_exits_134 ();
  (* Protocol failwith paths *)
  test_protocol_receive_response_wrong_type ();
  test_protocol_receive_request_unexpected_reply ();
  (* Mock hegel runner tests *)
  test_runner_normal ();
  test_runner_version_fail ();
  test_runner_unknown_event ();
  test_runner_no_results ();
  test_runner_failed ();
  test_runner_with_replay ();
  test_runner_no_event_key ();
  test_runner_non_int_interesting ();
  test_runner_early_close ();
  test_runner_slow_start ();
  test_runner_interesting_pass ();
  (* Summary *)
  if !failures > 0 then (
    Printf.eprintf "%d test(s) FAILED\n%!" !failures;
    exit 1)
  else Printf.printf "All unit tests passed.\n%!"
