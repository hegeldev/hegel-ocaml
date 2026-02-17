(* Mock hegel server for testing runner.ml coverage.
   Spawned by Runner.run via Unix.create_process.
   argv: mock_hegel socket_path [--verbosity normal --test-cases N]
   Behavior controlled by MOCK_HEGEL_MODE environment variable. *)

let mode () = try Sys.getenv "MOCK_HEGEL_MODE" with Not_found -> "normal"

let cbor_result v =
  Hegel.Cbor.encode_to_string (Hegel.Cbor.Map [ (Hegel.Cbor.Text "result", v) ])

let send_request fd channel payload =
  let msg_id = 1 in
  Hegel.Protocol.write_packet fd
    { Hegel.Protocol.channel; message_id = msg_id; is_reply = false; payload };
  msg_id

(* Handle a test case channel: read mark_complete, respond, read close *)
let handle_test_case fd tc_channel_id =
  (* Read mark_complete request *)
  let pkt = Hegel.Protocol.read_packet fd in
  assert (pkt.Hegel.Protocol.channel = tc_channel_id);
  let resp = cbor_result Hegel.Cbor.Null in
  Hegel.Protocol.write_packet fd
    {
      Hegel.Protocol.channel = tc_channel_id;
      message_id = pkt.message_id;
      is_reply = true;
      payload = resp;
    };
  (* Read close packet *)
  let _close_pkt = Hegel.Protocol.read_packet fd in
  ()

let next_tc_channel = ref 2

let send_test_case fd test_channel_id =
  let tc_id = !next_tc_channel in
  next_tc_channel := tc_id + 2;
  let event =
    Hegel.Cbor.encode_to_string
      (Hegel.Cbor.Map
         [
           (Hegel.Cbor.Text "event", Hegel.Cbor.Text "test_case");
           (Hegel.Cbor.Text "channel", Hegel.Cbor.Unsigned tc_id);
         ])
  in
  let _msg_id = send_request fd test_channel_id event in
  (* Read ack *)
  let _ack = Hegel.Protocol.read_packet fd in
  tc_id

let send_test_done fd test_channel_id results =
  let fields =
    [ (Hegel.Cbor.Text "event", Hegel.Cbor.Text "test_done") ]
    @
    match results with
    | Some r -> [ (Hegel.Cbor.Text "results", r) ]
    | None -> []
  in
  let event = Hegel.Cbor.encode_to_string (Hegel.Cbor.Map fields) in
  let _msg_id = send_request fd test_channel_id event in
  (* Read ack *)
  let _ack = Hegel.Protocol.read_packet fd in
  ()

let () =
  let socket_path = Sys.argv.(1) in
  let m = mode () in
  (* slow_start: create a regular file first so the runner's connect attempt
     fails with Unix.Unix_error, exercising the retry loop *)
  if m = "slow_start" then (
    let oc = open_out socket_path in
    close_out oc;
    Unix.sleepf 0.3);
  (try Sys.remove socket_path with Sys_error _ -> ());
  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_UNIX socket_path);
  Unix.listen sock 1;
  let client, _ = Unix.accept sock in
  (* Version negotiation *)
  let pkt = Hegel.Protocol.read_packet client in
  assert (
    pkt.Hegel.Protocol.payload = Hegel.Protocol.version_negotiation_message);
  if m = "version_fail" then (
    Hegel.Protocol.write_packet client
      {
        Hegel.Protocol.channel = 0;
        message_id = pkt.message_id;
        is_reply = true;
        payload = "NotOk";
      };
    Unix.close client;
    Unix.close sock;
    exit 0);
  Hegel.Protocol.write_packet client
    {
      Hegel.Protocol.channel = 0;
      message_id = pkt.message_id;
      is_reply = true;
      payload = Hegel.Protocol.version_negotiation_ok;
    };
  (* run_test command *)
  let pkt = Hegel.Protocol.read_packet client in
  let run_test = Hegel.Cbor.decode_string pkt.payload in
  let test_channel_id =
    match Hegel.Cbor.map_get run_test "channel" with
    | Some v -> (
        match Hegel.Cbor.as_int v with
        | Some n -> n
        | _ -> failwith "bad channel")
    | None -> failwith "missing channel"
  in
  (* Ack run_test *)
  Hegel.Protocol.write_packet client
    {
      Hegel.Protocol.channel = 0;
      message_id = pkt.message_id;
      is_reply = true;
      payload = cbor_result Hegel.Cbor.Null;
    };
  (match m with
  | "unknown_event" ->
      (* Send unknown event first *)
      let unk_event =
        Hegel.Cbor.encode_to_string
          (Hegel.Cbor.Map
             [ (Hegel.Cbor.Text "event", Hegel.Cbor.Text "something_weird") ])
      in
      let _msg_id = send_request client test_channel_id unk_event in
      let _ack = Hegel.Protocol.read_packet client in
      (* Then send 1 normal test_case *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      (* test_done with normal results *)
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map
              [
                (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool true);
                (Hegel.Cbor.Text "interesting_test_cases", Hegel.Cbor.Unsigned 0);
              ]))
  | "no_results" ->
      (* Send 1 test_case, then test_done without results *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id None
  | "failed" ->
      (* Send 1 test_case, then test_done with passed=false *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map [ (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool false) ]))
  | "with_replay" ->
      (* Send 1 test_case, then test_done with interesting_test_cases=1, then replay *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map
              [
                (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool false);
                (Hegel.Cbor.Text "interesting_test_cases", Hegel.Cbor.Unsigned 1);
              ]));
      (* Send replay test_case *)
      let tc_id2 = send_test_case client test_channel_id in
      handle_test_case client tc_id2
  | "no_event_key" ->
      (* Send event without "event" key, then normal test_case, then test_done *)
      let bad_event =
        Hegel.Cbor.encode_to_string
          (Hegel.Cbor.Map [ (Hegel.Cbor.Text "data", Hegel.Cbor.Unsigned 99) ])
      in
      let _msg_id = send_request client test_channel_id bad_event in
      let _ack = Hegel.Protocol.read_packet client in
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map [ (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool true) ]))
  | "non_int_interesting" ->
      (* test_done with interesting_test_cases as non-integer (Text) *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map
              [
                (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool true);
                ( Hegel.Cbor.Text "interesting_test_cases",
                  Hegel.Cbor.Text "not_a_number" );
              ]))
  | "early_close" ->
      (* Send test_case, handle it, send test_done, then shutdown+close
         immediately — forces the runner's close calls to hit EPIPE *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id (Some (Hegel.Cbor.Map []));
      (* Aggressive shutdown to ensure runner gets EPIPE *)
      (try Unix.shutdown client Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
      Unix.close client;
      Unix.close sock;
      exit 0
  | "interesting_pass" ->
      (* test_fn raises but server says passed=true.
         This exercises got_interesting=true with passed=true,
         covering the || !got_interesting branch *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id
        (Some
           (Hegel.Cbor.Map
              [
                (Hegel.Cbor.Text "passed", Hegel.Cbor.Bool true);
                (Hegel.Cbor.Text "interesting_test_cases", Hegel.Cbor.Unsigned 1);
              ]));
      (* Replay test_case *)
      let tc_id2 = send_test_case client test_channel_id in
      handle_test_case client tc_id2
  | _ ->
      (* "normal": 1 test_case, test_done with empty results *)
      let tc_id = send_test_case client test_channel_id in
      handle_test_case client tc_id;
      send_test_done client test_channel_id (Some (Hegel.Cbor.Map [])));
  (* Drain any remaining close packets *)
  (try
     while true do
       ignore (Hegel.Protocol.read_packet client)
     done
   with _ -> ());
  Unix.close client;
  Unix.close sock
