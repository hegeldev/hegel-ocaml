open Hegel.Connection
open Hegel.Generators

(* ==== Socketpair helpers (same pattern as test_client.ml) ==== *)

(** Helper: create a fake server and client, run both, then join. *)
let with_fake_server server_fn client_fn =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t = Thread.create server_fn server_conn in
  client_fn client_conn;
  close client_conn;
  close server_conn;
  Thread.join t

(** Helper: accept run_test on control channel and return the test channel. *)
let accept_run_test server_conn =
  receive_handshake server_conn;
  let control = control_channel server_conn in
  let msg_id, message = receive_request control () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let test_ch_id =
    Int32.of_int
      (Hegel.Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
  in
  let test_channel = connect_channel server_conn test_ch_id ~role:"Test" () in
  send_response_value control msg_id (`Bool true);
  test_channel

(** Helper: send a test_case event and return the data channel. *)
let send_test_case server_conn test_channel =
  let data_ch = new_channel server_conn ~role:"Data" () in
  let req_id =
    send_request test_channel
      (`Map
         [
           (`Text "event", `Text "test_case");
           (`Text "channel", `Int (Int32.to_int data_ch.channel_id));
         ])
  in
  ignore (receive_response_raw test_channel req_id ());
  data_ch

(** Helper: send test_done with given interesting count. *)
let send_test_done test_channel ~interesting =
  ignore
    (pending_get
       (request test_channel
          (`Map
             [
               (`Text "event", `Text "test_done");
               ( `Text "results",
                 `Map
                   [
                     (`Text "passed", `Bool (interesting = 0));
                     (`Text "test_cases", `Int 1);
                     ( `Text "valid_test_cases",
                       `Int (if interesting = 0 then 1 else 0) );
                     (`Text "invalid_test_cases", `Int 0);
                     (`Text "interesting_test_cases", `Int interesting);
                   ] );
             ])))

(** Helper: receive a request, extract command name and pairs. *)
let recv_command data_ch =
  let msg_id, message = receive_request data_ch () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let cmd =
    Hegel.Cbor_helpers.extract_string (List.assoc (`Text "command") pairs)
  in
  (msg_id, cmd, pairs)

(** Helper: handle one command, dispatching to the appropriate handler. Returns
    the command name for verification. *)
let handle_one data_ch respond =
  let msg_id, cmd, pairs = recv_command data_ch in
  respond data_ch msg_id cmd pairs;
  cmd

(** Standard responder: generate returns `Int value, start/stop span return
    Null, mark_complete returns Null. *)
let standard_respond ?(generate_value = `Int 42) () data_ch msg_id cmd _pairs =
  match cmd with
  | "generate" -> send_response_value data_ch msg_id generate_value
  | "start_span" | "stop_span" | "mark_complete" ->
      send_response_value data_ch msg_id `Null
  | other ->
      failwith (Printf.sprintf "Unexpected command in standard: %s" other)

(** Helper: handle N commands with a standard responder. *)
let handle_n_commands data_ch n ?(generate_value = `Int 42) () =
  for _ = 1 to n do
    ignore (handle_one data_ch (standard_respond ~generate_value ()))
  done

(* ==== Unit tests (no server needed) ==== *)

let test_span_label_constants () =
  let open Labels in
  Alcotest.(check int) "LIST" 1 list;
  Alcotest.(check int) "LIST_ELEMENT" 2 list_element;
  Alcotest.(check int) "SET" 3 set;
  Alcotest.(check int) "SET_ELEMENT" 4 set_element;
  Alcotest.(check int) "MAP" 5 map;
  Alcotest.(check int) "MAP_ENTRY" 6 map_entry;
  Alcotest.(check int) "TUPLE" 7 tuple;
  Alcotest.(check int) "ONE_OF" 8 one_of;
  Alcotest.(check int) "OPTIONAL" 9 optional;
  Alcotest.(check int) "FIXED_DICT" 10 fixed_dict;
  Alcotest.(check int) "FLAT_MAP" 11 flat_map;
  Alcotest.(check int) "FILTER" 12 filter;
  Alcotest.(check int) "MAPPED" 13 mapped;
  Alcotest.(check int) "SAMPLED_FROM" 14 sampled_from;
  Alcotest.(check int) "ENUM_VARIANT" 15 enum_variant

let test_basic_generator_schema () =
  let gen = integers ~min_value:0 ~max_value:10 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "integer" typ;
      let min_v =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_value") pairs)
      in
      Alcotest.(check int) "min_value" 0 min_v;
      let max_v =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_value") pairs)
      in
      Alcotest.(check int) "max_value" 10 max_v
  | None -> Alcotest.fail "expected Some schema"

let test_basic_generator_no_bounds () =
  let gen = integers () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check int) "pairs count" 1 (List.length pairs)
  | None -> Alcotest.fail "expected schema"

let test_map_on_basic_preserves_schema () =
  let gen = integers ~min_value:0 ~max_value:10 () in
  let mapped = map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2)) gen in
  Alcotest.(check bool) "still basic" true (is_basic mapped);
  match schema mapped with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type preserved" "integer" typ
  | None -> Alcotest.fail "expected schema"

let test_double_map_on_basic () =
  let gen = integers () in
  let m1 = map (fun _ -> `Int 2) gen in
  let m2 = map (fun _ -> `Int 3) m1 in
  Alcotest.(check bool) "double map still basic" true (is_basic m2);
  (* Verify schema is preserved through both maps *)
  match (schema gen, schema m2) with
  | Some s1, Some s2 ->
      Alcotest.(check bool)
        "schema unchanged" true
        (CBOR.Simple.encode s1 = CBOR.Simple.encode s2)
  | _ -> Alcotest.fail "expected both schemas"

let test_map_on_non_basic_creates_mapped () =
  let gen = integers () in
  let filtered = filter (fun _ -> true) gen in
  let mapped = map (fun x -> x) filtered in
  Alcotest.(check bool) "not basic" false (is_basic mapped)

let test_flat_map_not_basic () =
  let gen = integers () in
  let fm = flat_map (fun _ -> integers ()) gen in
  Alcotest.(check bool) "not basic" false (is_basic fm)

let test_filter_not_basic () =
  let gen = integers () in
  let f = filter (fun _ -> true) gen in
  Alcotest.(check bool) "not basic" false (is_basic f)

let test_schema_on_non_basic () =
  let gen = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "no schema" true (schema gen = None)

let test_as_basic_on_basic () =
  let gen = integers ~min_value:1 ~max_value:5 () in
  match as_basic gen with
  | Some (s, None) ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "integer" typ
  | Some (_, Some _) -> Alcotest.fail "expected no transform"
  | None -> Alcotest.fail "expected Some"

let test_as_basic_on_basic_with_transform () =
  let gen = map (fun x -> x) (integers ()) in
  match as_basic gen with
  | Some (_, Some _) -> ()
  | Some (_, None) -> Alcotest.fail "expected transform"
  | None -> Alcotest.fail "expected Some"

let test_as_basic_on_non_basic () =
  let gen = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "as_basic is None" true (as_basic gen = None)

let test_max_filter_attempts () =
  Alcotest.(check int) "max attempts" 3 max_filter_attempts

let test_collection_new () =
  let coll = new_collection ~min_size:0 ~max_size:5 () in
  Alcotest.(check bool) "not finished" false coll.finished

let test_collection_new_no_max () =
  let coll = new_collection ~min_size:0 () in
  Alcotest.(check bool) "not finished" false coll.finished;
  Alcotest.(check bool) "max_size is None" true (coll.max_size = None)

let test_collection_reject_when_finished () =
  let coll = new_collection ~min_size:0 () in
  coll.finished <- true;
  (* Should be a no-op, not send any commands *)
  collection_reject coll

(* ==== Socketpair-based tests (fake server protocol verification) ==== *)

(** Test: Basic generator without transform sends generate command. *)
let test_basic_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Handle generate request → respond with `Int 42 *)
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "cmd" "generate" cmd;
      send_response_value data_ch msg_id (`Int 42);
      (* Handle mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"basic_gen" ~test_cases:1 (fun () ->
          let gen = integers ~min_value:0 ~max_value:100 () in
          let v = generate gen in
          Alcotest.(check int) "value" 42 (Hegel.Cbor_helpers.extract_int v)))

(** Test: Basic generator with transform applies transform. *)
let test_basic_generate_with_transform_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Handle generate → respond with `Int 5 *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Int 5);
      (* Handle mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"transform_gen" ~test_cases:1 (fun () ->
          let gen =
            map
              (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
              (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 10 (Hegel.Cbor_helpers.extract_int v)))

(** Test: Double-map on basic composes transforms correctly via socketpair. *)
let test_double_map_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Handle generate → respond with `Int 3 *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Int 3);
      (* Handle mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"double_map" ~test_cases:1 (fun () ->
          let gen =
            integers ~min_value:1 ~max_value:5 ()
            |> map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
            |> map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v + 1))
          in
          (* schema should be unchanged *)
          Alcotest.(check bool) "still basic" true (is_basic gen);
          let v = generate gen in
          (* 3*2+1 = 7 *)
          Alcotest.(check int) "value" 7 (Hegel.Cbor_helpers.extract_int v)))

(** Test: Mapped generator on non-basic sends start_span(MAPPED)/stop_span. The
    inner filter also sends span commands, so the full sequence is:
    start_span(MAPPED), start_span(FILTER), generate, stop_span, stop_span,
    mark_complete. *)
let test_mapped_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      let cmds = Array.make 6 "" in
      let span_labels = Array.make 6 0 in
      for i = 0 to 5 do
        let msg_id, cmd, pairs = recv_command data_ch in
        cmds.(i) <- cmd;
        match cmd with
        | "start_span" ->
            let label =
              Hegel.Cbor_helpers.extract_int (List.assoc (`Text "label") pairs)
            in
            span_labels.(i) <- label;
            send_response_value data_ch msg_id `Null
        | "generate" -> send_response_value data_ch msg_id (`Int 7)
        | "stop_span" -> send_response_value data_ch msg_id `Null
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
      done;
      Alcotest.(check string) "cmd 0" "start_span" cmds.(0);
      Alcotest.(check int) "label 0 MAPPED" Labels.mapped span_labels.(0);
      Alcotest.(check string) "cmd 1" "start_span" cmds.(1);
      Alcotest.(check int) "label 1 FILTER" Labels.filter span_labels.(1);
      Alcotest.(check string) "cmd 2" "generate" cmds.(2);
      Alcotest.(check string) "cmd 3" "stop_span" cmds.(3);
      Alcotest.(check string) "cmd 4" "stop_span" cmds.(4);
      Alcotest.(check string) "cmd 5" "mark_complete" cmds.(5);
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"mapped_gen" ~test_cases:1 (fun () ->
          let gen = integers () |> filter (fun _ -> true) |> map (fun x -> x) in
          let v = generate gen in
          Alcotest.(check int) "value" 7 (Hegel.Cbor_helpers.extract_int v)))

(** Test: FlatMapped generator sends start_span(FLAT_MAP)/stop_span. *)
let test_flatmapped_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Expect: start_span(FLAT_MAP=11), generate(first), generate(second),
         stop_span, mark_complete *)
      let cmds = Array.make 5 "" in
      for i = 0 to 4 do
        let msg_id, cmd, pairs = recv_command data_ch in
        cmds.(i) <- cmd;
        match cmd with
        | "start_span" ->
            let label =
              Hegel.Cbor_helpers.extract_int (List.assoc (`Text "label") pairs)
            in
            Alcotest.(check int) "label is FLAT_MAP" Labels.flat_map label;
            send_response_value data_ch msg_id `Null
        | "generate" -> send_response_value data_ch msg_id (`Int 5)
        | "stop_span" -> send_response_value data_ch msg_id `Null
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
      done;
      Alcotest.(check string) "cmd 0" "start_span" cmds.(0);
      Alcotest.(check string) "cmd 1" "generate" cmds.(1);
      Alcotest.(check string) "cmd 2" "generate" cmds.(2);
      Alcotest.(check string) "cmd 3" "stop_span" cmds.(3);
      Alcotest.(check string) "cmd 4" "mark_complete" cmds.(4);
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"flatmap_gen" ~test_cases:1 (fun () ->
          let gen =
            flat_map
              (fun _ -> integers ~min_value:0 ~max_value:10 ())
              (integers ~min_value:1 ~max_value:3 ())
          in
          ignore (generate gen)))

(** Test: Filtered generator — passes on first attempt. *)
let test_filtered_pass_first_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Expect: start_span(FILTER=12), generate, stop_span(discard=false),
         mark_complete *)
      let cmds = Array.make 4 "" in
      for i = 0 to 3 do
        let msg_id, cmd, pairs = recv_command data_ch in
        cmds.(i) <- cmd;
        match cmd with
        | "start_span" ->
            let label =
              Hegel.Cbor_helpers.extract_int (List.assoc (`Text "label") pairs)
            in
            Alcotest.(check int) "label is FILTER" Labels.filter label;
            send_response_value data_ch msg_id `Null
        | "generate" -> send_response_value data_ch msg_id (`Int 4)
        | "stop_span" ->
            let discard =
              Hegel.Cbor_helpers.extract_bool
                (List.assoc (`Text "discard") pairs)
            in
            Alcotest.(check bool) "discard false" false discard;
            send_response_value data_ch msg_id `Null
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"filter_pass" ~test_cases:1 (fun () ->
          let gen =
            filter
              (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
              (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 4 (Hegel.Cbor_helpers.extract_int v)))

(** Test: Filtered generator — fails first, passes second. *)
let test_filtered_pass_after_reject_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Attempt 1: start_span, generate(odd=3), stop_span(discard=true)
         Attempt 2: start_span, generate(even=4), stop_span(discard=false)
         Then mark_complete *)
      let generate_count = ref 0 in
      let cmd_count = ref 0 in
      for _ = 1 to 7 do
        let msg_id, cmd, pairs = recv_command data_ch in
        incr cmd_count;
        match cmd with
        | "start_span" -> send_response_value data_ch msg_id `Null
        | "generate" ->
            incr generate_count;
            if !generate_count = 1 then
              send_response_value data_ch msg_id (`Int 3)
            else send_response_value data_ch msg_id (`Int 4)
        | "stop_span" ->
            let discard =
              Hegel.Cbor_helpers.extract_bool
                (List.assoc (`Text "discard") pairs)
            in
            if !generate_count = 1 then
              Alcotest.(check bool) "first discard=true" true discard
            else Alcotest.(check bool) "second discard=false" false discard;
            send_response_value data_ch msg_id `Null
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
      done;
      Alcotest.(check int) "total cmds" 7 !cmd_count;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"filter_retry" ~test_cases:1 (fun () ->
          let gen =
            filter
              (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
              (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 4 (Hegel.Cbor_helpers.extract_int v)))

(** Test: Filtered generator — all 3 attempts fail → assume(false) → INVALID. *)
let test_filtered_exhaustion_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* 3 attempts: each is start_span, generate(odd), stop_span(discard=true)
         Then mark_complete with status=INVALID *)
      for _ = 1 to 3 do
        (* start_span *)
        let msg_id, _cmd, _pairs = recv_command data_ch in
        send_response_value data_ch msg_id `Null;
        (* generate *)
        let msg_id, _cmd, _pairs = recv_command data_ch in
        send_response_value data_ch msg_id (`Int 3);
        (* stop_span *)
        let msg_id, _cmd, _pairs = recv_command data_ch in
        send_response_value data_ch msg_id `Null
      done;
      (* mark_complete with INVALID *)
      let msg_id, msg = receive_request data_ch () in
      let pairs = Hegel.Cbor_helpers.extract_dict msg in
      let status =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "status") pairs)
      in
      Alcotest.(check string) "status" "INVALID" status;
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"filter_exhaust" ~test_cases:1 (fun () ->
          let gen =
            filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ())
          in
          ignore (generate gen)))

(** Test: group helper wraps start_span/stop_span correctly. *)
let test_group_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Expect: start_span(42), stop_span, mark_complete *)
      let msg_id, cmd, pairs = recv_command data_ch in
      Alcotest.(check string) "start_span" "start_span" cmd;
      let label =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "label") pairs)
      in
      Alcotest.(check int) "label" 42 label;
      send_response_value data_ch msg_id `Null;
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "stop_span" "stop_span" cmd;
      send_response_value data_ch msg_id `Null;
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"group" ~test_cases:1 (fun () ->
          let result = group 42 (fun () -> `Int 99) in
          Alcotest.(check int)
            "result" 99
            (Hegel.Cbor_helpers.extract_int result)))

(** Test: discardable_group with success — stop_span(discard=false). *)
let test_discardable_group_success_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* start_span, stop_span(discard=false), mark_complete *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id `Null;
      let msg_id, _cmd, pairs = recv_command data_ch in
      let discard =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text "discard") pairs)
      in
      Alcotest.(check bool) "discard false" false discard;
      send_response_value data_ch msg_id `Null;
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"disc_ok" ~test_cases:1 (fun () ->
          let result = discardable_group 1 (fun () -> `Int 55) in
          Alcotest.(check int)
            "result" 55
            (Hegel.Cbor_helpers.extract_int result)))

(** Test: discardable_group with exception — stop_span(discard=true). *)
let test_discardable_group_exception_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* start_span, stop_span(discard=true), mark_complete(INTERESTING) *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id `Null;
      let msg_id, _cmd, pairs = recv_command data_ch in
      let discard =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text "discard") pairs)
      in
      Alcotest.(check bool) "discard true" true discard;
      send_response_value data_ch msg_id `Null;
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"disc_err" ~test_cases:1 (fun () ->
          (try ignore (discardable_group 1 (fun () -> failwith "test error"))
           with Failure _ -> ());
          ()))

(** Test: collection new_collection / collection_more / collection_reject
    protocol. *)
let test_collection_protocol_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* 1. new_collection command *)
      let msg_id, cmd, pairs = recv_command data_ch in
      Alcotest.(check string) "new_collection" "new_collection" cmd;
      let min_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "min_size" 1 min_size;
      let max_size =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "max_size" 3 max_size;
      send_response_value data_ch msg_id (`Text "coll_handle");
      (* 2. collection_more → true *)
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "collection_more" "collection_more" cmd;
      send_response_value data_ch msg_id (`Bool true);
      (* 3. generate (element) *)
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "generate" "generate" cmd;
      send_response_value data_ch msg_id (`Int 7);
      (* 4. collection_reject *)
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "collection_reject" "collection_reject" cmd;
      send_response_value data_ch msg_id `Null;
      (* 5. collection_more → false *)
      let msg_id, cmd, _pairs = recv_command data_ch in
      Alcotest.(check string) "collection_more 2" "collection_more" cmd;
      send_response_value data_ch msg_id (`Bool false);
      (* 6. mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"coll_proto" ~test_cases:1 (fun () ->
          let coll = new_collection ~min_size:1 ~max_size:3 () in
          Alcotest.(check bool) "more 1" true (collection_more coll);
          (* Generate an element *)
          ignore
            (Hegel.Client.generate_from_schema
               (`Map [ (`Text "type", `Text "integer") ]));
          (* Reject it *)
          collection_reject coll;
          (* Collection says done *)
          Alcotest.(check bool) "more 2" false (collection_more coll);
          (* After finished, more returns false immediately *)
          Alcotest.(check bool) "more 3" false (collection_more coll);
          (* reject is no-op when finished *)
          collection_reject coll))

(** Test: collection with no max_size sends Null for max_size. *)
let test_collection_no_max_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* new_collection with max_size=Null *)
      let msg_id, cmd, pairs = recv_command data_ch in
      Alcotest.(check string) "new_collection" "new_collection" cmd;
      let max_size_val = List.assoc (`Text "max_size") pairs in
      Alcotest.(check bool)
        "max_size is null" true
        (Hegel.Cbor_helpers.is_null max_size_val);
      send_response_value data_ch msg_id (`Text "coll2");
      (* collection_more → false *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Bool false);
      (* mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"coll_nomax" ~test_cases:1 (fun () ->
          let coll = new_collection ~min_size:0 () in
          ignore (collection_more coll)))

(** Test: StopTest during new_collection raises Data_exhausted. *)
let test_collection_stoptest_new_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Respond to new_collection with StopTest *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_error data_ch msg_id ~error:"Stop" ~error_type:"StopTest" ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"coll_stop_new" ~test_cases:1 (fun () ->
          let coll = new_collection ~min_size:0 () in
          ignore (collection_more coll)))

(** Test: StopTest during collection_more raises Data_exhausted. *)
let test_collection_stoptest_more_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* new_collection succeeds *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Text "coll_handle");
      (* collection_more responds with StopTest *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_error data_ch msg_id ~error:"Stop" ~error_type:"StopTest" ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"coll_stop_more" ~test_cases:1 (fun () ->
          let coll = new_collection ~min_size:0 () in
          ignore (collection_more coll)))

(** Test: collection_reject when server_name is already cached. *)
let test_collection_reject_cached_name_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* new_collection *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Text "coll_h");
      (* collection_more → true *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Bool true);
      (* collection_reject *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id `Null;
      (* collection_more → false *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Bool false);
      (* mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"coll_rej" ~test_cases:1 (fun () ->
          let coll = new_collection ~min_size:1 ~max_size:3 () in
          Alcotest.(check bool) "more" true (collection_more coll);
          collection_reject coll;
          ignore (collection_more coll)))

(* ==== E2E tests (real hegel binary) ==== *)

(** Test: integers(0, 100) generates values in range. *)
let test_integers_in_range () =
  Hegel.Session.run_hegel_test ~name:"int_range" ~test_cases:10 (fun () ->
      let gen = integers ~min_value:0 ~max_value:100 () in
      let v = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (v >= 0 && v <= 100))

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Hegel.Session.run_hegel_test ~name:"map_double" ~test_cases:10 (fun () ->
      let gen =
        integers ~min_value:1 ~max_value:5 ()
        |> map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
      in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let v = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (v >= 2 && v <= 10);
      assert (v mod 2 = 0))

(** Test: double map composes correctly. *)
let test_double_map_e2e () =
  Hegel.Session.run_hegel_test ~name:"double_map_e2e" ~test_cases:10 (fun () ->
      let gen =
        integers ~min_value:1 ~max_value:5 ()
        |> map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
        |> map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v + 1))
      in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let s = schema gen in
      (match s with
      | Some schema_v ->
          let pairs = Hegel.Cbor_helpers.extract_dict schema_v in
          let typ =
            Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
          in
          Alcotest.(check string) "schema type" "integer" typ
      | None -> Alcotest.fail "expected schema");
      let v = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (List.mem v [ 3; 5; 7; 9; 11 ]))

(** Test: flat_map through server. *)
let test_flat_map_e2e () =
  Hegel.Session.run_hegel_test ~name:"flatmap_e2e" ~test_cases:10 (fun () ->
      let gen =
        flat_map
          (fun v ->
            let n = Hegel.Cbor_helpers.extract_int v in
            integers ~min_value:0 ~max_value:(max 1 n) ())
          (integers ~min_value:1 ~max_value:5 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (v >= 0))

(** Test: filter through server. *)
let test_filter_e2e () =
  Hegel.Session.run_hegel_test ~name:"filter_e2e" ~test_cases:10 (fun () ->
      let gen =
        filter
          (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
          (integers ~min_value:0 ~max_value:100 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.Cbor_helpers.extract_int (generate gen) in
      assert (v mod 2 = 0))

(** Test: filter exhaustion through server (always false → assume false). *)
let test_filter_exhaustion_e2e () =
  Hegel.Session.run_hegel_test ~name:"filter_exhaust_e2e" ~test_cases:10
    (fun () ->
      let gen =
        filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ())
      in
      ignore (generate gen))

(** Test: group helper through server. *)
let test_group_e2e () =
  Hegel.Session.run_hegel_test ~name:"group_e2e" ~test_cases:5 (fun () ->
      let v =
        group Labels.list (fun () ->
            Hegel.Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ]))
      in
      let n = Hegel.Cbor_helpers.extract_int v in
      assert (n >= 0 && n <= 10))

(** Test: discardable_group through server — success path. *)
let test_discardable_group_e2e () =
  Hegel.Session.run_hegel_test ~name:"disc_group_e2e" ~test_cases:5 (fun () ->
      let v =
        discardable_group Labels.tuple (fun () ->
            Hegel.Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ]))
      in
      let n = Hegel.Cbor_helpers.extract_int v in
      assert (n >= 0 && n <= 10))

(* ==== Unit tests for lists / booleans ==== *)

(** Test: booleans() produces a Basic generator with type=boolean schema. *)
let test_booleans_schema () =
  let gen = booleans () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "boolean" typ
  | None -> Alcotest.fail "expected schema"

(** Test: lists(basic_elem) produces a Basic generator with type=list schema. *)
let test_lists_basic_schema () =
  let elem = integers ~min_value:0 ~max_value:10 () in
  let gen = lists elem ~min_size:2 ~max_size:5 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "list" typ;
      let min_s =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "min_size" 2 min_s;
      let max_s =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_size") pairs)
      in
      Alcotest.(check int) "max_size" 5 max_s
  | None -> Alcotest.fail "expected schema"

(** Test: lists(basic_elem) without max_size omits max_size from schema. *)
let test_lists_basic_no_max_schema () =
  let elem = integers () in
  let gen = lists elem () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "no max_size key" false
        (List.mem_assoc (`Text "max_size") pairs);
      let min_s =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "min_size") pairs)
      in
      Alcotest.(check int) "default min_size" 0 min_s
  | None -> Alcotest.fail "expected schema"

(** Test: lists(basic_elem_with_transform) produces a Basic generator whose
    transform applies the element transform to every item in the result list. *)
let test_lists_basic_with_element_transform () =
  (* Build a basic generator with a transform (doubles the value) *)
  let elem =
    map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2)) (integers ())
  in
  Alcotest.(check bool) "elem is_basic" true (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen is_basic" true (is_basic gen);
  (* The generator has a transform *)
  match as_basic gen with
  | Some (_, Some _) -> () (* expected: transform present *)
  | Some (_, None) -> Alcotest.fail "expected list transform"
  | None -> Alcotest.fail "expected basic"

(** Test: lists(non_basic_elem) produces a CompositeList (not Basic). *)
let test_lists_non_basic_is_not_basic () =
  let elem = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "elem not basic" false (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen not basic" false (is_basic gen);
  Alcotest.(check bool) "as_basic None" true (as_basic gen = None)

(* ==== Socketpair tests for lists ==== *)

(** Test: lists(basic_elem) sends a generate command with list schema. *)
let test_lists_basic_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Expect: generate{schema:{type:list,...}}, mark_complete *)
      let msg_id, cmd, pairs = recv_command data_ch in
      Alcotest.(check string) "cmd" "generate" cmd;
      let schema_v = List.assoc (`Text "schema") pairs in
      let schema_pairs = Hegel.Cbor_helpers.extract_dict schema_v in
      let typ =
        Hegel.Cbor_helpers.extract_string
          (List.assoc (`Text "type") schema_pairs)
      in
      Alcotest.(check string) "schema type" "list" typ;
      (* Server returns an array *)
      send_response_value data_ch msg_id (`Array [ `Int 3; `Int 7 ]);
      (* mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"lists_basic" ~test_cases:1 (fun () ->
          let gen = lists (integers ~min_value:0 ~max_value:10 ()) () in
          let v = generate gen in
          let items = Hegel.Cbor_helpers.extract_list v in
          Alcotest.(check int) "length" 2 (List.length items)))

(** Test: lists(basic_elem_with_transform) applies the list transform. *)
let test_lists_basic_with_transform_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* generate → array of [2, 4] *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Array [ `Int 2; `Int 4 ]);
      (* mark_complete *)
      let msg_id, _msg = receive_request data_ch () in
      send_response_value data_ch msg_id `Null;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"lists_transform" ~test_cases:1 (fun () ->
          (* elem doubles values; list transform should double each item *)
          let elem =
            map
              (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
              (integers ~min_value:1 ~max_value:5 ())
          in
          let gen = lists elem () in
          let v = generate gen in
          let items = Hegel.Cbor_helpers.extract_list v in
          (* The server returned [2, 4] as raw; the transform doubles them → [4, 8] *)
          let ints = List.map Hegel.Cbor_helpers.extract_int items in
          Alcotest.(check (list int)) "doubled" [ 4; 8 ] ints))

(** Test: lists(non_basic) uses collection protocol: start_span(LIST),
    new_collection, collection_more loop, stop_span, mark_complete. *)
let test_lists_composite_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Sequence:
         start_span(LIST=1)
         start_span(FILTER=12)   ← from the filter in elem
         new_collection
         collection_more → true
         start_span(FILTER=12)   ← filter attempt for element
         generate
         stop_span               ← filter span
         collection_more → false
         stop_span               ← LIST span
         mark_complete *)
      let steps = ref [] in
      let done_ = ref false in
      while not !done_ do
        let msg_id, cmd, pairs = recv_command data_ch in
        steps := cmd :: !steps;
        match cmd with
        | "start_span" -> send_response_value data_ch msg_id `Null
        | "stop_span" -> send_response_value data_ch msg_id `Null
        | "new_collection" ->
            let min_s =
              Hegel.Cbor_helpers.extract_int
                (List.assoc (`Text "min_size") pairs)
            in
            Alcotest.(check int) "min_size" 0 min_s;
            let max_s = List.assoc (`Text "max_size") pairs in
            Alcotest.(check bool)
              "max_size null" true
              (Hegel.Cbor_helpers.is_null max_s);
            send_response_value data_ch msg_id (`Text "coll1")
        | "collection_more" ->
            (* Return true once, then false *)
            let already_done =
              List.length (List.filter (( = ) "collection_more") !steps) >= 2
            in
            if already_done then begin
              send_response_value data_ch msg_id (`Bool false)
            end
            else send_response_value data_ch msg_id (`Bool true)
        | "generate" -> send_response_value data_ch msg_id (`Int 9)
        | "mark_complete" ->
            send_response_value data_ch msg_id `Null;
            done_ := true
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"lists_composite" ~test_cases:1 (fun () ->
          let elem =
            filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ())
          in
          let gen = lists elem () in
          Alcotest.(check bool) "not basic" false (is_basic gen);
          let v = generate gen in
          let items = Hegel.Cbor_helpers.extract_list v in
          Alcotest.(check int) "one element" 1 (List.length items)))

(** Test: lists(non_basic) with StopTest during collection_more aborts cleanly.
*)
let test_lists_composite_stoptest_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* start_span(LIST) *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id `Null;
      (* new_collection succeeds *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_value data_ch msg_id (`Text "coll_x");
      (* collection_more → StopTest *)
      let msg_id, _cmd, _pairs = recv_command data_ch in
      send_response_error data_ch msg_id ~error:"Stop" ~error_type:"StopTest" ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"lists_stoptest" ~test_cases:1 (fun () ->
          let elem = filter (fun _ -> true) (integers ()) in
          ignore (generate (lists elem ()))))

(* ==== E2E tests for lists ==== *)

(** Test: lists(integers) generates a list where all elements are in range. *)
let test_lists_of_integers_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_ints_e2e" ~test_cases:50 (fun () ->
      let gen =
        lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:3 ()
      in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      Alcotest.(check bool) "max 3" true (List.length items <= 3);
      List.iter
        (fun item ->
          let n = Hegel.Cbor_helpers.extract_int item in
          assert (n >= 0 && n <= 100))
        items)

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let test_lists_booleans_bounds_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_bools_bounds_e2e" ~test_cases:50
    (fun () ->
      let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      let n = List.length items in
      assert (n >= 3 && n <= 5))

(** Test: lists(filtered integers) → all elements satisfy predicate. *)
let test_lists_non_basic_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nonbasic_e2e" ~test_cases:50
    (fun () ->
      let elem =
        filter
          (fun v -> Hegel.Cbor_helpers.extract_int v > 5)
          (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = lists elem ~min_size:1 ~max_size:3 () in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = generate gen in
      let items = Hegel.Cbor_helpers.extract_list v in
      let n = List.length items in
      assert (n >= 1 && n <= 3);
      List.iter
        (fun item ->
          let x = Hegel.Cbor_helpers.extract_int item in
          assert (x > 5))
        items)

(** Test: lists(lists(booleans)) → nested lists work. *)
let test_lists_nested_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nested_e2e" ~test_cases:50
    (fun () ->
      let inner = lists (booleans ()) ~max_size:3 () in
      let gen = lists inner ~max_size:3 () in
      Alcotest.(check bool) "outer is_basic" true (is_basic gen);
      let v = generate gen in
      let outer_items = Hegel.Cbor_helpers.extract_list v in
      assert (List.length outer_items <= 3);
      List.iter
        (fun inner_v ->
          let inner_items = Hegel.Cbor_helpers.extract_list inner_v in
          assert (List.length inner_items <= 3))
        outer_items)

let tests =
  [
    (* Unit tests *)
    Alcotest.test_case "span label constants" `Quick test_span_label_constants;
    Alcotest.test_case "basic generator schema" `Quick
      test_basic_generator_schema;
    Alcotest.test_case "basic generator no bounds" `Quick
      test_basic_generator_no_bounds;
    Alcotest.test_case "map on basic preserves schema" `Quick
      test_map_on_basic_preserves_schema;
    Alcotest.test_case "double map on basic" `Quick test_double_map_on_basic;
    Alcotest.test_case "map on non-basic creates mapped" `Quick
      test_map_on_non_basic_creates_mapped;
    Alcotest.test_case "flat_map not basic" `Quick test_flat_map_not_basic;
    Alcotest.test_case "filter not basic" `Quick test_filter_not_basic;
    Alcotest.test_case "schema on non-basic" `Quick test_schema_on_non_basic;
    Alcotest.test_case "as_basic on basic" `Quick test_as_basic_on_basic;
    Alcotest.test_case "as_basic on basic with transform" `Quick
      test_as_basic_on_basic_with_transform;
    Alcotest.test_case "as_basic on non-basic" `Quick test_as_basic_on_non_basic;
    Alcotest.test_case "max_filter_attempts" `Quick test_max_filter_attempts;
    Alcotest.test_case "collection new" `Quick test_collection_new;
    Alcotest.test_case "collection new no max" `Quick test_collection_new_no_max;
    Alcotest.test_case "collection reject when finished" `Quick
      test_collection_reject_when_finished;
    (* Socketpair tests *)
    Alcotest.test_case "basic generate socketpair" `Quick
      test_basic_generate_socketpair;
    Alcotest.test_case "basic generate with transform socketpair" `Quick
      test_basic_generate_with_transform_socketpair;
    Alcotest.test_case "double map socketpair" `Quick test_double_map_socketpair;
    Alcotest.test_case "mapped generate socketpair" `Quick
      test_mapped_generate_socketpair;
    Alcotest.test_case "flatmapped generate socketpair" `Quick
      test_flatmapped_generate_socketpair;
    Alcotest.test_case "filtered pass first socketpair" `Quick
      test_filtered_pass_first_socketpair;
    Alcotest.test_case "filtered pass after reject socketpair" `Quick
      test_filtered_pass_after_reject_socketpair;
    Alcotest.test_case "filtered exhaustion socketpair" `Quick
      test_filtered_exhaustion_socketpair;
    Alcotest.test_case "group socketpair" `Quick test_group_socketpair;
    Alcotest.test_case "discardable_group success socketpair" `Quick
      test_discardable_group_success_socketpair;
    Alcotest.test_case "discardable_group exception socketpair" `Quick
      test_discardable_group_exception_socketpair;
    Alcotest.test_case "collection protocol socketpair" `Quick
      test_collection_protocol_socketpair;
    Alcotest.test_case "collection no max socketpair" `Quick
      test_collection_no_max_socketpair;
    Alcotest.test_case "collection StopTest new socketpair" `Quick
      test_collection_stoptest_new_socketpair;
    Alcotest.test_case "collection StopTest more socketpair" `Quick
      test_collection_stoptest_more_socketpair;
    Alcotest.test_case "collection reject cached socketpair" `Quick
      test_collection_reject_cached_name_socketpair;
    (* Unit tests for lists/booleans *)
    Alcotest.test_case "booleans schema" `Quick test_booleans_schema;
    Alcotest.test_case "lists basic schema" `Quick test_lists_basic_schema;
    Alcotest.test_case "lists basic no max schema" `Quick
      test_lists_basic_no_max_schema;
    Alcotest.test_case "lists basic with element transform" `Quick
      test_lists_basic_with_element_transform;
    Alcotest.test_case "lists non-basic is not basic" `Quick
      test_lists_non_basic_is_not_basic;
    (* Socketpair tests for lists *)
    Alcotest.test_case "lists basic generate socketpair" `Quick
      test_lists_basic_generate_socketpair;
    Alcotest.test_case "lists basic with transform generate socketpair" `Quick
      test_lists_basic_with_transform_generate_socketpair;
    Alcotest.test_case "lists composite generate socketpair" `Quick
      test_lists_composite_generate_socketpair;
    Alcotest.test_case "lists composite stoptest socketpair" `Quick
      test_lists_composite_stoptest_socketpair;
    (* E2E tests *)
    Alcotest.test_case "integers in range" `Quick test_integers_in_range;
    Alcotest.test_case "map doubles e2e" `Quick test_map_doubles_e2e;
    Alcotest.test_case "double map e2e" `Quick test_double_map_e2e;
    Alcotest.test_case "flat_map e2e" `Quick test_flat_map_e2e;
    Alcotest.test_case "filter e2e" `Quick test_filter_e2e;
    Alcotest.test_case "filter exhaustion e2e" `Quick test_filter_exhaustion_e2e;
    Alcotest.test_case "group e2e" `Quick test_group_e2e;
    Alcotest.test_case "discardable_group e2e" `Quick test_discardable_group_e2e;
    (* E2E tests for lists *)
    Alcotest.test_case "lists of integers e2e" `Quick test_lists_of_integers_e2e;
    Alcotest.test_case "lists booleans bounds e2e" `Quick
      test_lists_booleans_bounds_e2e;
    Alcotest.test_case "lists non-basic e2e" `Quick test_lists_non_basic_e2e;
    Alcotest.test_case "lists nested e2e" `Quick test_lists_nested_e2e;
  ]
