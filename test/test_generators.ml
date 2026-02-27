open Hegel.Connection
open Hegel.Generators

let with_fake_server = Test_helpers.with_fake_server
let accept_run_test = Test_helpers.accept_run_test
let send_test_case = Test_helpers.send_test_case
let send_test_done = Test_helpers.send_test_done
let recv_command = Test_helpers.recv_command

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
  let respond = standard_respond ~generate_value () in
  List.iter (fun _ -> ignore (handle_one data_ch respond)) (List.init n Fun.id)

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
  let mapped = map (fun v -> v * 2) gen in
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
  let m1 = map (fun _ -> 2) gen in
  let m2 = map (fun _ -> 3) m1 in
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
  | Some (s, _transform) ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "integer" typ
  | None -> Alcotest.fail "expected Some"

let test_as_basic_on_basic_with_transform () =
  let gen = map (fun x -> x) (integers ()) in
  match as_basic gen with
  | Some (_, _transform) -> ()
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
          Alcotest.(check int) "value" 42 v))

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
            map (fun v -> v * 2) (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 10 v))

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
            |> map (fun v -> v * 2)
            |> map (fun v -> v + 1)
          in
          (* schema should be unchanged *)
          Alcotest.(check bool) "still basic" true (is_basic gen);
          let v = generate gen in
          (* 3*2+1 = 7 *)
          Alcotest.(check int) "value" 7 v))

(** Test: Mapped generator on non-basic sends start_span(MAPPED)/stop_span. The
    inner filter also sends span commands, so the full sequence is:
    start_span(MAPPED), start_span(FILTER), generate, stop_span, stop_span,
    mark_complete. *)
let test_mapped_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      let rec handle_n_cmds n acc =
        if n = 0 then List.rev acc
        else
          let msg_id, cmd, pairs = recv_command data_ch in
          let label =
            match cmd with
            | "start_span" ->
                let l =
                  Hegel.Cbor_helpers.extract_int
                    (List.assoc (`Text "label") pairs)
                in
                send_response_value data_ch msg_id `Null;
                l
            | "generate" ->
                send_response_value data_ch msg_id (`Int 7);
                0
            | "stop_span" ->
                send_response_value data_ch msg_id `Null;
                0
            | "mark_complete" ->
                send_response_value data_ch msg_id `Null;
                0
            | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
          in
          handle_n_cmds (n - 1) ((cmd, label) :: acc)
      in
      let steps = handle_n_cmds 6 [] in
      let cmd i = fst (List.nth steps i) in
      let label i = snd (List.nth steps i) in
      Alcotest.(check string) "cmd 0" "start_span" (cmd 0);
      Alcotest.(check int) "label 0 MAPPED" Labels.mapped (label 0);
      Alcotest.(check string) "cmd 1" "start_span" (cmd 1);
      Alcotest.(check int) "label 1 FILTER" Labels.filter (label 1);
      Alcotest.(check string) "cmd 2" "generate" (cmd 2);
      Alcotest.(check string) "cmd 3" "stop_span" (cmd 3);
      Alcotest.(check string) "cmd 4" "stop_span" (cmd 4);
      Alcotest.(check string) "cmd 5" "mark_complete" (cmd 5);
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"mapped_gen" ~test_cases:1 (fun () ->
          let gen = integers () |> filter (fun _ -> true) |> map (fun x -> x) in
          let v = generate gen in
          Alcotest.(check int) "value" 7 v))

(** Test: FlatMapped generator sends start_span(FLAT_MAP)/stop_span. *)
let test_flatmapped_generate_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Expect: start_span(FLAT_MAP=11), generate(first), generate(second),
         stop_span, mark_complete *)
      let rec handle_n_cmds n acc =
        if n = 0 then List.rev acc
        else
          let msg_id, cmd, pairs = recv_command data_ch in
          (match cmd with
          | "start_span" ->
              let label =
                Hegel.Cbor_helpers.extract_int
                  (List.assoc (`Text "label") pairs)
              in
              Alcotest.(check int) "label is FLAT_MAP" Labels.flat_map label;
              send_response_value data_ch msg_id `Null
          | "generate" -> send_response_value data_ch msg_id (`Int 5)
          | "stop_span" -> send_response_value data_ch msg_id `Null
          | "mark_complete" -> send_response_value data_ch msg_id `Null
          | other -> failwith (Printf.sprintf "Unexpected command: %s" other));
          handle_n_cmds (n - 1) (cmd :: acc)
      in
      let cmds = handle_n_cmds 5 [] in
      let cmd i = List.nth cmds i in
      Alcotest.(check string) "cmd 0" "start_span" (cmd 0);
      Alcotest.(check string) "cmd 1" "generate" (cmd 1);
      Alcotest.(check string) "cmd 2" "generate" (cmd 2);
      Alcotest.(check string) "cmd 3" "stop_span" (cmd 3);
      Alcotest.(check string) "cmd 4" "mark_complete" (cmd 4);
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
      let rec handle_n_cmds n acc =
        if n = 0 then List.rev acc
        else
          let msg_id, cmd, pairs = recv_command data_ch in
          (match cmd with
          | "start_span" ->
              let label =
                Hegel.Cbor_helpers.extract_int
                  (List.assoc (`Text "label") pairs)
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
          | other -> failwith (Printf.sprintf "Unexpected command: %s" other));
          handle_n_cmds (n - 1) (cmd :: acc)
      in
      ignore (handle_n_cmds 4 []);
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"filter_pass" ~test_cases:1 (fun () ->
          let gen =
            filter
              (fun v -> v mod 2 = 0)
              (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 4 v))

(** Test: Filtered generator — fails first, passes second. *)
let test_filtered_pass_after_reject_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* Attempt 1: start_span, generate(odd=3), stop_span(discard=true)
         Attempt 2: start_span, generate(even=4), stop_span(discard=false)
         Then mark_complete *)
      let rec handle_cmds remaining gen_count =
        if remaining = 0 then gen_count
        else
          let msg_id, cmd, pairs = recv_command data_ch in
          let gen_count' =
            match cmd with
            | "start_span" ->
                send_response_value data_ch msg_id `Null;
                gen_count
            | "generate" ->
                let gc = gen_count + 1 in
                if gc = 1 then send_response_value data_ch msg_id (`Int 3)
                else send_response_value data_ch msg_id (`Int 4);
                gc
            | "stop_span" ->
                let discard =
                  Hegel.Cbor_helpers.extract_bool
                    (List.assoc (`Text "discard") pairs)
                in
                if gen_count = 1 then
                  Alcotest.(check bool) "first discard=true" true discard
                else Alcotest.(check bool) "second discard=false" false discard;
                send_response_value data_ch msg_id `Null;
                gen_count
            | "mark_complete" ->
                send_response_value data_ch msg_id `Null;
                gen_count
            | other -> failwith (Printf.sprintf "Unexpected command: %s" other)
          in
          handle_cmds (remaining - 1) gen_count'
      in
      ignore (handle_cmds 7 0);
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"filter_retry" ~test_cases:1 (fun () ->
          let gen =
            filter
              (fun v -> v mod 2 = 0)
              (integers ~min_value:0 ~max_value:10 ())
          in
          let v = generate gen in
          Alcotest.(check int) "value" 4 v))

(** Test: Filtered generator — all 3 attempts fail → assume(false) → INVALID. *)
let test_filtered_exhaustion_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* 3 attempts: each is start_span, generate(odd), stop_span(discard=true)
         Then mark_complete with status=INVALID *)
      List.iter
        (fun _ ->
          (* start_span *)
          let msg_id, _cmd, _pairs = recv_command data_ch in
          send_response_value data_ch msg_id `Null;
          (* generate *)
          let msg_id, _cmd, _pairs = recv_command data_ch in
          send_response_value data_ch msg_id (`Int 3);
          (* stop_span *)
          let msg_id, _cmd, _pairs = recv_command data_ch in
          send_response_value data_ch msg_id `Null)
        (List.init 3 Fun.id);
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
          let result = group 42 (fun () -> 99) in
          Alcotest.(check int) "result" 99 result))

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
          let result = discardable_group 1 (fun () -> 55) in
          Alcotest.(check int) "result" 55 result))

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
      let v = generate gen in
      assert (v >= 0 && v <= 100))

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Hegel.Session.run_hegel_test ~name:"map_double" ~test_cases:10 (fun () ->
      let gen = integers ~min_value:1 ~max_value:5 () |> map (fun v -> v * 2) in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let v = generate gen in
      assert (v >= 2 && v <= 10);
      assert (v mod 2 = 0))

(** Test: double map composes correctly. *)
let test_double_map_e2e () =
  Hegel.Session.run_hegel_test ~name:"double_map_e2e" ~test_cases:10 (fun () ->
      let gen =
        integers ~min_value:1 ~max_value:5 ()
        |> map (fun v -> v * 2)
        |> map (fun v -> v + 1)
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
      let v = generate gen in
      assert (List.mem v [ 3; 5; 7; 9; 11 ]))

(** Test: flat_map through server. *)
let test_flat_map_e2e () =
  Hegel.Session.run_hegel_test ~name:"flatmap_e2e" ~test_cases:10 (fun () ->
      let gen =
        flat_map
          (fun n -> integers ~min_value:0 ~max_value:(max 1 n) ())
          (integers ~min_value:1 ~max_value:5 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = generate gen in
      assert (v >= 0))

(** Test: filter through server. *)
let test_filter_e2e () =
  Hegel.Session.run_hegel_test ~name:"filter_e2e" ~test_cases:10 (fun () ->
      let gen =
        filter (fun v -> v mod 2 = 0) (integers ~min_value:0 ~max_value:100 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = generate gen in
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
  let elem = map (fun v -> v * 2) (integers ()) in
  Alcotest.(check bool) "elem is_basic" true (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen is_basic" true (is_basic gen);
  (* The generator has a transform — as_basic returns Some *)
  match as_basic gen with
  | Some (_, _transform) -> () (* expected: transform present *)
  | None -> Alcotest.fail "expected basic"

(** Test: lists(non_basic_elem) produces a CompositeList (not Basic). *)
let test_lists_non_basic_is_not_basic () =
  let elem = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "elem not basic" false (is_basic elem);
  let gen = lists elem () in
  Alcotest.(check bool) "gen not basic" false (is_basic gen);
  Alcotest.(check bool) "as_basic None" true (as_basic gen = None)

(** Test: lists basic transform raises on non-array input. *)
let test_lists_basic_non_array_raises () =
  let gen = lists (integers ()) () in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

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
          let items = generate gen in
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
            map (fun v -> v * 2) (integers ~min_value:1 ~max_value:5 ())
          in
          let gen = lists elem () in
          let items = generate gen in
          (* The server returned [2, 4] as raw; the transform doubles them → [4, 8] *)
          Alcotest.(check (list int)) "doubled" [ 4; 8 ] items))

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
      let rec handle_cmds collection_more_count =
        let msg_id, cmd, pairs = recv_command data_ch in
        match cmd with
        | "start_span" ->
            send_response_value data_ch msg_id `Null;
            handle_cmds collection_more_count
        | "stop_span" ->
            send_response_value data_ch msg_id `Null;
            handle_cmds collection_more_count
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
            send_response_value data_ch msg_id (`Text "coll1");
            handle_cmds collection_more_count
        | "collection_more" ->
            let cm = collection_more_count + 1 in
            (* Return true once, then false *)
            if cm >= 2 then send_response_value data_ch msg_id (`Bool false)
            else send_response_value data_ch msg_id (`Bool true);
            handle_cmds cm
        | "generate" ->
            send_response_value data_ch msg_id (`Int 9);
            handle_cmds collection_more_count
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      in
      handle_cmds 0;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"lists_composite" ~test_cases:1 (fun () ->
          let elem =
            filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ())
          in
          let gen = lists elem () in
          Alcotest.(check bool) "not basic" false (is_basic gen);
          let items = generate gen in
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
      let items = generate gen in
      Alcotest.(check bool) "max 3" true (List.length items <= 3);
      List.iter (fun n -> assert (n >= 0 && n <= 100)) items)

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let test_lists_booleans_bounds_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_bools_bounds_e2e" ~test_cases:50
    (fun () ->
      let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let items = generate gen in
      let n = List.length items in
      assert (n >= 3 && n <= 5))

(** Test: lists(filtered integers) → all elements satisfy predicate. *)
let test_lists_non_basic_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nonbasic_e2e" ~test_cases:50
    (fun () ->
      let elem =
        filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = lists elem ~min_size:1 ~max_size:3 () in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let items = generate gen in
      let n = List.length items in
      assert (n >= 1 && n <= 3);
      List.iter (fun x -> assert (x > 5)) items)

(** Test: lists(lists(booleans)) → nested lists work. *)
let test_lists_nested_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nested_e2e" ~test_cases:50
    (fun () ->
      let inner = lists (booleans ()) ~max_size:3 () in
      let gen = lists inner ~max_size:3 () in
      Alcotest.(check bool) "outer is_basic" true (is_basic gen);
      let outer_items = generate gen in
      assert (List.length outer_items <= 3);
      List.iter
        (fun inner_items -> assert (List.length inner_items <= 3))
        outer_items)

(* ==== Unit tests for new generators ==== *)

(** Test: just schema is const null. *)
let test_just_schema () =
  let gen = just 42 in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check int) "one pair" 1 (List.length pairs);
      Alcotest.(check bool)
        "const is null" true
        (Hegel.Cbor_helpers.is_null (List.assoc (`Text "const") pairs))
  | None -> Alcotest.fail "expected schema"

(** Test: just transform ignores server value. *)
let test_just_transform () =
  let gen = just "hello" in
  match gen with
  | Basic { transform; _ } ->
      Alcotest.(check string) "ignores server" "hello" (transform (`Int 999))
  | _ -> Alcotest.fail "expected Basic"

(** Test: from_regex schema with default fullmatch. *)
let test_from_regex_schema () =
  let gen = from_regex "[a-z]+" () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "regex" typ;
      let pat =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "pattern") pairs)
      in
      Alcotest.(check string) "pattern" "[a-z]+" pat;
      let fm =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text "fullmatch") pairs)
      in
      Alcotest.(check bool) "fullmatch" true fm
  | None -> Alcotest.fail "expected schema"

(** Test: from_regex with fullmatch=false. *)
let test_from_regex_no_fullmatch () =
  let gen = from_regex "abc" ~fullmatch:false () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let fm =
        Hegel.Cbor_helpers.extract_bool (List.assoc (`Text "fullmatch") pairs)
      in
      Alcotest.(check bool) "fullmatch false" false fm
  | None -> Alcotest.fail "expected schema"

(** Test: emails schema. *)
let test_emails_schema () =
  let gen = emails () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "email" typ
  | None -> Alcotest.fail "expected schema"

(** Test: urls schema. *)
let test_urls_schema () =
  let gen = urls () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "url" typ
  | None -> Alcotest.fail "expected schema"

(** Test: domains schema without max_length. *)
let test_domains_schema () =
  let gen = domains () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "domain" typ;
      Alcotest.(check bool)
        "no max_length" false
        (List.mem_assoc (`Text "max_length") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: domains schema with max_length. *)
let test_domains_max_length () =
  let gen = domains ~max_length:100 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let ml =
        Hegel.Cbor_helpers.extract_int (List.assoc (`Text "max_length") pairs)
      in
      Alcotest.(check int) "max_length" 100 ml
  | None -> Alcotest.fail "expected schema"

(** Test: dates schema. *)
let test_dates_schema () =
  let gen = dates () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "date" typ
  | None -> Alcotest.fail "expected schema"

(** Test: times schema. *)
let test_times_schema () =
  let gen = times () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "time" typ
  | None -> Alcotest.fail "expected schema"

(** Test: datetimes schema. *)
let test_datetimes_schema () =
  let gen = datetimes () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "datetime" typ
  | None -> Alcotest.fail "expected schema"

(** Test: one_of with < 2 generators raises. *)
let test_one_of_too_few () =
  let raised = ref false in
  (try ignore (one_of [ integers () ]) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: one_of all basic uses tagged-tuple schema. *)
let test_one_of_basic_schema () =
  let gen =
    one_of
      [
        integers ~min_value:0 ~max_value:10 ();
        integers ~min_value:100 ~max_value:200 ();
      ]
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      Alcotest.(check bool)
        "has one_of key" true
        (List.mem_assoc (`Text "one_of") pairs)
  | None -> Alcotest.fail "expected schema"

(** Test: one_of with non-basic generators is not basic. *)
let test_one_of_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = one_of [ integers (); filtered ] in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: one_of tagged-tuple dispatch transform. *)
let test_one_of_dispatch () =
  let gen =
    one_of
      [
        map (fun x -> x * 2) (integers ()); map (fun x -> x + 100) (integers ());
      ]
  in
  match gen with
  | Basic { transform; _ } ->
      (* Tag 0 → first branch, value 5 → 5 * 2 = 10 *)
      let result = transform (`Array [ `Int 0; `Int 5 ]) in
      Alcotest.(check int) "dispatched first" 10 result;
      (* Tag 1 → second branch, value 7 → 7 + 100 = 107 *)
      let result2 = transform (`Array [ `Int 1; `Int 7 ]) in
      Alcotest.(check int) "dispatched second" 107 result2
  | _ -> Alcotest.fail "expected Basic"

(** Test: one_of dispatch bad format raises. *)
let test_one_of_dispatch_bad_format () =
  let gen =
    one_of
      [
        integers ~min_value:0 ~max_value:10 ();
        integers ~min_value:100 ~max_value:200 ();
      ]
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "bad format raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: optional creates one_of-based generator. *)
let test_optional_basic () =
  let gen = optional (integers ()) in
  Alcotest.(check bool) "is_basic" true (is_basic gen)

(** Test: ip_addresses IPv4. *)
let test_ip_v4_schema () =
  let gen = ip_addresses ~version:4 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "ipv4" typ
  | None -> Alcotest.fail "expected schema"

(** Test: ip_addresses IPv6. *)
let test_ip_v6_schema () =
  let gen = ip_addresses ~version:6 () in
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "ipv6" typ
  | None -> Alcotest.fail "expected schema"

(** Test: ip_addresses default (both) is a one_of. *)
let test_ip_both () =
  let gen = ip_addresses () in
  Alcotest.(check bool) "is_basic (one_of)" true (is_basic gen)

(** Test: ip_addresses invalid version raises. *)
let test_ip_invalid_version () =
  let raised = ref false in
  (try ignore (ip_addresses ~version:3 ()) with Failure _ -> raised := true);
  Alcotest.(check bool) "raised" true !raised

(** Test: tuples2 basic schema. *)
let test_tuples2_basic_schema () =
  let gen = tuples2 (integers ()) (booleans ()) in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "tuple" typ
  | None -> Alcotest.fail "expected schema"

(** Test: tuples2 basic transform. *)
let test_tuples2_basic_transform () =
  let gen = tuples2 (integers ()) (booleans ()) in
  match gen with
  | Basic { transform; _ } ->
      let a, b = transform (`Array [ `Int 5; `Bool true ]) in
      Alcotest.(check int) "first" 5 a;
      Alcotest.(check bool) "second" true b
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples2 basic transform bad format raises. *)
let test_tuples2_bad_format () =
  let gen = tuples2 (integers ()) (booleans ()) in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples2 non-basic is not basic. *)
let test_tuples2_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples2 filtered (booleans ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: tuples3 basic schema. *)
let test_tuples3_basic_schema () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Hegel.Cbor_helpers.extract_dict s in
      let typ =
        Hegel.Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
      in
      Alcotest.(check string) "type" "tuple" typ
  | None -> Alcotest.fail "expected schema"

(** Test: tuples3 basic transform. *)
let test_tuples3_basic_transform () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  match gen with
  | Basic { transform; _ } ->
      let a, b, c = transform (`Array [ `Int 1; `Bool false; `Int 3 ]) in
      Alcotest.(check int) "first" 1 a;
      Alcotest.(check bool) "second" false b;
      Alcotest.(check int) "third" 3 c
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples3 bad format raises. *)
let test_tuples3_bad_format () =
  let gen =
    tuples3 (integers ()) (booleans ()) (integers ~min_value:0 ~max_value:5 ())
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples3 non-basic is not basic. *)
let test_tuples3_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples3 filtered (booleans ()) (integers ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(** Test: tuples4 basic schema. *)
let test_tuples4_basic_schema () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  Alcotest.(check bool) "is_basic" true (is_basic gen)

(** Test: tuples4 basic transform. *)
let test_tuples4_basic_transform () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  match gen with
  | Basic { transform; _ } ->
      let a, b, c, d =
        transform (`Array [ `Int 1; `Bool true; `Int 3; `Float 3.14 ])
      in
      Alcotest.(check int) "first" 1 a;
      Alcotest.(check bool) "second" true b;
      Alcotest.(check int) "third" 3 c;
      Alcotest.(check (float 0.01)) "fourth" 3.14 d
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples4 bad format raises. *)
let test_tuples4_bad_format () =
  let gen =
    tuples4 (integers ()) (booleans ())
      (integers ~min_value:0 ~max_value:5 ())
      (floats ())
  in
  match gen with
  | Basic { transform; _ } ->
      let raised = ref false in
      (try ignore (transform (`Int 42)) with Failure _ -> raised := true);
      Alcotest.(check bool) "raised" true !raised
  | _ -> Alcotest.fail "expected Basic"

(** Test: tuples4 non-basic is not basic. *)
let test_tuples4_non_basic () =
  let filtered = filter (fun _ -> true) (integers ()) in
  let gen = tuples4 filtered (booleans ()) (integers ()) (floats ()) in
  Alcotest.(check bool) "not basic" false (is_basic gen)

(* ==== E2E tests for new generators ==== *)

(** Test: just always returns the constant. *)
let test_just_e2e () =
  Hegel.Session.run_hegel_test ~name:"just_e2e" ~test_cases:10 (fun () ->
      let v = generate (just 42) in
      Alcotest.(check int) "always 42" 42 v)

(** Test: from_regex generates matching strings. *)
let test_from_regex_e2e () =
  Hegel.Session.run_hegel_test ~name:"from_regex_e2e" ~test_cases:10 (fun () ->
      let v = generate (from_regex "[0-9]+" ()) in
      assert (String.length v > 0))

(** Test: emails generates strings containing at-sign. *)
let test_emails_e2e () =
  Hegel.Session.run_hegel_test ~name:"emails_e2e" ~test_cases:10 (fun () ->
      let v = generate (emails ()) in
      assert (String.contains v '@'))

(** Test: urls generates strings starting with http. *)
let test_urls_e2e () =
  Hegel.Session.run_hegel_test ~name:"urls_e2e" ~test_cases:10 (fun () ->
      let v = generate (urls ()) in
      assert (
        String.length v >= 7
        && (String.sub v 0 7 = "http://" || String.sub v 0 8 = "https://")))

(** Test: domains generates non-empty strings. *)
let test_domains_e2e () =
  Hegel.Session.run_hegel_test ~name:"domains_e2e" ~test_cases:10 (fun () ->
      let v = generate (domains ()) in
      assert (String.length v > 0))

(** Test: dates generates YYYY-MM-DD strings. *)
let test_dates_e2e () =
  Hegel.Session.run_hegel_test ~name:"dates_e2e" ~test_cases:10 (fun () ->
      let v = generate (dates ()) in
      assert (String.contains v '-'))

(** Test: times generates strings with colons. *)
let test_times_e2e () =
  Hegel.Session.run_hegel_test ~name:"times_e2e" ~test_cases:10 (fun () ->
      let v = generate (times ()) in
      assert (String.contains v ':'))

(** Test: datetimes generates strings with T. *)
let test_datetimes_e2e () =
  Hegel.Session.run_hegel_test ~name:"datetimes_e2e" ~test_cases:10 (fun () ->
      let v = generate (datetimes ()) in
      assert (String.contains v 'T'))

(** Test: one_of with basic generators works e2e. *)
let test_one_of_e2e () =
  Hegel.Session.run_hegel_test ~name:"one_of_e2e" ~test_cases:50 (fun () ->
      let gen = one_of [ integers ~min_value:0 ~max_value:10 (); just 99 ] in
      let v = generate gen in
      assert ((v >= 0 && v <= 10) || v = 99))

(** Test: one_of with non-basic generators works e2e. *)
let test_one_of_non_basic_e2e () =
  Hegel.Session.run_hegel_test ~name:"one_of_nb_e2e" ~test_cases:50 (fun () ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        one_of [ filtered; integers ~min_value:100 ~max_value:200 () ]
      in
      let v = generate gen in
      assert ((v > 5 && v <= 10) || (v >= 100 && v <= 200)))

(** Test: optional produces None or Some e2e. *)
let test_optional_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.Session.run_hegel_test ~name:"optional_e2e" ~test_cases:50 (fun () ->
      let gen = optional (integers ~min_value:1 ~max_value:100 ()) in
      match generate gen with
      | Some v ->
          saw_some := true;
          assert (v >= 1 && v <= 100)
      | None -> saw_none := true);
  (* At least one of each should have occurred in 50 test cases *)
  Alcotest.(check bool) "saw Some" true !saw_some;
  Alcotest.(check bool) "saw None" true !saw_none

(** Test: ip_addresses generates valid IPs e2e. *)
let test_ip_addresses_e2e () =
  Hegel.Session.run_hegel_test ~name:"ip_e2e" ~test_cases:20 (fun () ->
      let v4 = generate (ip_addresses ~version:4 ()) in
      assert (String.contains v4 '.');
      let v6 = generate (ip_addresses ~version:6 ()) in
      assert (String.contains v6 ':'))

(** Test: ip_addresses default generates either v4 or v6 e2e. *)
let test_ip_both_e2e () =
  Hegel.Session.run_hegel_test ~name:"ip_both_e2e" ~test_cases:20 (fun () ->
      let v = generate (ip_addresses ()) in
      assert (String.contains v '.' || String.contains v ':'))

(** Test: tuples2 basic e2e. *)
let test_tuples2_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples2_e2e" ~test_cases:20 (fun () ->
      let gen =
        tuples2 (integers ~min_value:0 ~max_value:10 ()) (booleans ())
      in
      let a, _b = generate gen in
      assert (a >= 0 && a <= 10))

(** Test: tuples2 composite e2e. *)
let test_tuples2_composite_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples2_comp_e2e" ~test_cases:20
    (fun () ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = tuples2 filtered (booleans ()) in
      let a, _b = generate gen in
      assert (a > 5 && a <= 10))

(** Test: tuples3 basic e2e. *)
let test_tuples3_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples3_e2e" ~test_cases:20 (fun () ->
      let gen =
        tuples3
          (integers ~min_value:0 ~max_value:10 ())
          (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
      in
      let a, _b, c = generate gen in
      assert (a >= 0 && a <= 10);
      assert (c >= 100 && c <= 200))

(** Test: tuples3 composite e2e. *)
let test_tuples3_composite_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples3_comp_e2e" ~test_cases:20
    (fun () ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        tuples3 filtered (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
      in
      let a, _b, c = generate gen in
      assert (a > 5 && a <= 10);
      assert (c >= 100 && c <= 200))

(** Test: tuples4 basic e2e. *)
let test_tuples4_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples4_e2e" ~test_cases:20 (fun () ->
      let gen =
        tuples4
          (integers ~min_value:0 ~max_value:10 ())
          (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
          (floats ~min_value:0.0 ~max_value:1.0 ())
      in
      let a, _b, c, d = generate gen in
      assert (a >= 0 && a <= 10);
      assert (c >= 100 && c <= 200);
      assert (d >= 0.0 && d <= 1.0))

(** Test: tuples4 composite e2e. *)
let test_tuples4_composite_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples4_comp_e2e" ~test_cases:20
    (fun () ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen =
        tuples4 filtered (booleans ())
          (integers ~min_value:100 ~max_value:200 ())
          (floats ~min_value:0.0 ~max_value:1.0 ())
      in
      let a, _b, c, d = generate gen in
      assert (a > 5 && a <= 10);
      assert (c >= 100 && c <= 200);
      assert (d >= 0.0 && d <= 1.0))

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
    Alcotest.test_case "lists basic non-array raises" `Quick
      test_lists_basic_non_array_raises;
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
    (* Unit tests for new generators *)
    Alcotest.test_case "just schema" `Quick test_just_schema;
    Alcotest.test_case "just transform" `Quick test_just_transform;
    Alcotest.test_case "from_regex schema" `Quick test_from_regex_schema;
    Alcotest.test_case "from_regex no fullmatch" `Quick
      test_from_regex_no_fullmatch;
    Alcotest.test_case "emails schema" `Quick test_emails_schema;
    Alcotest.test_case "urls schema" `Quick test_urls_schema;
    Alcotest.test_case "domains schema" `Quick test_domains_schema;
    Alcotest.test_case "domains max_length" `Quick test_domains_max_length;
    Alcotest.test_case "dates schema" `Quick test_dates_schema;
    Alcotest.test_case "times schema" `Quick test_times_schema;
    Alcotest.test_case "datetimes schema" `Quick test_datetimes_schema;
    Alcotest.test_case "one_of too few" `Quick test_one_of_too_few;
    Alcotest.test_case "one_of basic schema" `Quick test_one_of_basic_schema;
    Alcotest.test_case "one_of non-basic" `Quick test_one_of_non_basic;
    Alcotest.test_case "one_of dispatch" `Quick test_one_of_dispatch;
    Alcotest.test_case "one_of dispatch bad format" `Quick
      test_one_of_dispatch_bad_format;
    Alcotest.test_case "optional basic" `Quick test_optional_basic;
    Alcotest.test_case "ip_addresses v4" `Quick test_ip_v4_schema;
    Alcotest.test_case "ip_addresses v6" `Quick test_ip_v6_schema;
    Alcotest.test_case "ip_addresses both" `Quick test_ip_both;
    Alcotest.test_case "ip_addresses invalid" `Quick test_ip_invalid_version;
    Alcotest.test_case "tuples2 basic schema" `Quick test_tuples2_basic_schema;
    Alcotest.test_case "tuples2 basic transform" `Quick
      test_tuples2_basic_transform;
    Alcotest.test_case "tuples2 bad format" `Quick test_tuples2_bad_format;
    Alcotest.test_case "tuples2 non-basic" `Quick test_tuples2_non_basic;
    Alcotest.test_case "tuples3 basic schema" `Quick test_tuples3_basic_schema;
    Alcotest.test_case "tuples3 basic transform" `Quick
      test_tuples3_basic_transform;
    Alcotest.test_case "tuples3 bad format" `Quick test_tuples3_bad_format;
    Alcotest.test_case "tuples3 non-basic" `Quick test_tuples3_non_basic;
    Alcotest.test_case "tuples4 basic schema" `Quick test_tuples4_basic_schema;
    Alcotest.test_case "tuples4 basic transform" `Quick
      test_tuples4_basic_transform;
    Alcotest.test_case "tuples4 bad format" `Quick test_tuples4_bad_format;
    Alcotest.test_case "tuples4 non-basic" `Quick test_tuples4_non_basic;
    (* E2E tests for new generators *)
    Alcotest.test_case "just e2e" `Quick test_just_e2e;
    Alcotest.test_case "from_regex e2e" `Quick test_from_regex_e2e;
    Alcotest.test_case "emails e2e" `Quick test_emails_e2e;
    Alcotest.test_case "urls e2e" `Quick test_urls_e2e;
    Alcotest.test_case "domains e2e" `Quick test_domains_e2e;
    Alcotest.test_case "dates e2e" `Quick test_dates_e2e;
    Alcotest.test_case "times e2e" `Quick test_times_e2e;
    Alcotest.test_case "datetimes e2e" `Quick test_datetimes_e2e;
    Alcotest.test_case "one_of e2e" `Quick test_one_of_e2e;
    Alcotest.test_case "one_of non-basic e2e" `Quick test_one_of_non_basic_e2e;
    Alcotest.test_case "optional e2e" `Quick test_optional_e2e;
    Alcotest.test_case "ip_addresses e2e" `Quick test_ip_addresses_e2e;
    Alcotest.test_case "ip_addresses both e2e" `Quick test_ip_both_e2e;
    Alcotest.test_case "tuples2 e2e" `Quick test_tuples2_e2e;
    Alcotest.test_case "tuples2 composite e2e" `Quick test_tuples2_composite_e2e;
    Alcotest.test_case "tuples3 e2e" `Quick test_tuples3_e2e;
    Alcotest.test_case "tuples3 composite e2e" `Quick test_tuples3_composite_e2e;
    Alcotest.test_case "tuples4 e2e" `Quick test_tuples4_e2e;
    Alcotest.test_case "tuples4 composite e2e" `Quick test_tuples4_composite_e2e;
  ]
