open Hegel
open Connection
open Generators

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
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
      Alcotest.(check string) "type" "integer" typ;
      let min_v =
        Cbor_helpers.extract_int (List.assoc (`Text "min_value") pairs)
      in
      Alcotest.(check int) "min_value" 0 min_v;
      let max_v =
        Cbor_helpers.extract_int (List.assoc (`Text "max_value") pairs)
      in
      Alcotest.(check int) "max_value" 10 max_v
  | None -> Alcotest.fail "expected Some schema"

let test_basic_generator_no_bounds () =
  let gen = integers () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      Alcotest.(check int) "pairs count" 1 (List.length pairs)
  | None -> Alcotest.fail "expected schema"

let test_map_on_basic_preserves_schema () =
  let gen = integers ~min_value:0 ~max_value:10 () in
  let mapped = map (fun v -> v * 2) gen in
  Alcotest.(check bool) "still basic" true (is_basic mapped);
  match schema mapped with
  | Some s ->
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
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
      let pairs = Cbor_helpers.extract_dict s in
      let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
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

let dummy_data () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Dummy" () in
  let data =
    Client.
      { channel = control_channel conn; is_final = false; test_aborted = false }
  in
  (data, conn, s2)

let test_collection_new () =
  let data, conn, s2 = dummy_data () in
  let coll = new_collection ~min_size:0 ~max_size:5 data () in
  Alcotest.(check bool) "not finished" false coll.finished;
  close conn;
  Unix.close s2

let test_collection_new_no_max () =
  let data, conn, s2 = dummy_data () in
  let coll = new_collection ~min_size:0 data () in
  Alcotest.(check bool) "not finished" false coll.finished;
  Alcotest.(check bool) "max_size is None" true (coll.max_size = None);
  close conn;
  Unix.close s2

let test_collection_reject_when_finished () =
  let data, conn, s2 = dummy_data () in
  let coll = new_collection ~min_size:0 data () in
  coll.finished <- true;
  (* Should be a no-op, not send any commands *)
  collection_reject coll data;
  close conn;
  Unix.close s2

(** Test: collection_more returns false when already finished. *)
let test_collection_more_when_finished () =
  let data, conn, s2 = dummy_data () in
  let coll = new_collection ~min_size:0 data () in
  coll.finished <- true;
  let result = collection_more coll data in
  Alcotest.(check bool) "returns false" false result;
  close conn;
  Unix.close s2

(** Test: discardable_group exception path — stop_span with discard. *)
let test_discardable_group_exception () =
  let data, conn, s2 = dummy_data () in
  data.Client.test_aborted <- true;
  let raised = ref false in
  (try ignore (discardable_group Labels.flat_map data (fun () -> raise Exit))
   with Exit -> raised := true);
  Alcotest.(check bool) "raised Exit" true !raised;
  close conn;
  Unix.close s2

(** Test: collection_more raises Data_exhausted on StopTest (socketpair). *)
let test_collection_more_stoptest () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Client" () in
  let peer_conn = Test_helpers.make_connection s2 ~name:"Peer" () in
  let t_hs = Thread.create Test_helpers.handshake_via_channel peer_conn in
  ignore (send_handshake conn);
  Thread.join t_hs;
  let ch = new_channel conn ~role:"Data" () in
  let data = Client.{ channel = ch; is_final = false; test_aborted = false } in
  let coll = new_collection ~min_size:0 data () in
  let peer_ch = connect_channel peer_conn (channel_id ch) ~role:"Peer" () in
  let t_peer =
    Thread.create
      (fun () ->
        let msg_id, _msg = receive_request peer_ch () in
        send_response_raw peer_ch msg_id
          (CBOR.Simple.encode
             (`Map
                [
                  (`Text "error", `Text "Test case is being abandoned");
                  (`Text "type", `Text "StopTest");
                ])))
      ()
  in
  let raised = ref false in
  (try ignore (collection_more coll data)
   with Client.Data_exhausted -> raised := true);
  Thread.join t_peer;
  Alcotest.(check bool) "raised Data_exhausted" true !raised;
  Alcotest.(check bool) "test_aborted" true data.test_aborted;
  close conn;
  close peer_conn

(** Test: collection_reject sends command when not finished (socketpair). *)
let test_collection_reject_live () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Test_helpers.make_connection s1 ~name:"Client" () in
  let peer_conn = Test_helpers.make_connection s2 ~name:"Peer" () in
  let t_hs = Thread.create Test_helpers.handshake_via_channel peer_conn in
  ignore (send_handshake conn);
  Thread.join t_hs;
  let ch = new_channel conn ~role:"Data" () in
  let data = Client.{ channel = ch; is_final = false; test_aborted = false } in
  let coll = new_collection ~min_size:0 data () in
  coll.server_name <- Some (`Text "test_coll");
  let peer_ch = connect_channel peer_conn (channel_id ch) ~role:"Peer" () in
  let received_cmd = ref "" in
  let t_peer =
    Thread.create
      (fun () ->
        let msg_id, msg = receive_request peer_ch () in
        let pairs = Cbor_helpers.extract_dict msg in
        received_cmd :=
          Cbor_helpers.extract_string (List.assoc (`Text "command") pairs);
        send_response_value peer_ch msg_id `Null)
      ()
  in
  collection_reject coll data;
  Thread.join t_peer;
  Alcotest.(check string) "command" "collection_reject" !received_cmd;
  close conn;
  close peer_conn

(* ==== E2E tests ==== *)

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen = integers ~min_value:1 ~max_value:5 () |> map (fun v -> v * 2) in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let v = Hegel.draw tc gen in
      assert (v >= 2 && v <= 10);
      assert (v mod 2 = 0))

(** Test: double map composes correctly. *)
let test_double_map_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen =
        integers ~min_value:1 ~max_value:5 ()
        |> map (fun v -> v * 2)
        |> map (fun v -> v + 1)
      in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let s = schema gen in
      (match s with
      | Some schema_v ->
          let pairs = Cbor_helpers.extract_dict schema_v in
          let typ =
            Cbor_helpers.extract_string (List.assoc (`Text "type") pairs)
          in
          Alcotest.(check string) "schema type" "integer" typ
      | None -> Alcotest.fail "expected schema");
      let v = Hegel.draw tc gen in
      assert (List.mem v [ 3; 5; 7; 9; 11 ]))

(** Test: map on non-basic (Mapped branch of do_draw). *)
let test_map_on_filtered_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen =
        filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
        |> map (fun v -> v * 2)
      in
      let v = Hegel.draw tc gen in
      assert (v > 10 && v <= 20))

(** Test: flat_map through server. *)
let test_flat_map_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen =
        flat_map
          (fun n -> integers ~min_value:0 ~max_value:(max 1 n) ())
          (integers ~min_value:1 ~max_value:5 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.draw tc gen in
      assert (v >= 0))

(** Test: filter through server. *)
let test_filter_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen =
        filter (fun v -> v mod 2 = 0) (integers ~min_value:0 ~max_value:100 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.draw tc gen in
      assert (v mod 2 = 0))

(** Test: filter exhaustion through server (always false → assume false). *)
let test_filter_exhaustion_e2e () =
  Session.run_hegel_test ~test_cases:10 (fun tc ->
      let gen =
        filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ())
      in
      ignore (Hegel.draw tc gen))

(** Test: group helper through server. *)
let test_group_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun tc ->
      let v =
        group Labels.list tc (fun () ->
            Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ])
              tc)
      in
      let n = Cbor_helpers.extract_int v in
      assert (n >= 0 && n <= 10))

(** Test: discardable_group through server — success path. *)
let test_discardable_group_e2e () =
  Session.run_hegel_test ~test_cases:5 (fun tc ->
      let v =
        discardable_group Labels.tuple tc (fun () ->
            Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ])
              tc)
      in
      let n = Cbor_helpers.extract_int v in
      assert (n >= 0 && n <= 10))

let tests =
  [
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
    Alcotest.test_case "collection_more when finished" `Quick
      test_collection_more_when_finished;
    Alcotest.test_case "discardable_group exception" `Quick
      test_discardable_group_exception;
    Alcotest.test_case "collection_more StopTest" `Quick
      test_collection_more_stoptest;
    Alcotest.test_case "collection_reject live" `Quick
      test_collection_reject_live;
    Alcotest.test_case "map doubles e2e" `Quick test_map_doubles_e2e;
    Alcotest.test_case "double map e2e" `Quick test_double_map_e2e;
    Alcotest.test_case "map on filtered e2e" `Quick test_map_on_filtered_e2e;
    Alcotest.test_case "flat_map e2e" `Quick test_flat_map_e2e;
    Alcotest.test_case "filter e2e" `Quick test_filter_e2e;
    Alcotest.test_case "filter exhaustion e2e" `Quick test_filter_exhaustion_e2e;
    Alcotest.test_case "group e2e" `Quick test_group_e2e;
    Alcotest.test_case "discardable_group e2e" `Quick test_discardable_group_e2e;
  ]
