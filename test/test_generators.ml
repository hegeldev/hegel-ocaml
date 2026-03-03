open Hegel.Connection
open Hegel.Generators

let contains_substring = Test_helpers.contains_substring

(* ==== Unit tests (no server needed) ==== *)

(** Test: draw outside test context raises. *)
let test_draw_outside_context () =
  let raised = ref false in
  (try ignore (draw (integers ()))
   with Failure msg ->
     raised := true;
     Alcotest.(check bool)
       "has 'outside of a Hegel test'" true
       (contains_substring msg "outside of a Hegel test"));
  Alcotest.(check bool) "raised" true !raised

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

let dummy_data () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Dummy" () in
  let data =
    Hegel.Client.
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
  data.Hegel.Client.test_aborted <- true;
  let raised = ref false in
  (try ignore (discardable_group Labels.flat_map data (fun () -> raise Exit))
   with Exit -> raised := true);
  Alcotest.(check bool) "raised Exit" true !raised;
  close conn;
  Unix.close s2

(** Test: collection_more raises Data_exhausted on StopTest (socketpair). *)
let test_collection_more_stoptest () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Client" () in
  let peer_conn = create_connection s2 ~name:"Peer" () in
  let t_hs =
    Thread.create
      (fun () ->
        Test_helpers.raw_handshake_responder peer_conn.socket;
        peer_conn.connection_state <- Client)
      ()
  in
  ignore (send_handshake conn);
  Thread.join t_hs;
  let ch = new_channel conn ~role:"Data" () in
  let data =
    Hegel.Client.{ channel = ch; is_final = false; test_aborted = false }
  in
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
   with Hegel.Client.Data_exhausted -> raised := true);
  Thread.join t_peer;
  Alcotest.(check bool) "raised Data_exhausted" true !raised;
  Alcotest.(check bool) "test_aborted" true data.test_aborted;
  close conn;
  close peer_conn

(** Test: collection_reject sends command when not finished (socketpair). *)
let test_collection_reject_live () =
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = create_connection s1 ~name:"Client" () in
  let peer_conn = create_connection s2 ~name:"Peer" () in
  let t_hs =
    Thread.create
      (fun () ->
        Test_helpers.raw_handshake_responder peer_conn.socket;
        peer_conn.connection_state <- Client)
      ()
  in
  ignore (send_handshake conn);
  Thread.join t_hs;
  let ch = new_channel conn ~role:"Data" () in
  let data =
    Hegel.Client.{ channel = ch; is_final = false; test_aborted = false }
  in
  let coll = new_collection ~min_size:0 data () in
  coll.server_name <- Some (`Text "test_coll");
  let peer_ch = connect_channel peer_conn (channel_id ch) ~role:"Peer" () in
  let received_cmd = ref "" in
  let t_peer =
    Thread.create
      (fun () ->
        let msg_id, msg = receive_request peer_ch () in
        let pairs = Hegel.Cbor_helpers.extract_dict msg in
        received_cmd :=
          Hegel.Cbor_helpers.extract_string (List.assoc (`Text "command") pairs);
        send_response_value peer_ch msg_id `Null)
      ()
  in
  collection_reject coll data;
  Thread.join t_peer;
  Alcotest.(check string) "command" "collection_reject" !received_cmd;
  close conn;
  close peer_conn

(* ==== E2E tests (real hegel binary) ==== *)

(** Test: integers(0, 100) generates values in range. *)
let test_integers_in_range () =
  Hegel.Session.run_hegel_test ~name:"int_range" ~test_cases:10 (fun () ->
      let gen = integers ~min_value:0 ~max_value:100 () in
      let v = Hegel.draw gen in
      assert (v >= 0 && v <= 100))

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Hegel.Session.run_hegel_test ~name:"map_double" ~test_cases:10 (fun () ->
      let gen = integers ~min_value:1 ~max_value:5 () |> map (fun v -> v * 2) in
      Alcotest.(check bool) "still basic" true (is_basic gen);
      let v = Hegel.draw gen in
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
      let v = Hegel.draw gen in
      assert (List.mem v [ 3; 5; 7; 9; 11 ]))

(** Test: map on non-basic (Mapped branch of do_draw). *)
let test_map_on_filtered_e2e () =
  Hegel.Session.run_hegel_test ~name:"map_on_filter" ~test_cases:10 (fun () ->
      let gen =
        filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
        |> map (fun v -> v * 2)
      in
      let v = Hegel.draw gen in
      assert (v > 10 && v <= 20))

(** Test: flat_map through server. *)
let test_flat_map_e2e () =
  Hegel.Session.run_hegel_test ~name:"flatmap_e2e" ~test_cases:10 (fun () ->
      let gen =
        flat_map
          (fun n -> integers ~min_value:0 ~max_value:(max 1 n) ())
          (integers ~min_value:1 ~max_value:5 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.draw gen in
      assert (v >= 0))

(** Test: filter through server. *)
let test_filter_e2e () =
  Hegel.Session.run_hegel_test ~name:"filter_e2e" ~test_cases:10 (fun () ->
      let gen =
        filter (fun v -> v mod 2 = 0) (integers ~min_value:0 ~max_value:100 ())
      in
      Alcotest.(check bool) "not basic" false (is_basic gen);
      let v = Hegel.draw gen in
      assert (v mod 2 = 0))

(** Test: filter exhaustion through server (always false → assume false). *)
let test_filter_exhaustion_e2e () =
  Hegel.Session.run_hegel_test ~name:"filter_exhaust_e2e" ~test_cases:10
    (fun () ->
      let gen =
        filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ())
      in
      ignore (Hegel.draw gen))

(** Test: group helper through server. *)
let test_group_e2e () =
  Hegel.Session.run_hegel_test ~name:"group_e2e" ~test_cases:5 (fun () ->
      let data = Hegel.Client.get_data () in
      let v =
        group Labels.list data (fun () ->
            Hegel.Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ])
              data)
      in
      let n = Hegel.Cbor_helpers.extract_int v in
      assert (n >= 0 && n <= 10))

(** Test: discardable_group through server — success path. *)
let test_discardable_group_e2e () =
  Hegel.Session.run_hegel_test ~name:"disc_group_e2e" ~test_cases:5 (fun () ->
      let data = Hegel.Client.get_data () in
      let v =
        discardable_group Labels.tuple data (fun () ->
            Hegel.Client.generate_from_schema
              (`Map
                 [
                   (`Text "type", `Text "integer");
                   (`Text "min_value", `Int 0);
                   (`Text "max_value", `Int 10);
                 ])
              data)
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

(* ==== E2E tests for lists ==== *)

(** Test: lists(integers) generates a list where all elements are in range. *)
let test_lists_of_integers_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_ints_e2e" ~test_cases:50 (fun () ->
      let gen =
        lists (integers ~min_value:0 ~max_value:100 ()) ~max_size:3 ()
      in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let items = Hegel.draw gen in
      Alcotest.(check bool) "max 3" true (List.length items <= 3);
      List.iter (fun n -> assert (n >= 0 && n <= 100)) items)

(** Test: lists(booleans, min_size=3, max_size=5) → length in [3,5]. *)
let test_lists_booleans_bounds_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_bools_bounds_e2e" ~test_cases:50
    (fun () ->
      let gen = lists (booleans ()) ~min_size:3 ~max_size:5 () in
      Alcotest.(check bool) "is_basic" true (is_basic gen);
      let items = Hegel.draw gen in
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
      let items = Hegel.draw gen in
      let n = List.length items in
      assert (n >= 1 && n <= 3);
      List.iter (fun x -> assert (x > 5)) items)

(** Test: lists(non-basic) without max_size (max_size=None in collection). *)
let test_lists_non_basic_no_max_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nb_nomax" ~test_cases:10 (fun () ->
      let elem =
        filter (fun _ -> true) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = lists elem () in
      let items = Hegel.draw gen in
      List.iter (fun x -> assert (x >= 0 && x <= 10)) items)

(** Test: lists(lists(booleans)) → nested lists work. *)
let test_lists_nested_e2e () =
  Hegel.Session.run_hegel_test ~name:"lists_nested_e2e" ~test_cases:50
    (fun () ->
      let inner = lists (booleans ()) ~max_size:3 () in
      let gen = lists inner ~max_size:3 () in
      Alcotest.(check bool) "outer is_basic" true (is_basic gen);
      let outer_items = Hegel.draw gen in
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
      let v = Hegel.draw (just 42) in
      Alcotest.(check int) "always 42" 42 v)

(** Test: from_regex generates matching strings. *)
let test_from_regex_e2e () =
  Hegel.Session.run_hegel_test ~name:"from_regex_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (from_regex "[0-9]+" ()) in
      assert (String.length v > 0))

(** Test: emails generates strings containing at-sign. *)
let test_emails_e2e () =
  Hegel.Session.run_hegel_test ~name:"emails_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (emails ()) in
      assert (String.contains v '@'))

(** Test: urls generates strings starting with http. *)
let test_urls_e2e () =
  Hegel.Session.run_hegel_test ~name:"urls_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (urls ()) in
      assert (
        String.length v >= 7
        && (String.sub v 0 7 = "http://" || String.sub v 0 8 = "https://")))

(** Test: domains generates non-empty strings. *)
let test_domains_e2e () =
  Hegel.Session.run_hegel_test ~name:"domains_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (domains ()) in
      assert (String.length v > 0))

(** Test: dates generates YYYY-MM-DD strings. *)
let test_dates_e2e () =
  Hegel.Session.run_hegel_test ~name:"dates_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (dates ()) in
      assert (String.contains v '-'))

(** Test: times generates strings with colons. *)
let test_times_e2e () =
  Hegel.Session.run_hegel_test ~name:"times_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (times ()) in
      assert (String.contains v ':'))

(** Test: datetimes generates strings with T. *)
let test_datetimes_e2e () =
  Hegel.Session.run_hegel_test ~name:"datetimes_e2e" ~test_cases:10 (fun () ->
      let v = Hegel.draw (datetimes ()) in
      assert (String.contains v 'T'))

(** Test: one_of with basic generators works e2e. *)
let test_one_of_e2e () =
  Hegel.Session.run_hegel_test ~name:"one_of_e2e" ~test_cases:50 (fun () ->
      let gen = one_of [ integers ~min_value:0 ~max_value:10 (); just 99 ] in
      let v = Hegel.draw gen in
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
      let v = Hegel.draw gen in
      assert ((v > 5 && v <= 10) || (v >= 100 && v <= 200)))

(** Test: optional produces None or Some e2e. *)
let test_optional_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.Session.run_hegel_test ~name:"optional_e2e" ~test_cases:50 (fun () ->
      let gen = optional (integers ~min_value:1 ~max_value:100 ()) in
      match Hegel.draw gen with
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
      let v4 = Hegel.draw (ip_addresses ~version:4 ()) in
      assert (String.contains v4 '.');
      let v6 = Hegel.draw (ip_addresses ~version:6 ()) in
      assert (String.contains v6 ':'))

(** Test: ip_addresses default generates either v4 or v6 e2e. *)
let test_ip_both_e2e () =
  Hegel.Session.run_hegel_test ~name:"ip_both_e2e" ~test_cases:20 (fun () ->
      let v = Hegel.draw (ip_addresses ()) in
      assert (String.contains v '.' || String.contains v ':'))

(** Test: tuples2 basic e2e. *)
let test_tuples2_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples2_e2e" ~test_cases:20 (fun () ->
      let gen =
        tuples2 (integers ~min_value:0 ~max_value:10 ()) (booleans ())
      in
      let a, _b = Hegel.draw gen in
      assert (a >= 0 && a <= 10))

(** Test: tuples2 composite e2e. *)
let test_tuples2_composite_e2e () =
  Hegel.Session.run_hegel_test ~name:"tuples2_comp_e2e" ~test_cases:20
    (fun () ->
      let filtered =
        filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ())
      in
      let gen = tuples2 filtered (booleans ()) in
      let a, _b = Hegel.draw gen in
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
      let a, _b, c = Hegel.draw gen in
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
      let a, _b, c = Hegel.draw gen in
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
      let a, _b, c, d = Hegel.draw gen in
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
      let a, _b, c, d = Hegel.draw gen in
      assert (a > 5 && a <= 10);
      assert (c >= 100 && c <= 200);
      assert (d >= 0.0 && d <= 1.0))

let tests =
  [
    (* Unit tests *)
    Alcotest.test_case "draw outside context" `Quick test_draw_outside_context;
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
    (* E2E tests *)
    Alcotest.test_case "integers in range" `Quick test_integers_in_range;
    Alcotest.test_case "map doubles e2e" `Quick test_map_doubles_e2e;
    Alcotest.test_case "double map e2e" `Quick test_double_map_e2e;
    Alcotest.test_case "map on filtered e2e" `Quick test_map_on_filtered_e2e;
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
    Alcotest.test_case "lists non-basic no max e2e" `Quick
      test_lists_non_basic_no_max_e2e;
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
