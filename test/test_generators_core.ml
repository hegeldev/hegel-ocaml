open Hegel
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
;;

let test_basic_generator_schema () =
  let gen = integers ~min_value:0 ~max_value:10 () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "integer" typ;
    let min_v = Cbor_helpers.extract_int (List.assoc (`Text "min_value") pairs) in
    Alcotest.(check int) "min_value" 0 min_v;
    let max_v = Cbor_helpers.extract_int (List.assoc (`Text "max_value") pairs) in
    Alcotest.(check int) "max_value" 10 max_v
  | None -> Alcotest.fail "expected Some schema"
;;

let test_basic_generator_no_bounds () =
  let gen = integers () in
  Alcotest.(check bool) "is_basic" true (is_basic gen);
  match schema gen with
  | Some s ->
    let pairs = Cbor_helpers.extract_dict s in
    Alcotest.(check int) "pairs count" 1 (List.length pairs)
  | None -> Alcotest.fail "expected schema"
;;

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
;;

let test_double_map_on_basic () =
  let gen = integers () in
  let m1 = map (fun _ -> 2) gen in
  let m2 = map (fun _ -> 3) m1 in
  Alcotest.(check bool) "double map still basic" true (is_basic m2);
  (* Verify schema is preserved through both maps *)
  match schema gen, schema m2 with
  | Some s1, Some s2 ->
    Alcotest.(check bool) "schema unchanged" true (Cbor.encode s1 = Cbor.encode s2)
  | _ -> Alcotest.fail "expected both schemas"
;;

let test_map_on_non_basic_creates_mapped () =
  let gen = integers () in
  let filtered = filter (fun _ -> true) gen in
  let mapped = map (fun x -> x) filtered in
  Alcotest.(check bool) "not basic" false (is_basic mapped)
;;

let test_flat_map_not_basic () =
  let gen = integers () in
  let fm = flat_map (fun _ -> integers ()) gen in
  Alcotest.(check bool) "not basic" false (is_basic fm)
;;

let test_filter_not_basic () =
  let gen = integers () in
  let f = filter (fun _ -> true) gen in
  Alcotest.(check bool) "not basic" false (is_basic f)
;;

let test_schema_on_non_basic () =
  let gen = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "no schema" true (schema gen = None)
;;

let test_as_basic_on_basic () =
  let gen = integers ~min_value:1 ~max_value:5 () in
  match as_basic gen with
  | Some (s, _transform) ->
    let pairs = Cbor_helpers.extract_dict s in
    let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
    Alcotest.(check string) "type" "integer" typ
  | None -> Alcotest.fail "expected Some"
;;

let test_as_basic_on_basic_with_transform () =
  let gen = map (fun x -> x) (integers ()) in
  match as_basic gen with
  | Some (_, _transform) -> ()
  | None -> Alcotest.fail "expected Some"
;;

let test_as_basic_on_non_basic () =
  let gen = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "as_basic is None" true (as_basic gen = None)
;;

let test_basic_unique_safe_on_leaf () =
  Alcotest.(check bool)
    "leaf integers is unique-safe"
    true
    (basic_unique_safe (integers ()))
;;

let test_basic_unique_safe_on_mapped_basic () =
  (* [map] over [Basic] still returns [Basic], but the user's function may
     collapse distinct inputs, so the flag is cleared. *)
  Alcotest.(check bool)
    "mapped basic is not unique-safe"
    false
    (basic_unique_safe (map (fun x -> x) (integers ())))
;;

let test_basic_unique_safe_on_non_basic () =
  let gen = filter (fun _ -> true) (integers ()) in
  Alcotest.(check bool) "non-basic is not unique-safe" false (basic_unique_safe gen)
;;

let test_max_filter_attempts () =
  Alcotest.(check int) "max attempts" 3 max_filter_attempts
;;

(* [with_tc f] runs [f] with a real per-test-case handle from the native
   engine. Used by the collection-record tests, which exercise the OCaml-side
   collection bookkeeping. *)
let with_tc f = Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:1 ()) f

let test_collection_new () =
  with_tc (fun data ->
    let coll = new_collection ~min_size:0 ~max_size:5 data () in
    Alcotest.(check bool) "not finished" false coll.finished)
;;

let test_collection_new_no_max () =
  with_tc (fun data ->
    let coll = new_collection ~min_size:0 data () in
    Alcotest.(check bool) "not finished" false coll.finished;
    Alcotest.(check bool) "max_size is None" true (coll.max_size = None))
;;

let test_collection_reject_when_finished () =
  with_tc (fun data ->
    let coll = new_collection ~min_size:0 data () in
    coll.finished <- true;
    (* Should be a no-op, not touch the engine. *)
    collection_reject coll data)
;;

(** Test: collection_more returns false when already finished. *)
let test_collection_more_when_finished () =
  with_tc (fun data ->
    let coll = new_collection ~min_size:0 data () in
    coll.finished <- true;
    let result = collection_more coll data in
    Alcotest.(check bool) "returns false" false result)
;;

(** Test: discardable_group exception path — stop_span skipped when aborted. *)
let test_discardable_group_exception () =
  with_tc (fun data ->
    data.Client.test_aborted <- true;
    let raised = ref false in
    (try ignore (discardable_group Labels.flat_map data (fun () -> raise Exit) : _) with
     | Exit -> raised := true);
    Alcotest.(check bool) "raised Exit" true !raised)
;;

(* ==== E2E tests ==== *)

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
    let gen = integers ~min_value:1 ~max_value:5 () |> map (fun v -> v * 2) in
    Alcotest.(check bool) "still basic" true (is_basic gen);
    let v = Hegel.draw_silent tc gen in
    assert (v >= 2 && v <= 10);
    assert (v mod 2 = 0))
;;

(** Test: double map composes correctly. *)
let test_double_map_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
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
       let typ = Cbor_helpers.extract_string (List.assoc (`Text "type") pairs) in
       Alcotest.(check string) "schema type" "integer" typ
     | None -> Alcotest.fail "expected schema");
    let v = Hegel.draw_silent tc gen in
    assert (List.mem v [ 3; 5; 7; 9; 11 ]))
;;

(** Test: map on non-basic (Mapped branch of do_draw). *)
let test_map_on_filtered_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
    let gen =
      filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
      |> map (fun v -> v * 2)
    in
    let v = Hegel.draw_silent tc gen in
    assert (v > 10 && v <= 20))
;;

(** Test: flat_map through server. *)
let test_flat_map_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
    let gen =
      flat_map
        (fun n -> integers ~min_value:0 ~max_value:(max 1 n) ())
        (integers ~min_value:1 ~max_value:5 ())
    in
    Alcotest.(check bool) "not basic" false (is_basic gen);
    let v = Hegel.draw_silent tc gen in
    assert (v >= 0))
;;

(** Test: filter through server. *)
let test_filter_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:10 ()) (fun tc ->
    let gen = filter (fun v -> v mod 2 = 0) (integers ~min_value:0 ~max_value:100 ()) in
    Alcotest.(check bool) "not basic" false (is_basic gen);
    let v = Hegel.draw tc gen in
    assert (v mod 2 = 0))
;;

(** Test: filter exhaustion through server (always false → assume false). *)
let test_filter_exhaustion_e2e () =
  Hegel.run_hegel_test
    ~settings:
      (Client.settings ~test_cases:10 ()
       |> Hegel.Client.with_suppress_health_check [ Hegel.Client.Filter_too_much ])
    (fun tc ->
       let gen = filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ()) in
       ignore (Hegel.draw tc gen))
;;

(** Test: group helper through server. *)
let test_group_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
    let v =
      group Labels.list tc (fun () ->
        Client.generate_from_schema
          (`Map
              [ `Text "type", `Text "integer"
              ; `Text "min_value", `Int 0
              ; `Text "max_value", `Int 10
              ])
          tc)
    in
    let n = Cbor_helpers.extract_int v in
    assert (n >= 0 && n <= 10))
;;

(** Test: discardable_group through server — success path. *)
let test_discardable_group_e2e () =
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:5 ()) (fun tc ->
    let v =
      discardable_group Labels.tuple tc (fun () ->
        Client.generate_from_schema
          (`Map
              [ `Text "type", `Text "integer"
              ; `Text "min_value", `Int 0
              ; `Text "max_value", `Int 10
              ])
          tc)
    in
    let n = Cbor_helpers.extract_int v in
    assert (n >= 0 && n <= 10))
;;

(** [printer gen] renders [value] to [expected]. ([gen] is printable, so its
    printer is total — no [option].) *)
let check_printer name gen value expected =
  Alcotest.(check string) name expected (Core.Sexp.to_string (printer gen value))
;;

let test_printer_int () = check_printer "int" (integers ()) 42 "42"
let test_printer_bool () = check_printer "bool" (booleans ()) true "true"
let test_printer_text () = check_printer "text" (text ()) "hi" "hi"

(* [filter] is type-preserving, so it delegates to the source's printer. *)
let test_printer_filter_delegates () =
  check_printer "filter" (filter (fun _ -> true) (integers ())) 5 "5"
;;

(* [with_printer] upgrades an unprintable generator (here [map] over a [Basic])
   to printable using the supplied printer. *)
let test_with_printer () =
  check_printer
    "with_printer"
    (with_printer Core.Int.sexp_of_t (map (fun v -> v * 2) (integers ())))
    21
    "21"
;;

(* [filter] over an unprintable generator stays unprintable; it can still be
   drawn via [draw_silent] and reports a non-basic core. *)
let test_filter_on_unprintable () =
  let gen = filter (fun _ -> true) (sampled_from [ 1; 2; 3 ]) in
  Alcotest.(check bool) "not basic" false (is_basic gen)
;;

(* Lists render via both the server-side path (basic elements) and the
   collection path (non-basic but printable elements). *)
let test_printer_list_basic () =
  check_printer "list" (lists (integers ()) ()) [ 1; 2; 3 ] "(1 2 3)"
;;

let test_printer_list_composite () =
  check_printer
    "list composite"
    (lists (filter (fun _ -> true) (integers ())) ())
    [ 1; 2 ]
    "(1 2)"
;;

let test_printer_list_unique_composite () =
  check_printer
    "list unique"
    (lists (filter (fun _ -> true) (integers ())) ~unique:true ())
    [ 1; 2 ]
    "(1 2)"
;;

(* Tuples render via both the all-basic (single schema) and composite paths. *)
let test_printer_tuple2 () =
  check_printer "tuple2" (tuples2 (integers ()) (integers ())) (1, 2) "(1 2)"
;;

let test_printer_tuple2_composite () =
  check_printer
    "tuple2 composite"
    (tuples2 (filter (fun _ -> true) (integers ())) (integers ()))
    (1, 2)
    "(1 2)"
;;

let test_printer_tuple3 () =
  check_printer
    "tuple3"
    (tuples3 (integers ()) (integers ()) (integers ()))
    (1, 2, 3)
    "(1 2 3)"
;;

let test_printer_tuple4 () =
  check_printer
    "tuple4"
    (tuples4 (integers ()) (integers ()) (integers ()) (integers ()))
    (1, 2, 3, 4)
    "(1 2 3 4)"
;;

(* one_of branches share a type, so any branch's printer renders the result. *)
let test_printer_one_of_basic () =
  check_printer
    "one_of basic"
    (one_of
       [ integers ~min_value:0 ~max_value:5 (); integers ~min_value:6 ~max_value:9 () ])
    3
    "3"
;;

let test_printer_one_of_composite () =
  check_printer
    "one_of composite"
    (one_of [ filter (fun _ -> true) (integers ()); integers () ])
    3
    "3"
;;

(* Hashmaps render via both the dict-schema (basic) and collection paths. *)
let test_printer_hashmap_basic () =
  check_printer
    "hashmap"
    (hashmaps (integers ()) (integers ()) ())
    [ 1, 2; 3, 4 ]
    "((1 2)(3 4))"
;;

let test_printer_hashmap_composite () =
  check_printer
    "hashmap composite"
    (hashmaps (filter (fun _ -> true) (integers ())) (integers ()) ())
    [ 1, 2 ]
    "((1 2))"
;;

(* optional composes an ['a option] printer from the element's, rendering
   [None] / [(Some v)] via [Option.sexp_of_t]. The element being non-basic
   (here filtered) exercises optional's composite [one_of] path. *)
(* [Option.sexp_of_t]'s round-trippable form: [(v)] for [Some v], [()] for
   [None]. *)
let test_printer_optional_some () =
  check_printer "optional some" (optional (integers ())) (Some 5) "(5)"
;;

let test_printer_optional_none () =
  check_printer "optional none" (optional (integers ())) None "()"
;;

let test_printer_optional_composite () =
  check_printer
    "optional composite"
    (optional (filter (fun _ -> true) (integers ())))
    (Some 7)
    "(7)"
;;

let tests =
  [ Alcotest.test_case "span label constants" `Quick test_span_label_constants
  ; Alcotest.test_case "basic generator schema" `Quick test_basic_generator_schema
  ; Alcotest.test_case "basic generator no bounds" `Quick test_basic_generator_no_bounds
  ; Alcotest.test_case
      "map on basic preserves schema"
      `Quick
      test_map_on_basic_preserves_schema
  ; Alcotest.test_case "double map on basic" `Quick test_double_map_on_basic
  ; Alcotest.test_case
      "map on non-basic creates mapped"
      `Quick
      test_map_on_non_basic_creates_mapped
  ; Alcotest.test_case "flat_map not basic" `Quick test_flat_map_not_basic
  ; Alcotest.test_case "filter not basic" `Quick test_filter_not_basic
  ; Alcotest.test_case "schema on non-basic" `Quick test_schema_on_non_basic
  ; Alcotest.test_case "as_basic on basic" `Quick test_as_basic_on_basic
  ; Alcotest.test_case
      "as_basic on basic with transform"
      `Quick
      test_as_basic_on_basic_with_transform
  ; Alcotest.test_case "as_basic on non-basic" `Quick test_as_basic_on_non_basic
  ; Alcotest.test_case "basic_unique_safe on leaf" `Quick test_basic_unique_safe_on_leaf
  ; Alcotest.test_case
      "basic_unique_safe on mapped basic"
      `Quick
      test_basic_unique_safe_on_mapped_basic
  ; Alcotest.test_case
      "basic_unique_safe on non-basic"
      `Quick
      test_basic_unique_safe_on_non_basic
  ; Alcotest.test_case "max_filter_attempts" `Quick test_max_filter_attempts
  ; Alcotest.test_case "collection new" `Quick test_collection_new
  ; Alcotest.test_case "collection new no max" `Quick test_collection_new_no_max
  ; Alcotest.test_case
      "collection reject when finished"
      `Quick
      test_collection_reject_when_finished
  ; Alcotest.test_case
      "collection_more when finished"
      `Quick
      test_collection_more_when_finished
  ; Alcotest.test_case
      "discardable_group exception"
      `Quick
      test_discardable_group_exception
  ; Alcotest.test_case "map doubles e2e" `Quick test_map_doubles_e2e
  ; Alcotest.test_case "double map e2e" `Quick test_double_map_e2e
  ; Alcotest.test_case "map on filtered e2e" `Quick test_map_on_filtered_e2e
  ; Alcotest.test_case "flat_map e2e" `Quick test_flat_map_e2e
  ; Alcotest.test_case "filter e2e" `Quick test_filter_e2e
  ; Alcotest.test_case "filter exhaustion e2e" `Quick test_filter_exhaustion_e2e
  ; Alcotest.test_case "group e2e" `Quick test_group_e2e
  ; Alcotest.test_case "discardable_group e2e" `Quick test_discardable_group_e2e
  ; Alcotest.test_case "printer int" `Quick test_printer_int
  ; Alcotest.test_case "printer bool" `Quick test_printer_bool
  ; Alcotest.test_case "printer text" `Quick test_printer_text
  ; Alcotest.test_case "printer filter delegates" `Quick test_printer_filter_delegates
  ; Alcotest.test_case "with_printer" `Quick test_with_printer
  ; Alcotest.test_case "filter on unprintable" `Quick test_filter_on_unprintable
  ; Alcotest.test_case "printer list basic" `Quick test_printer_list_basic
  ; Alcotest.test_case "printer list composite" `Quick test_printer_list_composite
  ; Alcotest.test_case
      "printer list unique composite"
      `Quick
      test_printer_list_unique_composite
  ; Alcotest.test_case "printer tuple2" `Quick test_printer_tuple2
  ; Alcotest.test_case "printer tuple2 composite" `Quick test_printer_tuple2_composite
  ; Alcotest.test_case "printer tuple3" `Quick test_printer_tuple3
  ; Alcotest.test_case "printer tuple4" `Quick test_printer_tuple4
  ; Alcotest.test_case "printer one_of basic" `Quick test_printer_one_of_basic
  ; Alcotest.test_case "printer one_of composite" `Quick test_printer_one_of_composite
  ; Alcotest.test_case "printer hashmap basic" `Quick test_printer_hashmap_basic
  ; Alcotest.test_case "printer hashmap composite" `Quick test_printer_hashmap_composite
  ; Alcotest.test_case "printer optional some" `Quick test_printer_optional_some
  ; Alcotest.test_case "printer optional none" `Quick test_printer_optional_none
  ; Alcotest.test_case "printer optional composite" `Quick test_printer_optional_composite
  ]
;;
