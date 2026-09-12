open Hegel
open Generators
open Generators.Ppx_internal

(* [Labels.from_name] / [Labels.combine] are the engine's own hashes. *)
let test_labels_match_engine () =
  let ctx = Hegel_ffi.Ffi.context_new () in
  Fun.protect
    ~finally:(fun () -> Hegel_ffi.Ffi.context_free ctx)
    (fun () ->
       List.iter
         (fun name ->
            Alcotest.(check int64)
              name
              (Hegel_ffi.Ffi.label_from_name ctx name)
              (Labels.from_name name))
         [ ""; "hegel_ocaml.list"; "hegel.integer"; "h\xc3\xa9llo" ];
       List.iter
         (fun labels ->
            Alcotest.(check int64)
              "combine"
              (Hegel_ffi.Ffi.label_combine ctx labels)
              (Labels.combine labels))
         [ []
         ; [ Labels.list ]
         ; [ Labels.list; Labels.from_name "x"; 0L; -1L; Int64.max_int ]
         ])
;;

let test_max_filter_attempts () =
  Alcotest.(check int) "max attempts" 3 max_filter_attempts
;;

(* [with_tc f] runs [f] with a real per-test-case handle from the native
   engine. Used by the collection-record tests, which exercise the OCaml-side
   collection bookkeeping. *)
let with_tc f = Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) f

(* A label reflects the generator's kind and its components'. *)
let test_labels_structural () =
  let same name a b = Alcotest.(check int64) name (label_of a) (label_of b) in
  let differ name a b =
    Alcotest.(check bool) name false (Int64.equal (label_of a) (label_of b))
  in
  same "same shape" (lists (integers ()) ()) (lists (integers ()) ());
  differ "element kind" (lists (integers ()) ()) (lists (text ()) ());
  differ "unique" (lists (integers ()) ()) (lists (integers ()) ~unique:true ());
  differ "map on a leaf" (map succ (integers ())) (integers ());
  differ
    "map on a composite"
    (map fst (tuples2 (integers ()) (text ())))
    (tuples2 (integers ()) (text ()));
  same
    "printer is not part of the label"
    (with_printer sexp_of_int (map succ (integers ())))
    (map succ (integers ()));
  differ "flat_map" (flat_map (fun _ -> integers ()) (integers ())) (integers ());
  differ "filter" (filter (fun _ -> true) (integers ())) (integers ());
  differ "tuple order" (tuples2 (integers ()) (text ())) (tuples2 (text ()) (integers ()));
  differ "one_of alternatives" (one_of [ integers () ]) (one_of [ text () ]);
  differ "optional" (optional (integers ())) (optional (text ()));
  differ
    "assoc_list vs hash_table"
    (assoc_lists (integers ()) (text ()) ())
    (hash_tables (integers ()) (text ()) ());
  differ
    "functions"
    (functions ~returns:(integers ()) ())
    (functions ~returns:(text ()) ());
  Alcotest.(check int64) "just" (Labels.from_name "hegel_ocaml.just") (label_of (just 1));
  with_tc (fun tc ->
    let pool = Stateful.Pool.create tc in
    Alcotest.(check int64)
      "pool"
      Labels.pool
      (label_of (Stateful.Pool.values_reusable pool)))
;;

let test_collection_new () =
  with_tc (fun data ->
    with_collection ~min_size:0 ~max_size:5 data (fun coll ->
      Alcotest.(check bool) "not finished" false coll.finished))
;;

let test_collection_new_no_max () =
  with_tc (fun data ->
    with_collection ~min_size:0 data (fun coll ->
      Alcotest.(check bool) "not finished" false coll.finished;
      Alcotest.(check bool) "max_size is None" true (coll.max_size = None)))
;;

let test_collection_reject_when_finished () =
  with_tc (fun data ->
    with_collection ~min_size:0 data (fun coll ->
      coll.finished <- true;
      collection_reject coll data))
;;

(** Test: collection_more returns false when already finished. *)
let test_collection_more_when_finished () =
  with_tc (fun data ->
    with_collection ~min_size:0 data (fun coll ->
      coll.finished <- true;
      let result = collection_more coll data in
      Alcotest.(check bool) "returns false" false result))
;;

(** Test: discardable_group exception path — stop_span skipped when aborted. *)
let test_discardable_group_exception () =
  with_tc (fun data ->
    Internal.set_test_aborted data true;
    let raised = ref false in
    (try ignore (discardable_group Labels.flat_map data (fun () -> raise Exit) : _) with
     | Exit -> raised := true);
    Alcotest.(check bool) "raised Exit" true !raised)
;;

(* ==== E2E tests ==== *)

(** Test: map doubles values correctly. *)
let test_map_doubles_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let gen = integers ~min_value:1 ~max_value:5 () |> map (fun v -> v * 2) in
    let v = Hegel.draw_silent tc gen in
    assert (v >= 2 && v <= 10);
    assert (v mod 2 = 0))
;;

(** Test: double map composes correctly (Leaf draw-closure composition). *)
let test_double_map_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let gen =
      integers ~min_value:1 ~max_value:5 ()
      |> map (fun v -> v * 2)
      |> map (fun v -> v + 1)
    in
    let v = Hegel.draw_silent tc gen in
    assert (List.mem v [ 3; 5; 7; 9; 11 ]))
;;

(** Test: map on non-basic (Mapped branch of do_draw). *)
let test_map_on_filtered_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let gen =
      filter (fun v -> v > 5) (integers ~min_value:0 ~max_value:10 ())
      |> map (fun v -> v * 2)
    in
    let v = Hegel.draw_silent tc gen in
    assert (v > 10 && v <= 20))
;;

(** Test: flat_map through engine. *)
let test_flat_map_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let gen =
      flat_map
        (fun n -> integers ~min_value:0 ~max_value:(max 1 n) ())
        (integers ~min_value:1 ~max_value:5 ())
    in
    let v = Hegel.draw_silent tc gen in
    assert (v >= 0))
;;

(** Test: filter through engine. *)
let test_filter_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let gen = filter (fun v -> v mod 2 = 0) (integers ~min_value:0 ~max_value:100 ()) in
    let v = Hegel.draw tc gen in
    assert (v mod 2 = 0))
;;

(** Test: filter exhaustion through engine (always false → assume false). *)
let test_filter_exhaustion_e2e () =
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:10 ()) with
        suppress_health_check = [ Settings.Filter_too_much ]
      }
    (fun tc ->
       let gen = filter (fun _ -> false) (integers ~min_value:0 ~max_value:10 ()) in
       ignore (Hegel.draw tc gen))
;;

(** Test: group helper through engine. *)
let test_group_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
    let n =
      group Labels.list tc (fun () ->
        Internal.generate_integer tc ~min_value:0 ~max_value:10)
    in
    assert (n >= 0 && n <= 10))
;;

(** Test: discardable_group through engine — success path. *)
let test_discardable_group_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
    let n =
      discardable_group Labels.tuple tc (fun () ->
        Internal.generate_integer tc ~min_value:0 ~max_value:10)
    in
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
let test_printer_chars () = check_printer "chars" (chars ()) 'a' "a"

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
   drawn via [draw_silent]. *)
let test_filter_on_unprintable () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
    let gen = filter (fun _ -> true) (sampled_from [ 1; 2; 3 ]) in
    let v = Hegel.draw_silent tc gen in
    assert (List.mem v [ 1; 2; 3 ]))
;;

(* Lists render via both the engine-side path (basic elements) and the
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

(* Association lists render via both the dict-schema (basic) and collection
   paths. *)
let test_printer_assoc_list_basic () =
  check_printer
    "association list"
    (assoc_lists (integers ()) (integers ()) ())
    [ 1, 2; 3, 4 ]
    "((1 2)(3 4))"
;;

let test_printer_assoc_list_composite () =
  check_printer
    "association list composite"
    (assoc_lists (filter (fun _ -> true) (integers ())) (integers ()) ())
    [ 1, 2 ]
    "((1 2))"
;;

(* Hash tables render by folding the [Stdlib.Hashtbl]; a single entry keeps the
   iteration order deterministic. *)
let test_printer_hash_table () =
  let table = Stdlib.Hashtbl.create 1 in
  Stdlib.Hashtbl.replace table 1 2;
  check_printer "hash table" (hash_tables (integers ()) (integers ()) ()) table "((1 2))"
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

module Pool_gen = Make_pool (Int_table)

let test_resolve_draw () =
  let tbl = Int_table.create 4 in
  Int_table.replace tbl 7 "v";
  (* consume:false keeps the entry *)
  Alcotest.(check string) "draw" "v" (Pool_gen.resolve_draw tbl ~consume:false 7);
  Alcotest.(check int) "still present" 1 (Int_table.length tbl);
  (* consume:true removes it *)
  Alcotest.(check string) "consume" "v" (Pool_gen.resolve_draw tbl ~consume:true 7);
  Alcotest.(check int) "removed" 0 (Int_table.length tbl);
  (* unknown id raises Flaky_strategy *)
  let raised =
    try
      ignore (Pool_gen.resolve_draw tbl ~consume:false 99 : string);
      false
    with
    | Internal.Flaky_strategy -> true
  in
  Alcotest.(check bool) "unknown id raises Flaky_strategy" true raised
;;

let tests =
  [ Alcotest.test_case "stateful: resolve_draw" `Quick test_resolve_draw
  ; Alcotest.test_case "labels match engine" `Quick test_labels_match_engine
  ; Alcotest.test_case "labels structural" `Quick test_labels_structural
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
  ; Alcotest.test_case "printer chars" `Quick test_printer_chars
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
  ; Alcotest.test_case
      "printer association list basic"
      `Quick
      test_printer_assoc_list_basic
  ; Alcotest.test_case
      "printer association list composite"
      `Quick
      test_printer_assoc_list_composite
  ; Alcotest.test_case "printer hash table" `Quick test_printer_hash_table
  ; Alcotest.test_case "printer optional some" `Quick test_printer_optional_some
  ; Alcotest.test_case "printer optional none" `Quick test_printer_optional_none
  ; Alcotest.test_case "printer optional composite" `Quick test_printer_optional_composite
  ]
;;
