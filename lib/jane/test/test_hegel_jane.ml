open Hegel

let check_printer name gen value expected =
  Alcotest.(check string)
    name
    expected
    (Core.Sexp.to_string (Generators.printer gen value))
;;

(** Test: char draws [Core.Char.t] values across the full Latin-1 range
    (codepoints 0-255) — not just the ASCII subset — printed via
    [Core.Char.sexp_of_t]. *)
let test_char_e2e () =
  let saw_above_ascii = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
    let c = draw tc (Hegel_jane.char ()) in
    if Core.Char.to_int c > 127 then saw_above_ascii := true);
  assert !saw_above_ascii;
  check_printer "char" (Hegel_jane.char ()) 'a' "a"
;;

(** Test: time_ns_spans draws [Core.Time_ns.Span.t] values across its full
    representable range, round-tripping through [Int63]. *)
let test_time_ns_spans_e2e () =
  let saw_negative = ref false in
  run_hegel_test ~settings:(settings ~test_cases:50 ()) (fun tc ->
    let span = draw tc (Hegel_jane.time_ns_spans ()) in
    if Core.Time_ns.Span.( < ) span Core.Time_ns.Span.zero then saw_negative := true;
    let (_ : int) = Core.Int63.to_int_exn (Core.Time_ns.Span.to_int63_ns span) in
    ());
  assert !saw_negative;
  check_printer
    "time_ns_spans"
    (Hegel_jane.time_ns_spans ())
    (Core.Time_ns.Span.of_int63_ns (Core.Int63.of_int 12345))
    "12.345us"
;;

(** Test: time_ns draws [Core.Time_ns.t] values across its full representable
    range, round-tripping through [Int63]. *)
let test_time_ns_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:50 ()) (fun tc ->
    let t = draw tc (Hegel_jane.time_ns ()) in
    let (_ : int) = Core.Int63.to_int_exn (Core.Time_ns.to_int63_ns_since_epoch t) in
    ());
  check_printer
    "time_ns"
    (Hegel_jane.time_ns ())
    (Core.Time_ns.of_int63_ns_since_epoch (Core.Int63.of_int 12345))
    {|"1970-01-01 00:00:00.000012345Z"|}
;;

(** Test: dates generates typed [Core.Date.t] values with year in [1, 9999],
    rendered by [Core.Date.to_string]. *)
let test_dates_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:10 ()) (fun tc ->
    let d = draw tc (Hegel_jane.dates ()) in
    let y = Core.Date.year d in
    assert (y >= 1 && y <= 9999);
    let s = Core.Date.to_string d in
    assert (String.length s = 10 && String.contains s '-'))
;;

(** Test: times generates typed [Core.Time_ns.Ofday.t] values within the day,
    rendered by [Core.Time_ns.Ofday.to_string]. *)
let test_times_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:10 ()) (fun tc ->
    let t = draw tc (Hegel_jane.times ()) in
    let ns =
      Core.Time_ns.Span.to_int_ns (Core.Time_ns.Ofday.to_span_since_start_of_day t)
    in
    assert (ns >= 0 && ns < 86_400_000_000_000);
    assert (String.contains (Core.Time_ns.Ofday.to_string t) ':'))
;;

(** Test: datetimes generates typed (date, time-of-day) pairs. *)
let test_datetimes_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:10 ()) (fun tc ->
    let d, t = draw tc (Hegel_jane.datetimes ()) in
    let y = Core.Date.year d in
    assert (y >= 1 && y <= 9999);
    let ns =
      Core.Time_ns.Span.to_int_ns (Core.Time_ns.Ofday.to_span_since_start_of_day t)
    in
    assert (ns >= 0 && ns < 86_400_000_000_000))
;;

(** Test: hash_tables produces a [Core.Hashtbl.t] within the size bounds,
    holding the generated entries. *)
let test_hash_tables_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:50 ()) (fun tc ->
    let gen =
      Hegel_jane.hash_tables
        (Generators.integers ~min_value:0 ~max_value:100 ())
        (Generators.booleans ())
        ~min_size:1
        ~max_size:5
        ()
    in
    let table = draw tc gen in
    let n = Core.Hashtbl.length table in
    assert (n >= 1 && n <= 5);
    Core.Hashtbl.iteri table ~f:(fun ~key ~data:_ -> assert (key >= 0 && key <= 100)))
;;

(** Test: hash_tables rejects crossed size bounds like assoc_lists. *)
let test_hash_tables_min_greater_than_max () =
  match
    Hegel_jane.hash_tables
      (Generators.integers ())
      (Generators.booleans ())
      ~min_size:5
      ~max_size:3
      ()
  with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(* Hash tables render through [Hashtbl.Poly.sexp_of_t]; a single entry keeps
   the iteration order deterministic. *)
let test_printer_hash_table () =
  check_printer
    "hash table"
    (Hegel_jane.hash_tables (Generators.integers ()) (Generators.integers ()) ())
    (Core.Hashtbl.Poly.of_alist_exn [ 1, 2 ])
    "((1 2))"
;;

let test_resolve_draw () =
  let tbl = Core.Hashtbl.create (module Core.Int) in
  Core.Hashtbl.set tbl ~key:7 ~data:"v";
  (* consume:false keeps the entry *)
  Alcotest.(check string) "draw" "v" (Hegel_jane.resolve_draw tbl ~consume:false 7);
  Alcotest.(check int) "still present" 1 (Core.Hashtbl.length tbl);
  (* consume:true removes it *)
  Alcotest.(check string) "consume" "v" (Hegel_jane.resolve_draw tbl ~consume:true 7);
  Alcotest.(check int) "removed" 0 (Core.Hashtbl.length tbl);
  (* unknown id raises Flaky_strategy *)
  let raised =
    try
      ignore (Hegel_jane.resolve_draw tbl ~consume:false 99 : string);
      false
    with
    | Internal.Flaky_strategy -> true
  in
  Alcotest.(check bool) "unknown id raises Flaky_strategy" true raised
;;

(* Draws through the engine's pool protocol against a Core.Hashtbl-backed
   pool, exercising [pool_values]' [is_empty] closure. *)
let test_pool_values_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:5 ()) (fun tc ->
    let pool = Internal.new_pool tc in
    let tbl = Core.Hashtbl.create (module Core.Int) in
    let variable_id = Internal.pool_add tc ~pool in
    Core.Hashtbl.set tbl ~key:variable_id ~data:"a";
    let gen = Hegel_jane.pool_values ~pool ~values:tbl ~consume:false in
    let v = draw_silent tc gen in
    assert (String.equal v "a"))
;;

let test_sexp_diff_renderer_colored () =
  let original = Core.Sexp.of_string "(1 2 3)" in
  let updated = Core.Sexp.of_string "(1 9 3)" in
  let rendered = Hegel_jane.sexp_diff_renderer ~colored:true ~original ~updated in
  Alcotest.(check bool) "contains an SGR code" true (String.contains rendered '\027')
;;

let () =
  Alcotest.run
    "hegel_jane"
    [ ( "hegel_jane"
      , [ Alcotest.test_case "char e2e" `Quick test_char_e2e
        ; Alcotest.test_case "time_ns_spans e2e" `Quick test_time_ns_spans_e2e
        ; Alcotest.test_case "time_ns e2e" `Quick test_time_ns_e2e
        ; Alcotest.test_case "dates e2e" `Quick test_dates_e2e
        ; Alcotest.test_case "times e2e" `Quick test_times_e2e
        ; Alcotest.test_case "datetimes e2e" `Quick test_datetimes_e2e
        ; Alcotest.test_case "hash_tables e2e" `Quick test_hash_tables_e2e
        ; Alcotest.test_case
            "hash_tables min > max"
            `Quick
            test_hash_tables_min_greater_than_max
        ; Alcotest.test_case "printer hash table" `Quick test_printer_hash_table
        ; Alcotest.test_case "resolve_draw" `Quick test_resolve_draw
        ; Alcotest.test_case "pool_values e2e" `Quick test_pool_values_e2e
        ; Alcotest.test_case
            "sexp_diff_renderer colored"
            `Quick
            test_sexp_diff_renderer_colored
        ] )
    ]
;;
