open Hegel

let check_printer name gen value expected =
  Alcotest.(check string)
    name
    expected
    (Core.Sexp.to_string (Generators.printer gen value))
;;

(** Test: chars draws [Core.Char.t] values across the full Latin-1 range
    (codepoints 0-255) — not just the ASCII subset — printed via
    [Core.Char.sexp_of_t]. *)
let test_chars_e2e () =
  let saw_above_ascii = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
    let c = draw tc (Hegel_jane.chars ()) in
    if Core.Char.to_int c > 127 then saw_above_ascii := true);
  assert !saw_above_ascii;
  check_printer "chars" (Hegel_jane.chars ()) 'a' "a"
;;

let expect_usage_error gen =
  match
    run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
      ignore (draw tc gen))
  with
  | exception Usage_error _ -> ()
  | () -> Alcotest.fail "expected Usage_error"
;;

let bounds_tests
      name
      ~gen
      ~(bounded : ?lo:'a -> ?hi:'a -> unit -> ('a, printable) generator)
      ~compare
  =
  let leq a b = compare a b <= 0 in
  [ Alcotest.test_case (name ^ " explicit bounds") `Quick (fun () ->
      run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
        let lo = draw tc gen in
        let hi = draw tc (bounded ~lo ()) in
        let v = draw tc (bounded ~lo ~hi ()) in
        assert (leq lo v && leq v hi)))
  ; Alcotest.test_case (name ^ " point bounds") `Quick (fun () ->
      run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
        let lo = draw tc gen in
        assert (compare (draw tc (bounded ~lo ~hi:lo ())) lo = 0)))
  ]
;;

(** Test: time_spans covers the representable range by default,
    including negative spans. *)
let test_time_spans_default_bounds () =
  let module Span = Core.Time_ns.Span in
  let saw_negative = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
    let span = draw tc (Hegel_jane.time_spans ()) in
    if Span.( < ) span Span.zero then saw_negative := true;
    assert (
      Span.( >= ) span Span.min_value_representable
      && Span.( <= ) span Span.max_value_representable));
  assert !saw_negative
;;

let test_time_spans_invalid_bounds () =
  let module Span = Core.Time_ns.Span in
  expect_usage_error (Hegel_jane.time_spans ~min_span:Span.second ~max_span:Span.zero ())
;;

(** Test: times covers the representable range by default. *)
let test_times_default_bounds () =
  let module Time_ns = Core.Time_ns in
  run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
    let t = draw tc (Hegel_jane.times ()) in
    assert (
      Time_ns.( >= ) t Time_ns.min_value_representable
      && Time_ns.( <= ) t Time_ns.max_value_representable))
;;

let test_times_invalid_bounds () =
  let module Time_ns = Core.Time_ns in
  expect_usage_error
    (Hegel_jane.times
       ~min_time:Time_ns.max_value_representable
       ~max_time:Time_ns.epoch
       ())
;;

(** Test: dates defaults to years 1 through 9999. *)
let test_dates_default_bounds () =
  run_hegel_test ~settings:(settings ~test_cases:100 ()) (fun tc ->
    let d = draw tc (Hegel_jane.dates ()) in
    assert (Core.Date.year d >= 1 && Core.Date.year d <= 9999))
;;

let test_dates_invalid_bounds () =
  let d = Core.Date.of_string "2024-01-01" in
  expect_usage_error (Hegel_jane.dates ~min_date:(Core.Date.add_days d 1) ~max_date:d ())
;;

(** Test: ofdays defaults to the whole day. *)
let test_ofdays_default_bounds () =
  let module Ofday = Core.Time_ns.Ofday in
  let saw_end_of_day = ref false in
  let saw_within_day = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ~seed:1 ()) (fun tc ->
    let t = draw tc (Hegel_jane.ofdays ()) in
    assert (Ofday.( >= ) t Ofday.start_of_day && Ofday.( <= ) t Ofday.start_of_next_day);
    if Ofday.equal t Ofday.start_of_next_day
    then saw_end_of_day := true
    else saw_within_day := true);
  assert (!saw_end_of_day && !saw_within_day)
;;

(** Test: lower bound to end of day always generates it *)
let test_ofdays_end_of_day_point () =
  let module Ofday = Core.Time_ns.Ofday in
  run_hegel_test ~settings:(settings ~test_cases:10 ()) (fun tc ->
    let t = draw tc (Hegel_jane.ofdays ~min_ofday:Ofday.start_of_next_day ()) in
    assert (Ofday.equal t Ofday.start_of_next_day))
;;

let test_ofdays_invalid_bounds () =
  let module Ofday = Core.Time_ns.Ofday in
  expect_usage_error
    (Hegel_jane.ofdays
       ~min_ofday:(Ofday.create ~hr:1 ())
       ~max_ofday:Ofday.start_of_day
       ())
;;

let time_bounds_tests =
  bounds_tests
    "time_spans"
    ~gen:(Hegel_jane.time_spans ())
    ~bounded:(fun ?lo ?hi () -> Hegel_jane.time_spans ?min_span:lo ?max_span:hi ())
    ~compare:Core.Time_ns.Span.compare
  @ bounds_tests
      "times"
      ~gen:(Hegel_jane.times ())
      ~bounded:(fun ?lo ?hi () -> Hegel_jane.times ?min_time:lo ?max_time:hi ())
      ~compare:Core.Time_ns.compare
  @ bounds_tests
      "dates"
      ~gen:(Hegel_jane.dates ())
      ~bounded:(fun ?lo ?hi () -> Hegel_jane.dates ?min_date:lo ?max_date:hi ())
      ~compare:Core.Date.compare
  @ bounds_tests
      "ofdays"
      ~gen:(Hegel_jane.ofdays ())
      ~bounded:(fun ?lo ?hi () -> Hegel_jane.ofdays ?min_ofday:lo ?max_ofday:hi ())
      ~compare:Core.Time_ns.Ofday.compare
;;

(** Time values render through their Core sexp converters. *)
let test_printer_times () =
  check_printer
    "time_spans"
    (Hegel_jane.time_spans ())
    (Core.Time_ns.Span.of_int63_ns (Core.Int63.of_int 12345))
    "12.345us";
  check_printer
    "times"
    (Hegel_jane.times ())
    (Core.Time_ns.of_int63_ns_since_epoch (Core.Int63.of_int 12345))
    {|"1970-01-01 00:00:00.000012345Z"|}
;;

(** Test: hash_tables produces a [Core.Hashtbl.t] within the size bounds,
    holding the generated entries. *)
let test_hash_tables_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:50 ()) (fun tc ->
    let gen =
      Hegel_jane.hash_tables
        (integers ~min_value:0 ~max_value:100 ())
        (booleans ())
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
  expect_usage_error
    (Hegel_jane.hash_tables (integers ()) (booleans ()) ~min_size:5 ~max_size:3 ())
;;

(* Hash tables render through [Hashtbl.Poly.sexp_of_t]; a single entry keeps
   the iteration order deterministic. *)
let test_printer_hash_table () =
  check_printer
    "hash table"
    (Hegel_jane.hash_tables (integers ()) (integers ()) ())
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
      , [ Alcotest.test_case "chars e2e" `Quick test_chars_e2e
        ; Alcotest.test_case
            "time_spans default bounds"
            `Quick
            test_time_spans_default_bounds
        ; Alcotest.test_case
            "time_spans invalid bounds"
            `Quick
            test_time_spans_invalid_bounds
        ; Alcotest.test_case "times default bounds" `Quick test_times_default_bounds
        ; Alcotest.test_case "times invalid bounds" `Quick test_times_invalid_bounds
        ; Alcotest.test_case "dates default bounds" `Quick test_dates_default_bounds
        ; Alcotest.test_case "dates invalid bounds" `Quick test_dates_invalid_bounds
        ; Alcotest.test_case "ofdays default bounds" `Quick test_ofdays_default_bounds
        ; Alcotest.test_case "ofdays end-of-day point" `Quick test_ofdays_end_of_day_point
        ; Alcotest.test_case "ofdays invalid bounds" `Quick test_ofdays_invalid_bounds
        ; Alcotest.test_case "printer times" `Quick test_printer_times
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
        ]
        @ time_bounds_tests )
    ]
;;
