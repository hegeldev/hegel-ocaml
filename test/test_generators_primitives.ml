open Hegel
open Generators

(** Test: integers(0, 100) generates values in range. *)
let test_integers_in_range () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let gen = integers ~min_value:0 ~max_value:100 () in
    let v = Hegel.draw tc gen in
    assert (v >= 0 && v <= 100))
;;

(** Test: unbounded integers() E2E — the engine requires a [min_value], so the
    generator must supply default bounds; values stay within OCaml's native int. *)
let test_integers_unbounded_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let v = Hegel.draw tc (integers ()) in
    assert (v >= Core.Int.min_value && v <= Core.Int.max_value);
    let xs = Hegel.draw tc (lists (integers ()) ()) in
    assert (List.for_all (fun n -> n >= Core.Int.min_value && n <= Core.Int.max_value) xs))
;;

(* ==== Validation tests ==== *)

(** Test: integers raises when min_value > max_value. *)
let test_integers_min_greater_than_max () =
  match integers ~min_value:10 ~max_value:5 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: floats raises when allow_nan=true with min_value set. *)
let test_floats_nan_with_min () =
  match floats ~allow_nan:true ~min_value:0.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: floats raises when allow_nan=true with max_value set. *)
let test_floats_nan_with_max () =
  match floats ~allow_nan:true ~max_value:1.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: floats raises when min_value > max_value. *)
let test_floats_min_greater_than_max () =
  match floats ~min_value:10.0 ~max_value:5.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: floats raises when allow_infinity=true with both bounds set. *)
let test_floats_infinity_with_both_bounds () =
  match floats ~allow_infinity:true ~min_value:0.0 ~max_value:1.0 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when min_size is negative. *)
let test_text_negative_min_size () =
  match text ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when max_size is negative. *)
let test_text_negative_max_size () =
  match text ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when min_size > max_size. *)
let test_text_min_greater_than_max () =
  match text ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: binary raises when min_size is negative. *)
let test_binary_negative_min_size () =
  match binary ~min_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: binary raises when max_size is negative. *)
let test_binary_negative_max_size () =
  match binary ~max_size:(-1) () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: binary raises when min_size > max_size. *)
let test_binary_min_greater_than_max () =
  match binary ~min_size:5 ~max_size:3 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: domains raises when max_length is below 4. *)
let test_domains_max_length_too_small () =
  match domains ~max_length:2 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: domains raises when max_length is above 255. *)
let test_domains_max_length_too_large () =
  match domains ~max_length:256 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(* ==== Character filtering validation tests ==== *)

(** Test: text raises when both categories and exclude_categories are set. *)
let test_text_categories_and_exclude_categories () =
  match text ~categories:[ "L" ] ~exclude_categories:[ "Zs" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when categories include surrogate category Cs. *)
let test_text_categories_surrogate_cs () =
  match text ~categories:[ "Cs" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when categories include surrogate category C. *)
let test_text_categories_surrogate_c () =
  match text ~categories:[ "C" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when alphabet is combined with codec. *)
let test_text_alphabet_with_codec () =
  match text ~alphabet:"abc" ~codec:"ascii" () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(** Test: text raises when alphabet is combined with max_codepoint. *)
let test_text_alphabet_with_max_codepoint () =
  match text ~alphabet:"abc" ~max_codepoint:90 () with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected Invalid_argument"
;;

(* ==== E2E tests ==== *)

(** Test: default floats() E2E — the unbounded default (where allow_nan and
    allow_infinity default to true) produces a valid schema the engine accepts.
    The value may be NaN/infinity, so we only require that a draw succeeds. *)
let test_floats_default_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let (_ : float) = Hegel.draw tc (floats ()) in
    ())
;;

(** Test: default text() E2E — the default form omits max_size *)
let test_text_default_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let s = Hegel.draw tc (text ()) in
    assert (String.length s >= 0))
;;

(** Test: default binary() E2E — the default form omits max_size *)
let test_binary_default_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let b = Hegel.draw tc (binary ()) in
    assert (String.length b >= 0))
;;

(** Test: just always returns the constant. *)
let test_just_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw_silent tc (just 42) in
    Alcotest.(check int) "always 42" 42 v)
;;

(** Test: from_regex generates matching strings. *)
let test_from_regex_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (from_regex "[0-9]+" ()) in
    assert (String.length v > 0))
;;

(** Test: emails generates strings containing at-sign. *)
let test_emails_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (emails ()) in
    assert (String.contains v '@'))
;;

(** Test: urls generates strings starting with http. *)
let test_urls_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (urls ()) in
    assert (
      String.length v >= 7
      && (String.sub v 0 7 = "http://" || String.sub v 0 8 = "https://")))
;;

(** Test: domains generates non-empty strings. *)
let test_domains_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (domains ()) in
    assert (String.length v > 0))
;;

(** Test: dates generates typed [Core.Date.t] values with year in [1, 9999],
    rendered by [Core.Date.to_string]. *)
let test_dates_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let d = Hegel.draw tc (dates ()) in
    let y = Core.Date.year d in
    assert (y >= 1 && y <= 9999);
    let s = Core.Date.to_string d in
    assert (String.length s = 10 && String.contains s '-'))
;;

(** Test: times generates typed [Core.Time_ns.Ofday.t] values within the day,
    rendered by [Core.Time_ns.Ofday.to_string]. *)
let test_times_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let t = Hegel.draw tc (times ()) in
    let ns =
      Core.Time_ns.Span.to_int_ns (Core.Time_ns.Ofday.to_span_since_start_of_day t)
    in
    assert (ns >= 0 && ns < 86_400_000_000_000);
    assert (String.contains (Core.Time_ns.Ofday.to_string t) ':'))
;;

(** Test: datetimes generates typed (date, time-of-day) pairs. *)
let test_datetimes_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let d, t = Hegel.draw tc (datetimes ()) in
    let y = Core.Date.year d in
    assert (y >= 1 && y <= 9999);
    let ns =
      Core.Time_ns.Span.to_int_ns (Core.Time_ns.Ofday.to_span_since_start_of_day t)
    in
    assert (ns >= 0 && ns < 86_400_000_000_000))
;;

(** Test: text with a category restriction (a non-surrogate category). *)
let test_text_categories_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~categories:[ "Lu" ] ~max_size:5 ()) in
    assert (String.length v >= 0))
;;

(** Test: text excluding a category that already lists surrogates (Cs). *)
let test_text_exclude_categories_cs_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~exclude_categories:[ "Cs" ] ~max_size:5 ()) in
    assert (String.length v >= 0))
;;

(** Test: text restricted to a codepoint range. *)
let test_text_codepoint_range_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~min_codepoint:65 ~max_codepoint:90 ~max_size:5 ()) in
    assert (String.length v >= 0))
;;

(** Test: text with an explicit include-characters set. *)
let test_text_include_characters_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~include_characters:"abc" ~max_size:5 ()) in
    assert (String.length v >= 0))
;;

(** Test: text with an explicit exclude-characters set. *)
let test_text_exclude_characters_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~exclude_characters:"z" ~max_size:5 ()) in
    assert (not (String.contains v 'z')))
;;

(** Test: text over a fixed alphabet only draws from that alphabet. *)
let test_text_alphabet_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (text ~alphabet:"abc" ~max_size:5 ()) in
    assert (String.for_all (fun c -> c = 'a' || c = 'b' || c = 'c') v))
;;

(** Test: characters restricted by category draws single characters. *)
let test_characters_categories_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:10 ()) (fun tc ->
    let v = Hegel.draw tc (characters ~categories:[ "Lu" ] ()) in
    assert (String.length v >= 1))
;;

let tests =
  [ Alcotest.test_case "integers in range" `Quick test_integers_in_range
  ; Alcotest.test_case "integers unbounded e2e" `Quick test_integers_unbounded_e2e
  ; Alcotest.test_case "integers min > max" `Quick test_integers_min_greater_than_max
  ; Alcotest.test_case "floats nan with min" `Quick test_floats_nan_with_min
  ; Alcotest.test_case "floats nan with max" `Quick test_floats_nan_with_max
  ; Alcotest.test_case "floats min > max" `Quick test_floats_min_greater_than_max
  ; Alcotest.test_case
      "floats infinity with both bounds"
      `Quick
      test_floats_infinity_with_both_bounds
  ; Alcotest.test_case "text negative min_size" `Quick test_text_negative_min_size
  ; Alcotest.test_case "text negative max_size" `Quick test_text_negative_max_size
  ; Alcotest.test_case "text min > max" `Quick test_text_min_greater_than_max
  ; Alcotest.test_case "binary negative min_size" `Quick test_binary_negative_min_size
  ; Alcotest.test_case "binary negative max_size" `Quick test_binary_negative_max_size
  ; Alcotest.test_case "binary min > max" `Quick test_binary_min_greater_than_max
  ; Alcotest.test_case
      "domains max_length too small"
      `Quick
      test_domains_max_length_too_small
  ; Alcotest.test_case
      "domains max_length too large"
      `Quick
      test_domains_max_length_too_large
  ; Alcotest.test_case
      "text categories + exclude_categories"
      `Quick
      test_text_categories_and_exclude_categories
  ; Alcotest.test_case
      "text categories surrogate Cs"
      `Quick
      test_text_categories_surrogate_cs
  ; Alcotest.test_case
      "text categories surrogate C"
      `Quick
      test_text_categories_surrogate_c
  ; Alcotest.test_case "text alphabet with codec" `Quick test_text_alphabet_with_codec
  ; Alcotest.test_case
      "text alphabet with max_codepoint"
      `Quick
      test_text_alphabet_with_max_codepoint
  ; Alcotest.test_case "floats default e2e" `Quick test_floats_default_e2e
  ; Alcotest.test_case "text default e2e" `Quick test_text_default_e2e
  ; Alcotest.test_case "binary default e2e" `Quick test_binary_default_e2e
  ; Alcotest.test_case "just e2e" `Quick test_just_e2e
  ; Alcotest.test_case "from_regex e2e" `Quick test_from_regex_e2e
  ; Alcotest.test_case "emails e2e" `Quick test_emails_e2e
  ; Alcotest.test_case "urls e2e" `Quick test_urls_e2e
  ; Alcotest.test_case "domains e2e" `Quick test_domains_e2e
  ; Alcotest.test_case "dates e2e" `Quick test_dates_e2e
  ; Alcotest.test_case "times e2e" `Quick test_times_e2e
  ; Alcotest.test_case "datetimes e2e" `Quick test_datetimes_e2e
  ; Alcotest.test_case "text categories e2e" `Quick test_text_categories_e2e
  ; Alcotest.test_case
      "text exclude categories Cs e2e"
      `Quick
      test_text_exclude_categories_cs_e2e
  ; Alcotest.test_case "text codepoint range e2e" `Quick test_text_codepoint_range_e2e
  ; Alcotest.test_case
      "text include characters e2e"
      `Quick
      test_text_include_characters_e2e
  ; Alcotest.test_case
      "text exclude characters e2e"
      `Quick
      test_text_exclude_characters_e2e
  ; Alcotest.test_case "text alphabet e2e" `Quick test_text_alphabet_e2e
  ; Alcotest.test_case "characters categories e2e" `Quick test_characters_categories_e2e
  ]
;;
