(** Tests for the [@@deriving hegel_generator] PPX.

    These tests exercise the PPX-generated code for:
    - Record types (product types)
    - Variant types (sum types)
    - Type aliases
    - Single-field records
    - Nested derived types
    - Variants with tuple arguments
    - quickcheck-convention naming ([hegel_generator] for [t],
      [hegel_generator_foo] otherwise; [M.t] fields resolve to
      [M.hegel_generator])
    - [char] fields (resolved from [Hegel.Derive], like every primitive)
    - [@hegel.generator] overrides on any type occurrence
    - [@hegel.do_not_generate] on variant constructors
    - always-printable derived generators: the deriver also derives
      [sexp_of_<t>] (via ppx_sexp_conv's expander) and bakes it in with
      [with_printer]; [\[@sexp.opaque\]] is the escape hatch for un-sexpable
      fields *)

open! Core
open Hegel

(** A record with two primitive fields. *)
type point =
  { x : int
  ; y : int
  }
[@@deriving hegel_generator]

(** A record with three different-typed fields. *)
type person =
  { name : string
  ; age : int
  ; active : bool
  }
[@@deriving hegel_generator]

(** A variant with no-arg constructors. *)
type color =
  | Red
  | Green
  | Blue
[@@deriving hegel_generator]

(** A variant with arguments. *)
type shape =
  | Circle of float
  | Rectangle of int * int
  | Point
[@@deriving hegel_generator]

(** A type alias to int. *)
type score = int [@@deriving hegel_generator]

(** A single-field record. *)
type wrapper = { value : int } [@@deriving hegel_generator]

(** A type with an option field. *)
type maybe_int = { data : int option } [@@deriving hegel_generator]

(** A nested derived type: record containing another derived record. *)
type line_segment =
  { start_pt : point
  ; end_pt : point
  }
[@@deriving hegel_generator]

(** A variant with a tuple argument. *)
type pair_or_single =
  | Pair of int * int
  | Single of int
[@@deriving hegel_generator]

type measured =
  | Dimensions of
      { width : int
      ; height : int
      }
  | Unmeasured
[@@deriving hegel_generator]

(** A type alias to bool. *)
type flag = bool [@@deriving hegel_generator]

(** A type alias to float. *)
type temperature = float [@@deriving hegel_generator]

(** A type alias to string. *)
type label = string [@@deriving hegel_generator]

(** A type with a list field. *)
type int_list_wrapper = { items : int list } [@@deriving hegel_generator]

(** A record with an int field, used to check that derived ints span the full
    native-int range (regression for the old ±2³⁰−1 clamp). *)
type full_range = { n : int } [@@deriving hegel_generator]

type char_and_float =
  { a : char
  ; b : float
  }
[@@deriving hegel_generator]

(** A module whose main type is [t]: derives a value named plain
    [hegel_generator] (no [_t] suffix), following the quickcheck convention. *)
module Temperature_reading = struct
  type t = { celsius : float } [@@deriving hegel_generator]
end

(** A record with a qualified [M.t] field: the deriver resolves it to
    [Temperature_reading.hegel_generator]. *)
type weather =
  { reading : Temperature_reading.t
  ; humidity : int
  }
[@@deriving hegel_generator]

(** A record-field override: limit an int to [\[3, 5\]] by swapping in a custom
    generator (the quickcheck idiom for range-limiting a field). *)
type ranked =
  { name : string
  ; level : (int[@hegel.generator integers ~min_value:3 ~max_value:5 ()])
  }
[@@deriving hegel_generator]

(** A constructor-argument override *)
type aged =
  | Age of (int[@hegel.generator integers ~min_value:18 ~max_value:99 ()])
  | Unknown
[@@deriving hegel_generator]

(** A type with no generator anywhere in scope (and none derivable: it holds a
    function). Excluded constructors must not require one. *)
type ungeneratable = { thunk : unit -> unit }

(** An all-nullary variant with an excluded constructor: the generator samples
    only from the remaining constructors. *)
type compass =
  | North
  | South
  | Broken [@hegel.do_not_generate]
[@@deriving hegel_generator]

(** A data-carrying variant with an excluded constructor whose argument type
    has no generator. [\[@hegel.do_not_generate\]] implies opaque printing for
    the excluded arguments (the deriver wraps them in [\[@sexp.opaque\]]
    before invoking ppx_sexp_conv's expander). *)
type task_result =
  | Finished of int
  | Blocked of ungeneratable [@hegel.do_not_generate]
  | Cancelled
[@@deriving hegel_generator]

type printed_point =
  { px : int
  ; py : int
  }
[@@deriving hegel_generator]

type with_opaque =
  { id : int
  ; handle : (ungeneratable[@sexp.opaque] [@hegel.generator just { thunk = Fun.id }])
  }
[@@deriving hegel_generator]

(** Test: derived [int] fields use the same full default range as
    [integers ()], not the old 30-bit clamp. The engine over-weights boundary
    values (empirically ~36% of unbounded draws exceed 2³⁰−1 in magnitude), so
    200 cases see one with near-certainty. *)
let test_int_full_range_e2e () =
  let clamp = 1073741823 in
  let saw_beyond_clamp = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:200 ()) (fun tc ->
    let r = Hegel.draw_silent tc hegel_generator_full_range in
    if r.n > clamp || r.n < -clamp then saw_beyond_clamp := true);
  assert !saw_beyond_clamp
;;

(** Test: derived point generator produces valid points. *)
let%hegel_test test_point_e2e tc =
  let p = Hegel.draw_silent tc hegel_generator_point in
  ignore ((p.x, p.y) : int * int)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived person generator produces valid persons. *)
let%hegel_test test_person_e2e tc =
  let p = Hegel.draw_silent tc hegel_generator_person in
  ignore ((p.name, p.age, p.active) : string * int * bool)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived score (type alias to int) generates integers. *)
let%hegel_test test_score_e2e tc =
  let _v : score = Hegel.draw_silent tc hegel_generator_score in
  ()
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived wrapper (single-field record) generates values. *)
let%hegel_test test_wrapper_e2e tc =
  let w = Hegel.draw_silent tc hegel_generator_wrapper in
  ignore w.value
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived line_segment (nested record) generates values, resolving the
    unqualified [point] fields to [hegel_generator_point]. *)
let%hegel_test test_line_segment_e2e tc =
  let ls = Hegel.draw_silent tc hegel_generator_line_segment in
  ignore (ls.start_pt.x, ls.start_pt.y, ls.end_pt.x, ls.end_pt.y)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived temperature (type alias to float) generates floats. *)
let%hegel_test test_temperature_e2e tc =
  let f : temperature = Hegel.draw_silent tc hegel_generator_temperature in
  assert (Float.is_finite f)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived label (type alias to string) generates strings. *)
let%hegel_test test_label_e2e tc =
  let s : label = Hegel.draw_silent tc hegel_generator_label in
  ignore (String.length s)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived int_list_wrapper (list field) generates values. *)
let%hegel_test test_int_list_wrapper_e2e tc =
  let w = Hegel.draw_silent tc hegel_generator_int_list_wrapper in
  ignore (List.length w.items)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: derived color generator covers all constructors. *)
let test_color_e2e () =
  let saw_red = ref false in
  let saw_green = ref false in
  let saw_blue = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_color with
    | Red -> saw_red := true
    | Green -> saw_green := true
    | Blue -> saw_blue := true);
  assert !saw_red;
  assert !saw_green;
  assert !saw_blue
;;

(** Test: derived shape generator covers all constructors. *)
let test_shape_e2e () =
  let saw_circle = ref false in
  let saw_rectangle = ref false in
  let saw_point = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_shape with
    | Circle f ->
      assert (Float.is_finite f);
      saw_circle := true
    | Rectangle (w, h) ->
      ignore (w, h);
      saw_rectangle := true
    | Point -> saw_point := true);
  assert !saw_circle;
  assert !saw_rectangle;
  assert !saw_point
;;

(** Test: derived maybe_int (option field) generates both Some and None. *)
let test_maybe_int_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match (Hegel.draw_silent tc hegel_generator_maybe_int).data with
    | Some _ -> saw_some := true
    | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none
;;

(** Test: derived pair_or_single covers both constructors. *)
let test_pair_or_single_e2e () =
  let saw_pair = ref false in
  let saw_single = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_pair_or_single with
    | Pair (a, b) ->
      ignore (a, b);
      saw_pair := true
    | Single n ->
      ignore n;
      saw_single := true);
  assert !saw_pair;
  assert !saw_single
;;

(** Test: derived measured (inline-record constructor, [Pcstr_record]) covers
    both constructors and draws the inline fields. *)
let test_inline_record_e2e () =
  let saw_dimensions = ref false in
  let saw_unmeasured = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_measured with
    | Dimensions { width; height } ->
      ignore ((width, height) : int * int);
      saw_dimensions := true
    | Unmeasured -> saw_unmeasured := true);
  assert !saw_dimensions;
  assert !saw_unmeasured
;;

(** Test: derived flag covers both true and false. *)
let test_flag_e2e () =
  let saw_true = ref false in
  let saw_false = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let b : flag = Hegel.draw_silent tc hegel_generator_flag in
    if b then saw_true := true else saw_false := true);
  assert !saw_true;
  assert !saw_false
;;

let%hegel_test test_char_and_float_e2e tc =
  let r = Hegel.draw_silent tc hegel_generator_char_and_float in
  assert (Char.to_int r.a >= 0 && Char.to_int r.a <= 255);
  assert (Float.is_finite r.b)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: a module's [t] derives a value named plain [hegel_generator]. *)
let%hegel_test test_module_t_naming_e2e tc =
  let r = Hegel.draw_silent tc Temperature_reading.hegel_generator in
  assert (Float.is_finite r.Temperature_reading.celsius)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: a qualified [M.t] field resolves to [M.hegel_generator]. *)
let%hegel_test test_qualified_field_e2e tc =
  let w = Hegel.draw_silent tc hegel_generator_weather in
  assert (Float.is_finite w.reading.Temperature_reading.celsius);
  ignore (w.humidity : int)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: a [@hegel.generator] field override pins the field to the custom
    generator's distribution — every draw lands in [\[3, 5\]] *)
let test_field_override_range_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:200 ()) (fun tc ->
    let r = Hegel.draw_silent tc hegel_generator_ranked in
    assert (r.level >= 3 && r.level <= 5))
;;

(** Test: a [@hegel.generator] override on a constructor argument. *)
let test_constructor_arg_override_e2e () =
  let saw_age = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:100 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_aged with
    | Age n ->
      assert (n >= 18 && n <= 99);
      saw_age := true
    | Unknown -> ());
  assert !saw_age
;;

(** Test: [@hegel.do_not_generate] on an all-nullary variant — the excluded
    constructor never appears, the others still do. *)
let test_do_not_generate_nullary_e2e () =
  let saw_north = ref false in
  let saw_south = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_compass with
    | North -> saw_north := true
    | South -> saw_south := true
    | Broken -> failwith "Broken is marked [@hegel.do_not_generate]");
  assert !saw_north;
  assert !saw_south
;;

(** Test: [@hegel.do_not_generate] on a data-carrying constructor whose
    argument type has no generator — the type still derives (the deriver never
    references [hegel_generator_ungeneratable]) and the constructor never
    appears. *)
let test_do_not_generate_data_e2e () =
  let saw_finished = ref false in
  let saw_cancelled = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc hegel_generator_task_result with
    | Finished _ -> saw_finished := true
    | Blocked _ -> failwith "Blocked is marked [@hegel.do_not_generate]"
    | Cancelled -> saw_cancelled := true);
  assert !saw_finished;
  assert !saw_cancelled;
  let blocked_sexp = Sexp.to_string (sexp_of_task_result (Blocked { thunk = Fun.id })) in
  assert (String.is_substring blocked_sexp ~substring:"opaque")
;;

(** Test: bare deriving yields a printable generator *)
let%hegel_test test_printer_e2e tc =
  let p = Hegel.draw tc hegel_generator_printed_point in
  ignore ((p.px, p.py) : int * int);
  ignore (sexp_of_printed_point p : Sexp.t)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: an [\[@sexp.opaque\]] field prints as the opaque placeholder while
    [\[@hegel.generator\]] supplies its values. *)
let%hegel_test test_opaque_field_e2e tc =
  let w = Hegel.draw tc hegel_generator_with_opaque in
  ignore (w.id : int);
  w.handle.thunk ();
  ignore (sexp_of_with_opaque w : Sexp.t)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

let () =
  Alcotest.run
    "hegel-ppx-derive"
    [ ( "ppx_derive"
      , [ Alcotest.test_case "derived point" `Quick test_point_e2e
        ; Alcotest.test_case "derived int full range" `Quick test_int_full_range_e2e
        ; Alcotest.test_case "derived person" `Quick test_person_e2e
        ; Alcotest.test_case "derived color covers all" `Quick test_color_e2e
        ; Alcotest.test_case "derived shape covers all" `Quick test_shape_e2e
        ; Alcotest.test_case "derived score (alias)" `Quick test_score_e2e
        ; Alcotest.test_case "derived wrapper (single field)" `Quick test_wrapper_e2e
        ; Alcotest.test_case "derived maybe_int (option)" `Quick test_maybe_int_e2e
        ; Alcotest.test_case "derived line_segment (nested)" `Quick test_line_segment_e2e
        ; Alcotest.test_case
            "derived pair_or_single (tuple)"
            `Quick
            test_pair_or_single_e2e
        ; Alcotest.test_case "derived flag (bool alias)" `Quick test_flag_e2e
        ; Alcotest.test_case
            "derived temperature (float alias)"
            `Quick
            test_temperature_e2e
        ; Alcotest.test_case "derived label (string alias)" `Quick test_label_e2e
        ; Alcotest.test_case
            "derived int_list_wrapper (list)"
            `Quick
            test_int_list_wrapper_e2e
        ; Alcotest.test_case "derived char record" `Quick test_char_and_float_e2e
        ; Alcotest.test_case
            "derived measured (inline record)"
            `Quick
            test_inline_record_e2e
        ; Alcotest.test_case "module t naming" `Quick test_module_t_naming_e2e
        ; Alcotest.test_case "qualified M.t field" `Quick test_qualified_field_e2e
        ; Alcotest.test_case "field override range" `Quick test_field_override_range_e2e
        ; Alcotest.test_case
            "constructor arg override"
            `Quick
            test_constructor_arg_override_e2e
        ; Alcotest.test_case
            "do_not_generate (nullary)"
            `Quick
            test_do_not_generate_nullary_e2e
        ; Alcotest.test_case "do_not_generate (data)" `Quick test_do_not_generate_data_e2e
        ; Alcotest.test_case "default printer" `Quick test_printer_e2e
        ; Alcotest.test_case "opaque field escape hatch" `Quick test_opaque_field_e2e
        ] )
    ]
;;
