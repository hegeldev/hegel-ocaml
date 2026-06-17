(** Tests for the [@@deriving hegel_generator] PPX.

    These tests exercise the PPX-generated code for:
    - Record types (product types)
    - Variant types (sum types)
    - Type aliases
    - Single-field records
    - Nested derived types
    - Variants with tuple arguments *)

open! Core
open Hegel

(* ==== Type declarations with derived generators ==== *)

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

(** A type alias to bool. *)
type flag = bool [@@deriving hegel_generator]

(** A type alias to float. *)
type temperature = float [@@deriving hegel_generator]

(** A type alias to string. *)
type label = string [@@deriving hegel_generator]

(** A type with a list field. *)
type int_list_wrapper = { items : int list } [@@deriving hegel_generator]

(* ==== Tests ==== *)

(** Test: derived point generator produces valid points. *)
let%hegel_test test_point_e2e tc =
  let p = Hegel.draw_silent tc point_generator in
  ignore ((p.x, p.y) : int * int)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived person generator produces valid persons. *)
let%hegel_test test_person_e2e tc =
  let p = Hegel.draw_silent tc person_generator in
  ignore ((p.name, p.age, p.active) : string * int * bool)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived score (type alias to int) generates integers. *)
let%hegel_test test_score_e2e tc =
  let _v : score = Hegel.draw_silent tc score_generator in
  ()
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived wrapper (single-field record) generates values. *)
let%hegel_test test_wrapper_e2e tc =
  let w = Hegel.draw_silent tc wrapper_generator in
  ignore w.value
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived line_segment (nested record) generates values. *)
let%hegel_test test_line_segment_e2e tc =
  let ls = Hegel.draw_silent tc line_segment_generator in
  ignore (ls.start_pt.x, ls.start_pt.y, ls.end_pt.x, ls.end_pt.y)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived temperature (type alias to float) generates floats. *)
let%hegel_test test_temperature_e2e tc =
  let f : temperature = Hegel.draw_silent tc temperature_generator in
  assert (Float.is_finite f)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived label (type alias to string) generates strings. *)
let%hegel_test test_label_e2e tc =
  let s : label = Hegel.draw_silent tc label_generator in
  ignore (String.length s)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived int_list_wrapper (list field) generates values. *)
let%hegel_test test_int_list_wrapper_e2e tc =
  let w = Hegel.draw_silent tc int_list_wrapper_generator in
  ignore (List.length w.items)
[@@settings Client.settings ~test_cases:20 ()]
;;

(** Test: derived color generator covers all constructors. *)
let test_color_e2e () =
  let saw_red = ref false in
  let saw_green = ref false in
  let saw_blue = ref false in
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc color_generator with
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
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc shape_generator with
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
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    match (Hegel.draw_silent tc maybe_int_generator).data with
    | Some _ -> saw_some := true
    | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none
;;

(** Test: derived pair_or_single covers both constructors. *)
let test_pair_or_single_e2e () =
  let saw_pair = ref false in
  let saw_single = ref false in
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    match Hegel.draw_silent tc pair_or_single_generator with
    | Pair (a, b) ->
      ignore (a, b);
      saw_pair := true
    | Single n ->
      ignore n;
      saw_single := true);
  assert !saw_pair;
  assert !saw_single
;;

(** Test: derived flag covers both true and false. *)
let test_flag_e2e () =
  let saw_true = ref false in
  let saw_false = ref false in
  Hegel.run_hegel_test ~settings:(Client.settings ~test_cases:50 ()) (fun tc ->
    let b : flag = Hegel.draw_silent tc flag_generator in
    if b then saw_true := true else saw_false := true);
  assert !saw_true;
  assert !saw_false
;;

let () =
  Alcotest.run
    "hegel-ppx-derive"
    [ ( "ppx_derive"
      , [ Alcotest.test_case "derived point" `Quick test_point_e2e
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
        ] )
    ]
;;
