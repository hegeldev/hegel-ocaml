(** Tests for the [@@deriving generator] PPX.

    These tests exercise the PPX-generated code for:
    - Record types (product types)
    - Variant types (sum types)
    - Type aliases
    - Single-field records
    - Nested derived types
    - Variants with tuple arguments *)

open Hegel

(* ==== Type declarations with derived generators ==== *)

type point = { x : int; y : int } [@@deriving generator]
(** A record with two primitive fields. *)

type person = { name : string; age : int; active : bool } [@@deriving generator]
(** A record with three different-typed fields. *)

(** A variant with no-arg constructors. *)
type color = Red | Green | Blue [@@deriving generator]

(** A variant with arguments. *)
type shape = Circle of float | Rectangle of int * int | Point
[@@deriving generator]

type score = int [@@deriving generator]
(** A type alias to int. *)

type wrapper = { value : int } [@@deriving generator]
(** A single-field record. *)

type maybe_int = { data : int option } [@@deriving generator]
(** A type with an option field. *)

type line_segment = { start_pt : point; end_pt : point } [@@deriving generator]
(** A nested derived type: record containing another derived record. *)

(** A variant with a tuple argument. *)
type pair_or_single = Pair of int * int | Single of int [@@deriving generator]

type flag = bool [@@deriving generator]
(** A type alias to bool. *)

type temperature = float [@@deriving generator]
(** A type alias to float. *)

type label = string [@@deriving generator]
(** A type alias to string. *)

type int_list_wrapper = { items : int list } [@@deriving generator]
(** A type with a list field. *)

(* ==== Test functions ==== *)

(** Test: derived point generator produces valid points. *)
let test_point_e2e () =
  Session.run_hegel_test ~name:"derived_point" ~test_cases:20 (fun () ->
      let p = point_generator () in
      ignore (p.x, p.y))

(** Test: derived person generator produces valid persons. *)
let test_person_e2e () =
  Session.run_hegel_test ~name:"derived_person" ~test_cases:20 (fun () ->
      let p = person_generator () in
      ignore (p.name, p.age, p.active))

(** Test: derived color generator covers all constructors. *)
let test_color_e2e () =
  let saw_red = ref false in
  let saw_green = ref false in
  let saw_blue = ref false in
  Session.run_hegel_test ~name:"derived_color" ~test_cases:50 (fun () ->
      let v = color_generator () in
      match v with
      | Red -> saw_red := true
      | Green -> saw_green := true
      | Blue -> saw_blue := true);
  assert !saw_red;
  assert !saw_green;
  assert !saw_blue

(** Test: derived shape generator covers all constructors. *)
let test_shape_e2e () =
  let saw_circle = ref false in
  let saw_rectangle = ref false in
  let saw_point = ref false in
  Session.run_hegel_test ~name:"derived_shape" ~test_cases:50 (fun () ->
      let v = shape_generator () in
      match v with
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

(** Test: derived score (type alias to int) generates integers. *)
let test_score_e2e () =
  Session.run_hegel_test ~name:"derived_score" ~test_cases:20 (fun () ->
      let _v : score = score_generator () in
      ())

(** Test: derived wrapper (single-field record) generates values. *)
let test_wrapper_e2e () =
  Session.run_hegel_test ~name:"derived_wrapper" ~test_cases:20 (fun () ->
      let w = wrapper_generator () in
      ignore w.value)

(** Test: derived maybe_int (option field) generates values. *)
let test_maybe_int_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Session.run_hegel_test ~name:"derived_maybe_int" ~test_cases:50 (fun () ->
      let m = maybe_int_generator () in
      match m.data with Some _ -> saw_some := true | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none

(** Test: derived line_segment (nested record) generates values. *)
let test_line_segment_e2e () =
  Session.run_hegel_test ~name:"derived_line_seg" ~test_cases:20 (fun () ->
      let ls = line_segment_generator () in
      ignore (ls.start_pt.x, ls.start_pt.y, ls.end_pt.x, ls.end_pt.y))

(** Test: derived pair_or_single (variant with tuple) generates values. *)
let test_pair_or_single_e2e () =
  let saw_pair = ref false in
  let saw_single = ref false in
  Session.run_hegel_test ~name:"derived_pair_single" ~test_cases:50 (fun () ->
      let v = pair_or_single_generator () in
      match v with
      | Pair (a, b) ->
          ignore (a, b);
          saw_pair := true
      | Single n ->
          ignore n;
          saw_single := true);
  assert !saw_pair;
  assert !saw_single

(** Test: derived flag (type alias to bool) generates booleans. *)
let test_flag_e2e () =
  let saw_true = ref false in
  let saw_false = ref false in
  Session.run_hegel_test ~name:"derived_flag" ~test_cases:50 (fun () ->
      let b : flag = flag_generator () in
      if b then saw_true := true else saw_false := true);
  assert !saw_true;
  assert !saw_false

(** Test: derived temperature (type alias to float) generates floats. *)
let test_temperature_e2e () =
  Session.run_hegel_test ~name:"derived_temp" ~test_cases:20 (fun () ->
      let f : temperature = temperature_generator () in
      assert (Float.is_finite f))

(** Test: derived label (type alias to string) generates strings. *)
let test_label_e2e () =
  Session.run_hegel_test ~name:"derived_label" ~test_cases:20 (fun () ->
      let s : label = label_generator () in
      ignore (String.length s))

(** Test: derived int_list_wrapper (list field) generates values. *)
let test_int_list_wrapper_e2e () =
  Session.run_hegel_test ~name:"derived_list_wrap" ~test_cases:20 (fun () ->
      let w = int_list_wrapper_generator () in
      ignore (List.length w.items))

let () =
  Alcotest.run "hegel-ppx-derive"
    [
      ( "ppx_derive",
        [
          Alcotest.test_case "derived point" `Quick test_point_e2e;
          Alcotest.test_case "derived person" `Quick test_person_e2e;
          Alcotest.test_case "derived color covers all" `Quick test_color_e2e;
          Alcotest.test_case "derived shape covers all" `Quick test_shape_e2e;
          Alcotest.test_case "derived score (alias)" `Quick test_score_e2e;
          Alcotest.test_case "derived wrapper (single field)" `Quick
            test_wrapper_e2e;
          Alcotest.test_case "derived maybe_int (option)" `Quick
            test_maybe_int_e2e;
          Alcotest.test_case "derived line_segment (nested)" `Quick
            test_line_segment_e2e;
          Alcotest.test_case "derived pair_or_single (tuple)" `Quick
            test_pair_or_single_e2e;
          Alcotest.test_case "derived flag (bool alias)" `Quick test_flag_e2e;
          Alcotest.test_case "derived temperature (float alias)" `Quick
            test_temperature_e2e;
          Alcotest.test_case "derived label (string alias)" `Quick
            test_label_e2e;
          Alcotest.test_case "derived int_list_wrapper (list)" `Quick
            test_int_list_wrapper_e2e;
        ] );
    ]
