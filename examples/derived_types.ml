(** Derived types: demonstrating [@@deriving generator].

    This example shows how to use the PPX deriver to automatically generate test
    data for user-defined types. Instead of manually constructing generators,
    you annotate your types with [@@deriving generator] and get a
    [<type>_generator : (<type>, unprintable) generator] value for free, drawn
    with [Hegel.draw_silent]. (No [@@deriving sexp_of] is needed; add it and use
    [Hegel.with_printer] if you want the value printed on a failing replay.) *)

(** A 2D point with integer coordinates. *)
type point =
  { x : int
  ; y : int
  }
[@@deriving generator]

(** An RGB color represented as a variant. *)
type color =
  | Red
  | Green
  | Blue
[@@deriving generator]

(** A geometric shape that can be a circle, rectangle, or labeled point. *)
type shape =
  | Circle of float
  | Rect of int * int
  | Labeled of string
  | Dot
[@@deriving generator]

(** A named entity with an optional tag. *)
type entity =
  { name : string
  ; tag : int option
  ; active : bool
  }
[@@deriving generator]

(** Property: the distance from any point to the origin is non-negative. *)
let%hegel_test test_point_distance_nonnegative tc =
  let p = Hegel.draw_silent tc point_generator in
  let dist_sq = (p.x * p.x) + (p.y * p.y) in
  assert (dist_sq >= 0)
[@@settings Hegel.settings ~test_cases:100 ()]
;;

(** Property: color_generator covers all three constructors. *)
let saw_colors = Hashtbl.create 3

let%hegel_test test_color_all_variants tc =
  let c = Hegel.draw_silent tc color_generator in
  (match c with
   | Red -> Hashtbl.replace saw_colors "red" true
   | Green -> Hashtbl.replace saw_colors "green" true
   | Blue -> Hashtbl.replace saw_colors "blue" true);
  assert (Hashtbl.length saw_colors >= 1)
[@@settings Hegel.settings ~test_cases:50 ()]
;;

let saw_circle = ref false
let saw_rect = ref false
let saw_labeled = ref false
let saw_dot = ref false

let%hegel_test test_shape_all_variants tc =
  let s = Hegel.draw_silent tc shape_generator in
  match s with
  | Circle r ->
    assert (Float.is_finite r);
    saw_circle := true
  | Rect (w, h) ->
    ignore (w + h);
    saw_rect := true
  | Labeled s ->
    ignore (String.length s);
    saw_labeled := true
  | Dot -> saw_dot := true
[@@settings Hegel.settings ~test_cases:100 ()]
;;

let saw_tagged = ref false
let saw_untagged = ref false

let%hegel_test test_entity_valid tc =
  let e = Hegel.draw_silent tc entity_generator in
  ignore (String.length e.name);
  ignore e.active;
  match e.tag with
  | Some _ -> saw_tagged := true
  | None -> saw_untagged := true
[@@settings Hegel.settings ~test_cases:50 ()]
;;

let () =
  Printf.printf "Running derived-type property tests...\n%!";
  test_point_distance_nonnegative ();
  Printf.printf "  point_distance_nonnegative: OK\n%!";
  test_color_all_variants ();
  assert (Hashtbl.length saw_colors = 3);
  Printf.printf "  color_all_variants: OK\n%!";
  test_shape_all_variants ();
  assert (!saw_circle && !saw_rect && !saw_labeled && !saw_dot);
  Printf.printf "  shape_all_variants: OK\n%!";
  test_entity_valid ();
  assert (!saw_tagged && !saw_untagged);
  Printf.printf "  entity_valid: OK\n%!";
  Printf.printf "All derived-type tests passed.\n%!"
;;
