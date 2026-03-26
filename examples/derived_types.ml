(** Derived types: demonstrating [@@deriving generator].

    This example shows how to use the PPX deriver to automatically generate test
    data for user-defined types. Instead of manually constructing generators,
    you annotate your types with [@@deriving generator] and get a
    [<type>_generator : unit -> <type>] function for free. *)

type point = { x : int; y : int } [@@deriving generator]
(** A 2D point with integer coordinates. *)

(** An RGB color represented as a variant. *)
type color = Red | Green | Blue [@@deriving generator]

(** A geometric shape that can be a circle, rectangle, or labeled point. *)
type shape = Circle of float | Rect of int * int | Labeled of string | Dot
[@@deriving generator]

type entity = { name : string; tag : int option; active : bool }
[@@deriving generator]
(** A named entity with an optional tag. *)

(** Property: the distance from any point to the origin is non-negative. *)
let test_point_distance_nonnegative () =
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases:100 ())
    (fun tc ->
      let p = point_generator tc in
      let dist_sq = (p.x * p.x) + (p.y * p.y) in
      assert (dist_sq >= 0))

(** Property: color_generator covers all three constructors. *)
let test_color_all_variants () =
  let saw = Hashtbl.create 3 in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases:50 ())
    (fun tc ->
      let c = color_generator tc in
      match c with
      | Red -> Hashtbl.replace saw "red" true
      | Green -> Hashtbl.replace saw "green" true
      | Blue -> Hashtbl.replace saw "blue" true);
  assert (Hashtbl.length saw = 3)

(** Property: shape_generator covers all constructors. *)
let test_shape_all_variants () =
  let saw_circle = ref false in
  let saw_rect = ref false in
  let saw_labeled = ref false in
  let saw_dot = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases:100 ())
    (fun tc ->
      let s = shape_generator tc in
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
      | Dot -> saw_dot := true);
  assert !saw_circle;
  assert !saw_rect;
  assert !saw_labeled;
  assert !saw_dot

(** Property: entity_generator produces entities with valid fields. *)
let test_entity_valid () =
  let saw_tagged = ref false in
  let saw_untagged = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases:50 ())
    (fun tc ->
      let e = entity_generator tc in
      ignore (String.length e.name);
      ignore e.active;
      match e.tag with
      | Some _ -> saw_tagged := true
      | None -> saw_untagged := true);
  assert !saw_tagged;
  assert !saw_untagged

let () =
  Printf.printf "Running derived-type property tests...\n%!";
  test_point_distance_nonnegative ();
  Printf.printf "  point_distance_nonnegative: OK\n%!";
  test_color_all_variants ();
  Printf.printf "  color_all_variants: OK\n%!";
  test_shape_all_variants ();
  Printf.printf "  shape_all_variants: OK\n%!";
  test_entity_valid ();
  Printf.printf "  entity_valid: OK\n%!";
  Printf.printf "All derived-type tests passed.\n%!"
