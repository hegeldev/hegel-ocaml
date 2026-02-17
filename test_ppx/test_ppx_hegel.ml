(* Test PPX-derived generators *)

(* Simple record *)
type point = { x : int; y : int } [@@deriving hegel]

(* Variant with different constructor types *)
type color = Red | Green | Blue [@@deriving hegel]

(* Variant with data *)
type shape = Circle of float | Rectangle of int * int | Point of point
[@@deriving hegel]

(* Parametric type *)
type 'a box = { value : 'a } [@@deriving hegel]

(* Record with option and list fields *)
type person = {
  name : string;
  age : int;
  email : string option;
  scores : int list;
}
[@@deriving hegel]

(* Type alias *)
type int_pair = int * int [@@deriving hegel]

(* Recursive type *)
type tree = Leaf | Node of int * tree * tree [@@deriving hegel]

(* Variant with inline record *)
type event = Click of { x : int; y : int } | KeyPress of { key : string }
[@@deriving hegel]

(* Nested derived types compose *)
type name = { first : string; last : string } [@@deriving hegel]
type employee = { name : name; department : string } [@@deriving hegel]

(* Multiple type parameters *)
type ('a, 'b) pair_box = { left : 'a; right : 'b } [@@deriving hegel]

let () =
  (* Test point generation *)
  Hegel.run (fun () ->
      let p = gen_point.generate () in
      ignore (p.x + p.y));

  (* Test color generation *)
  Hegel.run (fun () ->
      let c = gen_color.generate () in
      ignore (match c with Red -> 0 | Green -> 1 | Blue -> 2));

  (* Test shape generation *)
  Hegel.run (fun () ->
      let s = gen_shape.generate () in
      ignore
        (match s with
        | Circle r -> r
        | Rectangle (w, h) -> Float.of_int (w * h)
        | Point p -> Float.of_int (p.x + p.y)));

  (* Test parametric box *)
  Hegel.run (fun () ->
      let b = (gen_box (Hegel.Gen.int ())).generate () in
      ignore b.value);

  (* Test person with option/list fields *)
  Hegel.run (fun () ->
      let p = gen_person.generate () in
      assert (String.length p.name >= 0);
      assert (p.age = p.age);
      ignore p.email;
      ignore p.scores);

  (* Test type alias *)
  Hegel.run (fun () ->
      let a, b = gen_int_pair.generate () in
      ignore (a + b));

  (* Test recursive tree *)
  Hegel.run (fun () ->
      let t = gen_tree.generate () in
      let rec size = function
        | Leaf -> 1
        | Node (_, l, r) -> 1 + size l + size r
      in
      assert (size t >= 1));

  (* Test variant with inline record *)
  Hegel.run (fun () ->
      let e = gen_event.generate () in
      ignore
        (match e with
        | Click { x; y } -> x + y
        | KeyPress { key } -> String.length key));

  (* Test nested derived types compose *)
  Hegel.run (fun () ->
      let e = gen_employee.generate () in
      assert (String.length e.name.first >= 0);
      assert (String.length e.department >= 0));

  (* Test multiple type parameters *)
  Hegel.run (fun () ->
      let pb =
        (gen_pair_box (Hegel.Gen.int ()) (Hegel.Gen.string ())).generate ()
      in
      ignore (pb.left + String.length pb.right))
