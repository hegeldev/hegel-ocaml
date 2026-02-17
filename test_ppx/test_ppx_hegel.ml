type point = { x : int; y : int } [@@deriving hegel]
type color = Red | Green | Blue [@@deriving hegel]

type shape = Circle of float | Rectangle of int * int | Point of point
[@@deriving hegel]

type 'a box = { value : 'a } [@@deriving hegel]

type person = {
  name : string;
  age : int;
  email : string option;
  scores : int list;
}
[@@deriving hegel]

type int_pair = int * int [@@deriving hegel]
type tree = Leaf | Node of int * tree * tree [@@deriving hegel]

type event = Click of { x : int; y : int } | KeyPress of { key : string }
[@@deriving hegel]

type name = { first : string; last : string } [@@deriving hegel]
type employee = { name : name; department : string } [@@deriving hegel]
type ('a, 'b) pair_box = { left : 'a; right : 'b } [@@deriving hegel]

let test_point () =
  Hegel.run (fun () ->
      let p = gen_point.generate () in
      ignore (p.x + p.y))

let test_color () =
  Hegel.run (fun () ->
      let c = gen_color.generate () in
      ignore (match c with Red -> 0 | Green -> 1 | Blue -> 2))

let test_shape () =
  Hegel.run (fun () ->
      let s = gen_shape.generate () in
      ignore
        (match s with
        | Circle r -> r
        | Rectangle (w, h) -> Float.of_int (w * h)
        | Point p -> Float.of_int (p.x + p.y)))

let test_parametric_box () =
  Hegel.run (fun () ->
      let b = (gen_box (Hegel.Gen.int ())).generate () in
      ignore b.value)

let test_person () =
  Hegel.run (fun () ->
      let p = gen_person.generate () in
      assert (String.length p.name >= 0);
      assert (p.age = p.age);
      ignore p.email;
      ignore p.scores)

let test_type_alias () =
  Hegel.run (fun () ->
      let a, b = gen_int_pair.generate () in
      ignore (a + b))

let test_recursive_tree () =
  Hegel.run (fun () ->
      let t = gen_tree.generate () in
      let rec size = function
        | Leaf -> 1
        | Node (_, l, r) -> 1 + size l + size r
      in
      assert (size t >= 1))

let test_inline_record_variant () =
  Hegel.run (fun () ->
      let e = gen_event.generate () in
      ignore
        (match e with
        | Click { x; y } -> x + y
        | KeyPress { key } -> String.length key))

let test_nested_derived () =
  Hegel.run (fun () ->
      let e = gen_employee.generate () in
      assert (String.length e.name.first >= 0);
      assert (String.length e.department >= 0))

let test_multiple_type_params () =
  Hegel.run (fun () ->
      let pb =
        (gen_pair_box (Hegel.Gen.int ()) (Hegel.Gen.string ())).generate ()
      in
      ignore (pb.left + String.length pb.right))

let () =
  Alcotest.run "Hegel PPX"
    [
      ( "derived generators",
        [
          Alcotest.test_case "point" `Quick test_point;
          Alcotest.test_case "color" `Quick test_color;
          Alcotest.test_case "shape" `Quick test_shape;
          Alcotest.test_case "parametric box" `Quick test_parametric_box;
          Alcotest.test_case "person" `Quick test_person;
          Alcotest.test_case "type alias" `Quick test_type_alias;
          Alcotest.test_case "recursive tree" `Quick test_recursive_tree;
          Alcotest.test_case "inline record variant" `Quick
            test_inline_record_variant;
          Alcotest.test_case "nested derived" `Quick test_nested_derived;
          Alcotest.test_case "multiple type params" `Quick
            test_multiple_type_params;
        ] );
    ]
