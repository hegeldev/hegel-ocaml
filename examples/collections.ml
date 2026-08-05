(** Collection and combinator examples.

    Demonstrates: lists, assoc_lists, hash_tables, sampled_from, map,
    flat_map, filter. *)

open Hegel

(** Property: every element in a list of non-negative integers is non-negative.
    Uses [filter] to restrict the element generator. *)
let%hegel_test test_filtered_list tc =
  let non_neg =
    Generators.filter
      (fun v -> v >= 0)
      (Generators.integers ~min_value:(-100) ~max_value:100 ())
  in
  let lst = draw tc (Generators.lists non_neg ~min_size:0 ~max_size:10 ()) in
  List.iter (fun x -> assert (x >= 0)) lst
[@@settings settings ~test_cases:100 ()]
;;

(** Property: a list generated with [min_size] has at least that many elements.
*)
let%hegel_test test_list_min_size tc =
  let lst =
    draw
      tc
      (Generators.lists
         (Generators.integers ~min_value:(-1000) ~max_value:1000 ())
         ~min_size:3
         ~max_size:10
         ())
  in
  assert (List.length lst >= 3)
[@@settings settings ~test_cases:100 ()]
;;

(** Property: [map] transforms every element. Here we map integers to their
    absolute values and check all are >= 0. *)
let%hegel_test test_map_combinator tc =
  let abs_gen =
    Generators.with_printer
      (fun i -> Sexplib0.Sexp.Atom (string_of_int i))
      (Generators.map
         (fun v -> abs v)
         (Generators.integers ~min_value:(-100) ~max_value:100 ()))
  in
  let lst = draw tc (Generators.lists abs_gen ~min_size:1 ~max_size:10 ()) in
  List.iter (fun x -> assert (x >= 0)) lst
[@@settings settings ~test_cases:100 ()]
;;

(** Property: [flat_map] can make a pair (n, list-of-n-integers). Generates an
    integer [n] in [1..5], then generates a list of exactly [n] integers using
    [flat_map]. *)
let%hegel_test test_flat_map_combinator tc =
  let pair_gen =
    Generators.flat_map
      (fun n ->
         Generators.map
           (fun lst -> n, lst)
           (Generators.lists
              (Generators.integers ~min_value:0 ~max_value:99 ())
              ~min_size:n
              ~max_size:n
              ()))
      (Generators.integers ~min_value:1 ~max_value:5 ())
  in
  let n, lst = draw_silent tc pair_gen in
  assert (List.length lst = n)
[@@settings settings ~test_cases:50 ()]
;;

(** Property: [sampled_from] always returns one of the specified values. *)
let%hegel_test test_sampled_from tc =
  let options = [ 10; 20; 30; 40 ] in
  (* [sampled_from] is unprintable; [with_printer] makes it drawable with the
     printing [draw]. *)
  let v =
    draw
      tc
      (Generators.with_printer
         (fun i -> Sexplib0.Sexp.Atom (string_of_int i))
         (Generators.sampled_from options))
  in
  assert (v = 10 || v = 20 || v = 30 || v = 40)
[@@settings settings ~test_cases:100 ()]
;;

(** Property: association lists generated with a min_size have at least that
    many entries. *)
let%hegel_test test_assoc_list_size tc =
  let pairs =
    draw
      tc
      (Generators.assoc_lists
         (Generators.text ~min_size:1 ~max_size:8 ())
         (Generators.integers ~min_value:0 ~max_value:100 ())
         ~min_size:2
         ~max_size:6
         ())
  in
  assert (List.length pairs >= 2)
[@@settings settings ~test_cases:50 ()]
;;

(** Property: hash tables respect their size bounds and hold unique keys by
    construction. *)
let%hegel_test test_hash_table_size tc =
  let table =
    draw
      tc
      (Generators.hash_tables
         (Generators.text ~min_size:1 ~max_size:8 ())
         (Generators.integers ~min_value:0 ~max_value:100 ())
         ~min_size:2
         ~max_size:6
         ())
  in
  let n = Hashtbl.length table in
  assert (n >= 2 && n <= 6)
[@@settings settings ~test_cases:50 ()]
;;

let () =
  Printf.printf "Running collection and combinator examples...\n%!";
  test_filtered_list ();
  Printf.printf "  filtered_list: OK\n%!";
  test_list_min_size ();
  Printf.printf "  list_min_size: OK\n%!";
  test_map_combinator ();
  Printf.printf "  map_combinator: OK\n%!";
  test_flat_map_combinator ();
  Printf.printf "  flat_map_combinator: OK\n%!";
  test_sampled_from ();
  Printf.printf "  sampled_from: OK\n%!";
  test_assoc_list_size ();
  Printf.printf "  assoc_list_size: OK\n%!";
  test_hash_table_size ();
  Printf.printf "  hash_table_size: OK\n%!";
  Printf.printf "All tests passed.\n%!"
;;
