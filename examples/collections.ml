(** Collection and combinator examples.

    Demonstrates: lists, hashmaps, sampled_from, map, flat_map, filter. *)

open Hegel.Generators

(** Property: every element in a list of non-negative integers is non-negative.
    Uses [filter] to restrict the element generator. *)
let test_filtered_list () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let non_neg =
        filter (fun v -> v >= 0) (integers ~min_value:(-100) ~max_value:100 ())
      in
      let lst = Hegel.draw tc (lists non_neg ~min_size:0 ~max_size:10 ()) in
      List.iter (fun x -> assert (x >= 0)) lst)

(** Property: a list generated with [min_size] has at least that many elements.
*)
let test_list_min_size () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let lst =
        Hegel.draw tc (lists (integers ()) ~min_size:3 ~max_size:10 ())
      in
      assert (List.length lst >= 3))

(** Property: [map] transforms every element. Here we map integers to their
    absolute values and check all are >= 0. *)
let test_map_combinator () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let abs_gen =
        map (fun v -> abs v) (integers ~min_value:(-100) ~max_value:100 ())
      in
      let lst = Hegel.draw tc (lists abs_gen ~min_size:1 ~max_size:10 ()) in
      List.iter (fun x -> assert (x >= 0)) lst)

(** Property: [flat_map] can make a pair (n, list-of-n-integers). Generates an
    integer [n] in [1..5], then generates a list of exactly [n] integers using
    [flat_map]. *)
let test_flat_map_combinator () =
  Hegel.run_hegel_test ~test_cases:50 (fun tc ->
      let pair_gen =
        flat_map
          (fun n ->
            map
              (fun lst -> (n, lst))
              (lists
                 (integers ~min_value:0 ~max_value:99 ())
                 ~min_size:n ~max_size:n ()))
          (integers ~min_value:1 ~max_value:5 ())
      in
      let n, lst = Hegel.draw tc pair_gen in
      assert (List.length lst = n))

(** Property: [sampled_from] always returns one of the specified values. *)
let test_sampled_from () =
  Hegel.run_hegel_test ~test_cases:100 (fun tc ->
      let options = [ 10; 20; 30; 40 ] in
      let v = Hegel.draw tc (sampled_from options) in
      assert (v = 10 || v = 20 || v = 30 || v = 40))

(** Property: hashmaps generated with a min_size have at least that many
    entries. *)
let test_hashmap_size () =
  Hegel.run_hegel_test ~test_cases:50 (fun tc ->
      let pairs =
        Hegel.draw tc
          (hashmaps
             (text ~min_size:1 ~max_size:8 ())
             (integers ~min_value:0 ~max_value:100 ())
             ~min_size:2 ~max_size:6 ())
      in
      assert (List.length pairs >= 2))

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
  test_hashmap_size ();
  Printf.printf "  hashmap_size: OK\n%!";
  Printf.printf "All tests passed.\n%!"
