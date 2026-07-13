(* Scratch demo: observe hegel-ocaml's failure output for various scenarios. *)

open Hegel.Generators

type point =
  { x : int
  ; y : int
  }
[@@deriving hegel_generator]

(* 1: basic failing property, two draws bound to names *)
let%hegel_test scenario_ints tc =
  let a = Hegel.draw tc (integers ~min_value:0 ~max_value:1000 ()) in
  let b = Hegel.draw tc (integers ~min_value:0 ~max_value:1000 ()) in
  assert (a + b < 100)
;;

(* 2: failing property over a list (shrinking display) *)
let%hegel_test scenario_list tc =
  let l = Hegel.draw tc (lists (integers ~min_value:0 ~max_value:100 ()) ()) in
  let sorted = List.sort compare l in
  assert (List.length sorted < 3 || List.hd sorted > 0 || List.length l < 5)
;;

(* 3: note + assume *)
let%hegel_test scenario_note tc =
  let n = Hegel.draw tc (integers ~min_value:(-100) ~max_value:100 ()) in
  Hegel.assume tc (n <> 0);
  Hegel.note tc (Printf.sprintf "computed: n*n = %d" (n * n));
  assert (n * n < 500)
;;

(* 4: derived (unprintable) generator drawn silently *)
let%hegel_test scenario_derived tc =
  let p = Hegel.draw_silent tc point_generator in
  assert (p.x + p.y < 100)
;;

(* 5: exception raised (not assert) *)
let%hegel_test scenario_exn tc =
  let s = Hegel.draw tc (text ~min_size:0 ~max_size:10 ()) in
  if String.length s > 3 then failwith "string too long for my liking"
;;

(* 6: equality-style failure: compare two values *)
let%hegel_test scenario_eq tc =
  let l = Hegel.draw tc (lists (integers ~min_value:(-50) ~max_value:50 ()) ()) in
  let f x = x * 2 in
  (* wrong claim: map then rev = rev then... same thing, so make it actually wrong *)
  let lhs = List.rev_map f l in
  let rhs = List.map f l in
  assert (lhs = rhs)
;;

(* 7: print_blob on *)
let scenario_blob () =
  Hegel.run_hegel_test
    ~settings:(Hegel.default_settings () |> Hegel.with_print_blob true)
    (fun tc ->
       let a = Hegel.draw tc (integers ~min_value:0 ~max_value:1000 ()) in
       assert (a < 500))
;;

(* 8: multiple failures *)
let scenario_multi () =
  Hegel.run_hegel_test
    ~settings:(Hegel.default_settings () |> Hegel.with_report_multiple_failures true)
    (fun tc ->
       let a = Hegel.draw tc (integers ~min_value:0 ~max_value:1000 ()) in
       if a > 900 then failwith "bug one";
       if a > 0 && a mod 7 = 0 then assert false)
;;

(* 9: verbose verbosity *)
let scenario_verbose () =
  Hegel.run_hegel_test
    ~settings:
      (Hegel.default_settings ()
       |> Hegel.with_verbosity Hegel.Verbose
       |> Hegel.with_test_cases 5)
    (fun tc ->
       let a = Hegel.draw tc (integers ~min_value:0 ~max_value:10 ()) in
       ignore a)
;;

(* 10: tuple + long list rendering *)
let%hegel_test scenario_tuple tc =
  let t =
    Hegel.draw
      tc
      (tuples3
         (integers ~min_value:0 ~max_value:1000 ())
         (text ~min_size:0 ~max_size:30 ())
         (lists (floats ~allow_nan:false ~allow_infinity:false ()) ()))
  in
  let a, _, _ = t in
  assert (a < 900)
;;

(* 11: long multiline value *)
let%hegel_test scenario_long tc =
  let l = Hegel.draw tc (lists ~min_size:20 (text ~min_size:5 ~max_size:20 ()) ()) in
  assert (List.length l < 20)
;;

(* 12: hashmap rendering *)
let%hegel_test scenario_map tc =
  let m =
    Hegel.draw tc (assoc_lists (integers ~min_value:0 ~max_value:100 ()) (booleans ()) ())
  in
  assert (List.length m < 2)
;;

(* 13: stateful test failure — counter with a buggy "double" rule *)
let scenario_stateful () =
  Hegel.run_hegel_test (fun tc ->
    let add =
      Hegel.Stateful.Rule.create ~name:"add" ~step:(fun tc state ->
        let n = Hegel.draw tc ~label:"n" (integers ~min_value:1 ~max_value:10 ()) in
        state + n)
    in
    let double =
      Hegel.Stateful.Rule.create ~name:"double" ~step:(fun _tc state -> state * 2)
    in
    let reset = Hegel.Stateful.Rule.create ~name:"reset" ~step:(fun _tc _state -> 0) in
    Hegel.Stateful.run
      ~init:0
      ~rules:[ add; double; reset ]
      ~invariants:[ (fun state -> assert (state < 25)) ]
      tc)
;;

let scenarios =
  [ "ints", scenario_ints
  ; "list", scenario_list
  ; "note", scenario_note
  ; "derived", scenario_derived
  ; "exn", scenario_exn
  ; "eq", scenario_eq
  ; "blob", scenario_blob
  ; "multi", scenario_multi
  ; "verbose", scenario_verbose
  ; "tuple", scenario_tuple
  ; "long", scenario_long
  ; "map", scenario_map
  ; "stateful", scenario_stateful
  ]
;;

let () =
  let name = Sys.argv.(1) in
  match List.assoc_opt name scenarios with
  | None ->
    Printf.eprintf "unknown scenario %s\n" name;
    exit 2
  | Some f ->
    (try
       f ();
       Printf.printf "== scenario %s passed ==\n%!" name
     with
     | e -> Printf.printf "== scenario %s raised ==\n%s\n%!" name (Printexc.to_string e))
;;
