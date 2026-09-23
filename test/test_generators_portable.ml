open Hegel
open Generators
open Generators.Private

let settings =
  { (Settings.create ~test_cases:5 ~seed:0 ()) with database = Settings.Disabled }
;;

let collect gen =
  let seen = ref [] in
  Hegel.run_hegel_test ~settings (fun tc -> seen := Hegel.draw_silent tc gen :: !seen);
  List.rev !seen
;;

let silent name default portable =
  Alcotest.(check bool)
    (name ^ ": same label")
    true
    (Int64.equal (label_of default) (label_of portable));
  Alcotest.(check bool) (name ^ ": same draws") true (collect default = collect portable)
;;

let check_printed name default portable =
  silent name default portable;
  let render p vs = List.map (fun v -> Sexplib0.Sexp.to_string ((printer p) v)) vs in
  let drawn = collect default in
  Alcotest.(check (list string))
    (name ^ ": same printing")
    (render default drawn)
    (render portable drawn)
;;

let ints () = integers ~min_value:0 ~max_value:9 ()

let test_map () =
  let f x = x * 2 in
  silent "map" (map f (ints ())) ((map [@mode portable]) f (ints ()));
  (* a non-[Leaf] source takes the [Mapped] branch rather than composing the
     draw closure in place *)
  let fst_of (a, _) = a in
  silent
    "map over composite"
    (map fst_of (tuples2 (ints ()) (booleans ())))
    ((map [@mode portable]) fst_of ((tuples2 [@mode portable]) (ints ()) (booleans ())))
;;

let test_flat_map () =
  let f n = integers ~min_value:0 ~max_value:(abs n + 1) () in
  silent "flat_map" (flat_map f (ints ())) ((flat_map [@mode portable]) f (ints ()))
;;

let test_filter () =
  let even n = n mod 2 = 0 in
  check_printed
    "filter"
    (filter even (ints ()))
    ((filter [@mode portable]) even (ints ()));
  (* filtering an unprintable generator takes the other branch *)
  silent
    "filter unprintable"
    (filter even (map (fun x -> x + 1) (ints ())))
    ((filter [@mode portable]) even ((map [@mode portable]) (fun x -> x + 1) (ints ())))
;;

let test_composite () =
  let build tc = Hegel.draw_silent tc (integers ~min_value:0 ~max_value:3 ()) in
  silent "composite" (composite build) ((composite [@mode portable]) build)
;;

let test_with_printer () =
  let sexp_of = Sexplib0.Sexp_conv.sexp_of_int in
  check_printed
    "with_printer"
    (with_printer sexp_of (map (fun x -> x + 1) (ints ())))
    ((with_printer [@mode portable])
       sexp_of
       ((map [@mode portable]) (fun x -> x + 1) (ints ())))
;;

let test_just () = silent "just" (just 42) ((just [@mode portable]) 42)

let test_sampled_from () =
  let options = [ 1; 2; 3 ] in
  silent "sampled_from" (sampled_from options) ((sampled_from [@mode portable]) options);
  (* an empty list is a usage error on both sides *)
  let raises f =
    try
      ignore (f () : (int, unprintable) generator);
      false
    with
    | Hegel.Usage_error _ -> true
  in
  Alcotest.(check bool)
    "sampled_from []: default raises"
    true
    (raises (fun () -> sampled_from []));
  Alcotest.(check bool)
    "sampled_from []: portable raises"
    true
    (raises (fun () -> (sampled_from [@mode portable]) []))
;;

let test_one_of () =
  let branches () = [ ints (); integers ~min_value:9 ~max_value:9 () ] in
  check_printed "one_of" (one_of (branches ())) ((one_of [@mode portable]) (branches ()));
  let raises f =
    try
      ignore (f () : (int, printable) generator);
      false
    with
    | Failure _ -> true
  in
  Alcotest.(check bool) "one_of []: default raises" true (raises (fun () -> one_of []));
  Alcotest.(check bool)
    "one_of []: portable raises"
    true
    (raises (fun () -> (one_of [@mode portable]) []))
;;

let test_optional () =
  check_printed "optional" (optional (ints ())) ((optional [@mode portable]) (ints ()))
;;

let test_tuples () =
  let a () = integers ~min_value:0 ~max_value:3 () in
  let b () = booleans () in
  let c () = chars () in
  let d () = binary ~max_size:2 () in
  check_printed
    "tuples2"
    (tuples2 (a ()) (b ()))
    ((tuples2 [@mode portable]) (a ()) (b ()));
  check_printed
    "tuples3"
    (tuples3 (a ()) (b ()) (c ()))
    ((tuples3 [@mode portable]) (a ()) (b ()) (c ()));
  check_printed
    "tuples4"
    (tuples4 (a ()) (b ()) (c ()) (d ()))
    ((tuples4 [@mode portable]) (a ()) (b ()) (c ()) (d ()))
;;

let test_lists () =
  let elements () = integers ~min_value:0 ~max_value:5 () in
  check_printed
    "lists"
    (lists (elements ()) ~max_size:3 ())
    ((lists [@mode portable]) (elements ()) ~max_size:3 ());
  check_printed
    "lists ~unique"
    (lists (elements ()) ~max_size:3 ~unique:true ())
    ((lists [@mode portable]) (elements ()) ~max_size:3 ~unique:true ())
;;

let test_assoc_lists () =
  let keys () = integers ~min_value:0 ~max_value:5 () in
  let values () = booleans () in
  check_printed
    "assoc_lists"
    (assoc_lists (keys ()) (values ()) ~max_size:3 ())
    ((assoc_lists [@mode portable]) (keys ()) (values ()) ~max_size:3 ())
;;

let test_hash_tables () =
  let keys () = integers ~min_value:0 ~max_value:5 () in
  let values () = booleans () in
  (* [Stdlib.Hashtbl.t] holds no order, so compare the pairs each table holds. *)
  let to_pairs t =
    Stdlib.Hashtbl.fold (fun k v acc -> (k, v) :: acc) t [] |> List.sort compare
  in
  let default = hash_tables (keys ()) (values ()) ~max_size:3 () in
  let portable = (hash_tables [@mode portable]) (keys ()) (values ()) ~max_size:3 () in
  Alcotest.(check bool)
    "hash_tables: same label"
    true
    (Int64.equal (label_of default) (label_of portable));
  let drawn = collect default in
  Alcotest.(check bool)
    "hash_tables: same draws"
    true
    (List.map to_pairs drawn = List.map to_pairs (collect portable));
  (* apply both printers so each twin's [sexp_of_t] runs *)
  let render p t = Sexplib0.Sexp.to_string ((printer p) t) in
  Alcotest.(check (list string))
    "hash_tables: same printing"
    (List.map (render default) drawn)
    (List.map (render portable) drawn);
  (* the table-agnostic form, over a default association list *)
  let of_pairs pairs = pairs in
  let sexp_of_t pk pv pairs =
    Sexplib0.Sexp.List (List.map (fun (k, v) -> Sexplib0.Sexp.List [ pk k; pv v ]) pairs)
  in
  check_printed
    "make_hash_tables"
    (make_hash_tables ~of_pairs ~sexp_of_t (keys ()) (values ()) ~max_size:3 ())
    ((make_hash_tables [@mode portable])
       ~of_pairs
       ~sexp_of_t
       (keys ())
       (values ())
       ~max_size:3
       ())
;;

let test_make_builders () =
  let sexp_of_string = Sexplib0.Sexp_conv.sexp_of_string in
  let sexp_of_char = Sexplib0.Sexp_conv.sexp_of_char in
  check_printed
    "make_characters"
    (make_characters ~of_char:Fun.id ~sexp_of:sexp_of_char ())
    ((make_characters [@mode portable]) ~of_char:Fun.id ~sexp_of:sexp_of_char ());
  let of_date (d : date) = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day in
  check_printed
    "make_dates"
    (make_dates ~of_date ~sexp_of:sexp_of_string ())
    ((make_dates [@mode portable]) ~of_date ~sexp_of:sexp_of_string ());
  let of_time (t : time) = Printf.sprintf "%02d:%02d" t.hour t.minute in
  check_printed
    "make_times"
    (make_times ~of_time ~sexp_of:sexp_of_string ())
    ((make_times [@mode portable]) ~of_time ~sexp_of:sexp_of_string ());
  let of_datetime ((d : date), (t : time)) =
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d" d.year d.month d.day t.hour t.minute
  in
  check_printed
    "make_datetimes"
    (make_datetimes ~of_datetime ~sexp_of:sexp_of_string ())
    ((make_datetimes [@mode portable]) ~of_datetime ~sexp_of:sexp_of_string ())
;;

let test_functions () =
  let returns () = integers ~min_value:0 ~max_value:9 () in
  let sexp_of = Sexplib0.Sexp_conv.sexp_of_int in
  (* a drawn function is not comparable, so compare what it returns *)
  let applied1 gen =
    let seen = ref [] in
    Hegel.run_hegel_test ~settings (fun tc ->
      let f = Hegel.draw_silent tc gen in
      seen := f 2 :: f 1 :: !seen);
    List.rev !seen
  in
  let applied2 gen =
    let seen = ref [] in
    Hegel.run_hegel_test ~settings (fun tc ->
      let f = Hegel.draw_silent tc gen in
      seen := f 1 2 :: !seen);
    List.rev !seen
  in
  let applied3 gen =
    let seen = ref [] in
    Hegel.run_hegel_test ~settings (fun tc ->
      let f = Hegel.draw_silent tc gen in
      seen := f 1 2 3 :: !seen);
    List.rev !seen
  in
  let pair name default portable applied =
    Alcotest.(check bool)
      (name ^ ": same label")
      true
      (Int64.equal (label_of default) (label_of portable));
    Alcotest.(check (list int))
      (name ^ ": same results")
      (applied default)
      (applied portable)
  in
  (* with a printer for the argument, and without: [sexp_or] has a branch each *)
  pair
    "functions"
    (functions ~sexp_of_arg:sexp_of ~returns:(returns ()) ())
    ((functions [@mode portable]) ~sexp_of_arg:sexp_of ~returns:(returns ()) ())
    applied1;
  pair
    "functions (opaque arg)"
    (functions ~returns:(returns ()) ())
    ((functions [@mode portable]) ~returns:(returns ()) ())
    applied1;
  pair
    "functions2"
    (functions2 ~sexp_of_arg1:sexp_of ~sexp_of_arg2:sexp_of ~returns:(returns ()) ())
    ((functions2 [@mode portable])
       ~sexp_of_arg1:sexp_of
       ~sexp_of_arg2:sexp_of
       ~returns:(returns ())
       ())
    applied2;
  pair
    "functions3"
    (functions3
       ~sexp_of_arg1:sexp_of
       ~sexp_of_arg2:sexp_of
       ~sexp_of_arg3:sexp_of
       ~returns:(returns ())
       ())
    ((functions3 [@mode portable])
       ~sexp_of_arg1:sexp_of
       ~sexp_of_arg2:sexp_of
       ~sexp_of_arg3:sexp_of
       ~returns:(returns ())
       ())
    applied3
;;

let tests =
  [ Alcotest.test_case "portable: map" `Quick test_map
  ; Alcotest.test_case "portable: flat_map" `Quick test_flat_map
  ; Alcotest.test_case "portable: filter" `Quick test_filter
  ; Alcotest.test_case "portable: composite" `Quick test_composite
  ; Alcotest.test_case "portable: with_printer" `Quick test_with_printer
  ; Alcotest.test_case "portable: just" `Quick test_just
  ; Alcotest.test_case "portable: sampled_from" `Quick test_sampled_from
  ; Alcotest.test_case "portable: one_of" `Quick test_one_of
  ; Alcotest.test_case "portable: optional" `Quick test_optional
  ; Alcotest.test_case "portable: tuples" `Quick test_tuples
  ; Alcotest.test_case "portable: lists" `Quick test_lists
  ; Alcotest.test_case "portable: assoc_lists" `Quick test_assoc_lists
  ; Alcotest.test_case "portable: hash_tables" `Quick test_hash_tables
  ; Alcotest.test_case "portable: make_* builders" `Quick test_make_builders
  ; Alcotest.test_case "portable: functions" `Quick test_functions
  ]
;;
