open Hegel
open Generators

(** Test: a drawn function is a genuine function within one test case — applying
    it to the same argument twice yields the same result (the per-argument memo
    table makes it stable, which properties like [foldr f] rely on). *)
let test_functions_deterministic_e2e () =
  run_hegel_test ~settings:(settings ~test_cases:50 ()) (fun tc ->
    let f =
      draw_silent tc (functions ~sexp_of_arg:Core.sexp_of_int ~returns:(integers ()) ())
    in
    let a = f 7 in
    let b = f 7 in
    assert (a = b))
;;

(** Test: distinct arguments get independent results (never a pre-chosen domain,
    and results are not forced equal), and every result respects the [returns]
    generator's bounds. Over many cases, [f 0] and [f 1] must differ at least
    once. Re-applying [f 0] also exercises the memo-hit path. *)
let test_functions_independent_e2e () =
  let saw_differ = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ~seed:0 ()) (fun tc ->
    let f =
      draw_silent
        tc
        (functions
           ~sexp_of_arg:Core.sexp_of_int
           ~returns:(integers ~min_value:0 ~max_value:5 ())
           ())
    in
    let a0 = f 0 in
    let a1 = f 1 in
    if a0 <> a1 then saw_differ := true;
    (* memo hit: same argument, same result *)
    assert (f 0 = a0);
    assert (a0 >= 0 && a0 <= 5);
    assert (a1 >= 0 && a1 <= 5));
  Alcotest.(check bool) "distinct args can differ" true !saw_differ
;;

(** Test: [functions2] produces a curried two-argument function. Same arguments
    give the same result (memo hit); over many cases, varying the second
    argument must draw a different result at least once (the second argument is
    part of the key); every result respects the [returns] bounds. *)
let test_functions2_e2e () =
  let saw_differ = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ~seed:0 ()) (fun tc ->
    let f =
      draw_silent
        tc
        (functions2
           ~sexp_of_arg1:Core.sexp_of_int
           ~sexp_of_arg2:Core.sexp_of_bool
           ~returns:(integers ~min_value:0 ~max_value:9 ())
           ())
    in
    let r = f 3 true in
    let r' = f 3 false in
    if r <> r' then saw_differ := true;
    (* memo hit: same arguments, same result *)
    assert (f 3 true = r);
    assert (r >= 0 && r <= 9);
    assert (r' >= 0 && r' <= 9));
  Alcotest.(check bool) "different second arg can differ" true !saw_differ
;;

(** Test: [functions3] produces a curried three-argument function. Results are
    consistent per argument triple (memo hit); over many cases, varying the
    third argument must draw a different result at least once; every result
    respects the [returns] bounds. *)
let test_functions3_e2e () =
  let saw_differ = ref false in
  run_hegel_test ~settings:(settings ~test_cases:100 ~seed:0 ()) (fun tc ->
    let f =
      draw_silent
        tc
        (functions3
           ~sexp_of_arg1:Core.sexp_of_int
           ~sexp_of_arg2:Core.sexp_of_bool
           ~sexp_of_arg3:Core.sexp_of_int
           ~returns:(integers ~min_value:0 ~max_value:9 ())
           ())
    in
    let r = f 1 true 2 in
    let r' = f 1 true 3 in
    if r <> r' then saw_differ := true;
    (* memo hit: same arguments, same result *)
    assert (f 1 true 2 = r);
    assert (r >= 0 && r <= 9);
    assert (r' >= 0 && r' <= 9));
  Alcotest.(check bool) "different third arg can differ" true !saw_differ
;;

let tests =
  [ Alcotest.test_case
      "functions deterministic e2e"
      `Quick
      test_functions_deterministic_e2e
  ; Alcotest.test_case "functions independent e2e" `Quick test_functions_independent_e2e
  ; Alcotest.test_case "functions2 curry e2e" `Quick test_functions2_e2e
  ; Alcotest.test_case "functions3 curry e2e" `Quick test_functions3_e2e
  ]
;;
