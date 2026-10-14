open Hegel
open Generators

(** Test: one_of with empty list raises. *)
let test_one_of_empty () =
  match one_of [] with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail "expected Failure"
;;

(** Test: one_of with a single generator is accepted. *)
let test_one_of_single_accepted () =
  ignore (one_of [ booleans () ] : (bool, printable) generator)
;;

(** Test: sampled_from raises when given an empty list. *)
let test_sampled_from_empty () =
  match sampled_from [] with
  | exception Hegel.Usage_error _ -> ()
  | _ -> Alcotest.fail "expected Usage_error"
;;

(* ==== E2E tests ==== *)

(** Test: one_of with primitive generators works e2e. *)
let test_one_of_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:50 ()) (fun tc ->
    let gen =
      one_of
        [ integers ~min_value:0 ~max_value:10 ()
        ; with_printer Core.Int.sexp_of_t (just 99)
        ]
    in
    let v = Hegel.draw tc gen in
    assert ((v >= 0 && v <= 10) || v = 99))
;;

(** Test: one_of with filtered generators works e2e. *)
let test_one_of_filtered_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:50 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = one_of [ filtered; integers ~min_value:100 ~max_value:200 () ] in
    let v = Hegel.draw tc gen in
    assert ((v > 5 && v <= 10) || (v >= 100 && v <= 200)))
;;

(** Regression test: a value drawn from [one_of] prints through the printer of
    the branch it was actually drawn from, not the first branch's printer. *)
let test_one_of_branch_printer () =
  let saw_one = ref false in
  let saw_two = ref false in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:50 ~seed:0 ())
    (fun tc ->
       let g1 =
         with_printer (fun i -> Core.Sexp.Atom ("one:" ^ string_of_int i)) (just 1)
       in
       let g2 =
         with_printer (fun i -> Core.Sexp.Atom ("two:" ^ string_of_int i)) (just 2)
       in
       let gen = one_of [ g1; g2 ] in
       let v = Hegel.draw tc gen in
       let rendered = Core.Sexp.to_string ((printer gen) v) in
       match v with
       | 1 ->
         saw_one := true;
         Alcotest.(check string) "branch 1 printer" "one:1" rendered
       | 2 ->
         saw_two := true;
         Alcotest.(check string) "branch 2 printer" "two:2" rendered
       | _ -> Alcotest.fail "unexpected value");
  Alcotest.(check bool) "saw branch 1" true !saw_one;
  Alcotest.(check bool) "saw branch 2" true !saw_two
;;

(** Test: optional produces None or Some e2e. *)
let test_optional_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:50 ~seed:0 ())
    (fun tc ->
       let gen = optional (integers ~min_value:1 ~max_value:100 ()) in
       match Hegel.draw tc gen with
       | Some v ->
         saw_some := true;
         assert (v >= 1 && v <= 100)
       | None -> saw_none := true);
  (* At least one of each should have occurred in 50 test cases *)
  Alcotest.(check bool) "saw Some" true !saw_some;
  Alcotest.(check bool) "saw None" true !saw_none
;;

(** Test: ip_addresses generates typed [Ipaddr.t] values of the requested
    version e2e. *)
let test_ip_addresses_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    (match Hegel.draw tc (ip_addresses ~version:`V4 ()) with
     | Ipaddr.V4 _ as ip -> assert (String.contains (Ipaddr.to_string ip) '.')
     | Ipaddr.V6 _ -> Alcotest.fail "expected an IPv4 address");
    match Hegel.draw tc (ip_addresses ~version:`V6 ()) with
    | Ipaddr.V6 _ as ip -> assert (String.contains (Ipaddr.to_string ip) ':')
    | Ipaddr.V4 _ -> Alcotest.fail "expected an IPv6 address")
;;

(** Test: ip_addresses default generates both v4 and v6 e2e. *)
let test_ip_both_e2e () =
  let saw_v4 = ref false in
  let saw_v6 = ref false in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:50 ~seed:0 ())
    (fun tc ->
       match Hegel.draw tc (ip_addresses ()) with
       | Ipaddr.V4 _ -> saw_v4 := true
       | Ipaddr.V6 _ -> saw_v6 := true);
  Alcotest.(check bool) "saw v4" true !saw_v4;
  Alcotest.(check bool) "saw v6" true !saw_v6
;;

(** Test: tuples2 primitive e2e. *)
let test_tuples2_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let gen = tuples2 (integers ~min_value:0 ~max_value:10 ()) (booleans ()) in
    let a, _b = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10))
;;

(** Test: tuples2 composite e2e. *)
let test_tuples2_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen = tuples2 filtered (booleans ()) in
    let a, _b = Hegel.draw tc gen in
    assert (a > 5 && a <= 10))
;;

(** Test: tuples3 primitive e2e. *)
let test_tuples3_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let gen =
      tuples3
        (integers ~min_value:0 ~max_value:10 ())
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
    in
    let a, _b, c = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10);
    assert (c >= 100 && c <= 200))
;;

(** Test: tuples3 composite e2e. *)
let test_tuples3_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen =
      tuples3 filtered (booleans ()) (integers ~min_value:100 ~max_value:200 ())
    in
    let a, _b, c = Hegel.draw tc gen in
    assert (a > 5 && a <= 10);
    assert (c >= 100 && c <= 200))
;;

(** Test: tuples4 primitive e2e. *)
let test_tuples4_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let gen =
      tuples4
        (integers ~min_value:0 ~max_value:10 ())
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
        (floats ~min_value:0.0 ~max_value:1.0 ())
    in
    let a, _b, c, d = Hegel.draw tc gen in
    assert (a >= 0 && a <= 10);
    assert (c >= 100 && c <= 200);
    assert (d >= 0.0 && d <= 1.0))
;;

(** Test: tuples4 composite e2e. *)
let test_tuples4_composite_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let filtered = filter (fun x -> x > 5) (integers ~min_value:0 ~max_value:10 ()) in
    let gen =
      tuples4
        filtered
        (booleans ())
        (integers ~min_value:100 ~max_value:200 ())
        (floats ~min_value:0.0 ~max_value:1.0 ())
    in
    let a, _b, c, d = Hegel.draw tc gen in
    assert (a > 5 && a <= 10);
    assert (c >= 100 && c <= 200);
    assert (d >= 0.0 && d <= 1.0))
;;

let arithmetic =
  {|
?start: sum
?sum: product | sum "+" product
?product: atom | product "*" atom
?atom: NUMBER | "(" sum ")"
%import common.NUMBER
%import common.WS
%ignore WS
|}
;;

let test_from_lark_e2e () =
  let gen = from_lark arithmetic ~max_size:40 ~max_depth:20 () in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let a = Hegel.draw tc gen in
    let b = Hegel.draw tc gen in
    List.iter
      (fun s ->
         assert (String.length s <= 40);
         assert (String.for_all (fun c -> not (Char.equal c '<')) s))
      [ a; b ])
;;

let test_from_lark_start_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:10 ()) (fun tc ->
    let s = Hegel.draw tc (from_lark "start: \"a\" | b\nb: \"b\"\n" ~start:"b" ()) in
    Alcotest.(check string) "rule b" "b" s)
;;

let test_from_lark_explicit_e2e () =
  let gen =
    from_lark
      "start: \"<\" NAME \"\195\169>\"\n%declare NAME\n"
      ~explicit:[ "NAME", from_regex "[a-z]{1,3}" () ]
      ()
  in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
    let s = Hegel.draw tc gen in
    let n = String.length s in
    assert (n >= 5 && n <= 7);
    Alcotest.(check string) "prefix" "<" (String.sub s 0 1);
    Alcotest.(check string) "suffix" "\195\169>" (String.sub s (n - 3) 3);
    assert (String.for_all (fun c -> c >= 'a' && c <= 'z') (String.sub s 1 (n - 4))))
;;

let test_from_lark_explicit_retry_e2e () =
  let gen =
    from_lark
      "start: NAME (\",\" NAME)*\n%declare NAME\n"
      ~max_size:12
      ~explicit:[ "NAME", from_regex "[a-z]{0,10}" () ]
      ()
  in
  let settings =
    { (Hegel.Settings.create ~test_cases:100 ()) with
      suppress_health_check = [ Hegel.Settings.Filter_too_much ]
    }
  in
  Hegel.run_hegel_test ~settings (fun tc ->
    assert (String.length (Hegel.draw tc gen) <= 12))
;;

let test_from_lark_explicit_never_fits () =
  let gen =
    from_lark
      "start: \"x\" NAME\n%declare NAME\n"
      ~max_size:5
      ~explicit:[ "NAME", just (String.make 30 'a') ]
      ()
  in
  let settings =
    { (Hegel.Settings.create ~test_cases:10 ()) with
      suppress_health_check = [ Hegel.Settings.Filter_too_much ]
    }
  in
  Test_helpers.expect_unsatisfiable
    (fun () -> Hegel.run_hegel_test ~settings (fun tc -> ignore (Hegel.draw tc gen)))
    "filtered out by assume()"
;;

let test_from_lark_import_paths_e2e () =
  Test_helpers.with_tempdir ~prefix:"hegel_lark" ~f:(fun dir ->
    let write name text =
      Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
        Out_channel.output_string oc text)
    in
    write "words.lark" "%import .marks.BANG\nWORD: \"hello\" BANG\n";
    write "marks.lark" "BANG: \"!\"\n";
    let gen =
      from_lark
        "%import words.WORD\nstart: WORD\n"
        ~import_paths:[ Filename.concat dir "missing"; dir ]
        ()
    in
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:5 ()) (fun tc ->
      Alcotest.(check string) "imported" "hello!" (Hegel.draw tc gen)))
;;

let test_from_lark_finalised () =
  let run () =
    let gen = from_lark "start: \"a\"\n" () in
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:2 ()) (fun tc ->
      Alcotest.(check string) "a" "a" (Hegel.draw tc gen))
  in
  run ();
  Gc.full_major ()
;;

let test_from_lark_usage_errors () =
  Test_helpers.expect_usage_error (from_lark "start: foo\n" ()) "foo";
  Test_helpers.expect_usage_error (from_lark "start: NAME\n%declare NAME\n" ()) "NAME";
  let raises f substring =
    match f () with
    | exception Hegel.Usage_error msg ->
      assert (Test_helpers.contains_substring msg substring)
    | _ -> Alcotest.fail "expected Usage_error"
  in
  raises (fun () -> from_lark "start: \"a\"" ~max_size:0 ()) "max_size";
  raises (fun () -> from_lark "start: \"a\"" ~max_depth:0 ()) "max_depth";
  raises
    (fun () ->
       from_lark
         "start: A\n%declare A\n"
         ~explicit:[ "A", from_regex "a" (); "A", from_regex "b" () ]
         ())
    "more than one generator"
;;

let tests =
  [ Alcotest.test_case "sampled_from empty" `Quick test_sampled_from_empty
  ; Alcotest.test_case "one_of empty" `Quick test_one_of_empty
  ; Alcotest.test_case "one_of single accepted" `Quick test_one_of_single_accepted
  ; Alcotest.test_case "one_of e2e" `Quick test_one_of_e2e
  ; Alcotest.test_case "one_of filtered e2e" `Quick test_one_of_filtered_e2e
  ; Alcotest.test_case "one_of branch printer" `Quick test_one_of_branch_printer
  ; Alcotest.test_case "optional e2e" `Quick test_optional_e2e
  ; Alcotest.test_case "ip_addresses e2e" `Quick test_ip_addresses_e2e
  ; Alcotest.test_case "ip_addresses both e2e" `Quick test_ip_both_e2e
  ; Alcotest.test_case "tuples2 e2e" `Quick test_tuples2_e2e
  ; Alcotest.test_case "tuples2 composite e2e" `Quick test_tuples2_composite_e2e
  ; Alcotest.test_case "tuples3 e2e" `Quick test_tuples3_e2e
  ; Alcotest.test_case "tuples3 composite e2e" `Quick test_tuples3_composite_e2e
  ; Alcotest.test_case "tuples4 e2e" `Quick test_tuples4_e2e
  ; Alcotest.test_case "tuples4 composite e2e" `Quick test_tuples4_composite_e2e
  ; Alcotest.test_case "from_lark e2e" `Quick test_from_lark_e2e
  ; Alcotest.test_case "from_lark start e2e" `Quick test_from_lark_start_e2e
  ; Alcotest.test_case "from_lark explicit e2e" `Quick test_from_lark_explicit_e2e
  ; Alcotest.test_case
      "from_lark explicit retry e2e"
      `Quick
      test_from_lark_explicit_retry_e2e
  ; Alcotest.test_case
      "from_lark explicit never fits"
      `Quick
      test_from_lark_explicit_never_fits
  ; Alcotest.test_case "from_lark import paths e2e" `Quick test_from_lark_import_paths_e2e
  ; Alcotest.test_case "from_lark finalised" `Quick test_from_lark_finalised
  ; Alcotest.test_case "from_lark usage errors" `Quick test_from_lark_usage_errors
  ]
;;
