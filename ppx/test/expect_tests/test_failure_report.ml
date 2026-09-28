let int_gen = Hegel.integers ~min_value:0 ~max_value:100 ()

let%expect_test "no-draw failure prints a report without drawn values" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Hegel.Settings.create ~test_cases:10 ()) with
           database = Hegel.Settings.Disabled
         }
       (fun _tc -> failwith "boom")
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------
    Exception: Failure("boom")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

let%expect_test "failure report prints the shrunk counterexample" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Hegel.Settings.create ~test_cases:300 ~seed:9 ()) with
           database = Hegel.Settings.Disabled
         }
       (fun tc ->
          let v = Hegel.draw tc int_gen in
          if v >= 60 then failwith "large values are broken")
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    draw_1 = 60

    Exception: Failure("large values are broken")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

(* The failure does not reproduce, so the report has a caveat. *)
let%expect_test "a flaky failure is reported with a caveat" =
  let calls = ref 0 in
  (match
     Hegel.run_hegel_test
       ~settings:
         { (Hegel.Settings.create ~test_cases:10 ~seed:0 ()) with
           database = Hegel.Settings.Disabled
         ; phases = [ Hegel.Settings.Generate ]
         }
       (fun tc ->
          ignore (Hegel.draw tc int_gen : int);
          let call = !calls in
          incr calls;
          if call = 0 then failwith "first call only")
   with
   | () -> print_endline "passed"
   | exception Failure message -> print_endline message);
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------
    Exception: Failure("first call only")
    note: unconfirmed failure: failed 0 of 10 replays after the observed failure — a rare failure, or the environment changed between executions
    first call only
    |}]
;;

(* A fixed explicit position keeps wrapping identical across compilers.
   Continuation lines align under the sexp, including the location prefix. *)
let%expect_test "a multiline drawn value aligns under its name" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Hegel.Settings.create ~test_cases:100 ~seed:0 ()) with
           database = Hegel.Settings.Disabled
         }
       (fun tc ->
          let l =
            Hegel.draw
              tc
              ~label:"l"
              ~loc:
                { Lexing.pos_fname = "draw.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
              (Hegel.lists ~min_size:15 (Hegel.text ~min_size:5 ~max_size:10 ()) ())
          in
          assert (List.length l < 15))
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report ~hide_draw_positions:false [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    l @ draw.ml:<LINE> = (00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000
     00000)

    Exception: File "ppx/test/expect_tests/test_failure_report.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;
