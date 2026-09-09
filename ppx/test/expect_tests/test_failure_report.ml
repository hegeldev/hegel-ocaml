let int_gen = Hegel.integers ~min_value:0 ~max_value:100 ()

let%expect_test "no-draw failure prints a bodyless singular report" =
  (try
     Hegel.run_hegel_test
       ~settings:(Hegel.settings ~test_cases:10 () |> Hegel.with_database Disabled)
       (fun _tc -> failwith "boom")
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):
    Exception: Failure("boom")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

let%expect_test "later falsification counts plural test cases" =
  (try
     Hegel.run_hegel_test
       ~settings:
         (Hegel.settings ~test_cases:300 ~seed:9 () |> Hegel.with_database Disabled)
       (fun tc ->
          let v = Hegel.draw tc int_gen in
          if v >= 60 then failwith "large values are broken")
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 4 test cases (0 discarded):

      draw_1 = 60

    Exception: Failure("large values are broken")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

(* A fixed explicit position keeps wrapping identical across compilers.
   Continuation lines align under the sexp, including the location prefix. *)
let%expect_test "a multiline drawn value aligns under its name" =
  (try
     Hegel.run_hegel_test
       ~settings:
         (Hegel.settings ~test_cases:100 ~seed:0 () |> Hegel.with_database Disabled)
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
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

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
