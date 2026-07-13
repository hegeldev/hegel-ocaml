let int_gen = Hegel.Generators.integers ~min_value:0 ~max_value:100 ()

let%expect_test "no-draw failure prints a bodyless singular report" =
  (try
     Hegel.run_hegel_test
       ~settings:(Hegel.settings ~test_cases:10 () |> Hegel.with_database Disabled)
       (fun _tc -> failwith "boom")
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):
    Exception: Failure("boom")
    rerun with: [@@failure_blobs "AAAAAAA="]
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
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 3 test cases (0 discarded):

      draw_1 = 60

    Exception: Failure("large values are broken")
    rerun with: [@@failure_blobs "AAEAAAAACgEAAAA8"]
    |}]
;;

(* A value too wide for one line: continuation lines align under the sexp
   (which the pretty-printer breaks knowing it starts after "l = "), not at
   column 0. *)
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
              (Hegel.Generators.lists
                 ~min_size:20
                 (Hegel.Generators.text ~min_size:5 ~max_size:20 ())
                 ())
          in
          assert (List.length l < 20))
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

      l = (00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000
           00000 00000 00000 00000 00000 00000 00000 00000)

    Exception: File "ppx/test/expect_tests/test_failure_report.ml", line 59, characters 10-16: Assertion failed
    rerun with: [@@failure_blobs "AXic7ckxDQAACMTAfgJi2PCvDgTggB+aDleAFLnrI9N3YgAeKxPH"]
    |}]
;;
