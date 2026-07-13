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
