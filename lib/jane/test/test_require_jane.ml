let sexp_of_int_list = Core.List.sexp_of_t Core.Int.sexp_of_t
let settings () = Hegel.settings ~test_cases:10 () |> Hegel.with_database Disabled
let () = Hegel_jane.set_sexp_diff ()

let%expect_test "require_equal prints a structural sexp_diff when installed" =
  (try
     Hegel.run_hegel_test ~settings:(settings ()) (fun tc ->
       Hegel.require_equal tc sexp_of_int_list [ 1; 2; 3 ] [ 1; 9; 3 ])
   with
   | _ -> ());
  print_string (Expect_tests.Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

      require_equal: values differ (- lhs / + rhs):
       (1    (1
      - 2   + 9
        3)    3)

    Exception: Failure("require_equal: values differ")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;
