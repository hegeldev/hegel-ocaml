(** Snapshot tests for [Hegel.require] and [Hegel.require_equal]: the failure
    messages, the sexp diff printed in the report body, and the caller-derived
    failure origins. Tests call [run_hegel_test] directly with the database
    disabled — see [test_failure_report.ml] for why. *)

let sexp_of_int_list = Core.List.sexp_of_t Core.Int.sexp_of_t
let settings () = Hegel.settings ~test_cases:10 () |> Hegel.with_database Disabled

let%expect_test "require fails with the default message" =
  (try
     Hegel.run_hegel_test ~settings:(settings ()) (fun tc ->
       Hegel.require tc true;
       Hegel.require tc false)
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):
    Exception: Failure("require: condition was false")
    rerun with: [@@failure_blobs "AAAAAAA="]
    |}]
;;

let%expect_test "require fails with a custom message" =
  (try
     Hegel.run_hegel_test ~settings:(settings ()) (fun tc ->
       Hegel.require tc ~msg:"the invariant broke" false)
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):
    Exception: Failure("the invariant broke")
    rerun with: [@@failure_blobs "AAAAAAA="]
    |}]
;;

let%expect_test "require_equal prints a sexp diff of the two values" =
  (try
     Hegel.run_hegel_test ~settings:(settings ()) (fun tc ->
       Hegel.require_equal tc sexp_of_int_list [ 1; 2; 3 ] [ 1; 2; 3 ];
       Hegel.require_equal tc sexp_of_int_list [ 1; 2; 3 ] [ 1; 9; 3 ])
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

      require_equal: values differ (- lhs / + rhs):
       (1    (1
      - 2   + 9
        3)    3)

    Exception: Failure("require_equal: values differ")
    rerun with: [@@failure_blobs "AAAAAAA="]
    |}]
;;

(* Two require calls at distinct source lines produce two distinct failures:
   the origin is derived from the caller's line, not from require's raise site
   inside the library (which would collapse every require failure in a run
   into one origin). *)
let%expect_test "require failures get caller-derived origins" =
  (try
     Hegel.run_hegel_test
       ~settings:
         (Hegel.settings ~test_cases:300 ~seed:9 ()
          |> Hegel.with_database Disabled
          |> Hegel.with_report_multiple_failures true
          |> Hegel.with_print_blob false)
       (fun tc ->
          let v =
            Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
          in
          Hegel.require tc ~msg:"too big" (v < 60);
          Hegel.require tc ~msg:"too small" (v > 30))
   with
   | _ -> ());
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 1 test case (0 discarded):

    Failure 1 of 2:
      draw_1 = 60

    Exception: Failure("too big")

    Failure 2 of 2:
      draw_1 = 0

    Exception: Failure("too small")
    |}]
;;
