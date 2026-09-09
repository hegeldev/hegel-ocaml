open Hegel

let int_gen = integers ~min_value:0 ~max_value:100 ()

let run ~show_statistics body =
  let settings =
    Hegel.settings ~test_cases:5 ~seed:0 ()
    |> with_database Disabled
    |> with_show_statistics show_statistics
  in
  Hegel.run_hegel_test ~settings body
;;

let record_events tc =
  let v = draw_silent tc int_gen in
  event tc "always";
  if v >= 50 then event tc "big draw";
  event_value tc 1.0 "obs";
  event_value tc 3.0 "obs"
;;

let%expect_test "no statistics block when show_statistics is off (the default)" =
  run ~show_statistics:false record_events;
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect {| |}]
;;

let%expect_test "statistics aggregate events and numeric observations" =
  run ~show_statistics:true record_events;
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    Statistics (over 5 test cases):
      * always: 100.0% of test cases
      * big draw: 40.0% of test cases
      * obs: count 10, min 1, median 2, mean 2.00, p90 3, max 3
    |}]
;;

let%expect_test "statistics with no events recorded point at event/event_value" =
  run ~show_statistics:true (fun tc -> ignore (draw_silent tc int_gen : int));
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    Statistics (over 5 test cases): no events were recorded; record them with tc.event(..) or tc.event_value(..)
    |}]
;;

let%expect_test "shrink replays are excluded from the statistics" =
  (try
     run ~show_statistics:true (fun tc ->
       let v = draw_silent tc int_gen in
       event_value tc (float_of_int v) "v";
       assert (v < 90))
   with
   | _ -> ());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure ------------------------------------------------------------
    Falsified after 5 test cases (0 discarded):

    Exception: File "ppx/test/expect_tests/test_statistics.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    Statistics (over 5 test cases):
      * v: count 5, min 0, median 50, mean 50.00, p90 90, max 90
    |}]
;;
