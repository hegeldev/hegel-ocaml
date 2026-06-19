(** Snapshot tests for [Hegel.note]'s verbosity gating: silent under [Quiet],
    final-replay-only under [Normal], every case under [Verbose]/[Debug]. *)

let marker = "NOTE_MARKER"
let int_gen = Hegel.Generators.integers ~min_value:0 ~max_value:100 ()

(** A passing property (5 cases) that notes the marker on every case. *)
let run_passing verbosity =
  let settings =
    Hegel.settings ~test_cases:5 ~seed:1 () |> Hegel.Client.with_verbosity verbosity
  in
  Hegel.run_hegel_test ~settings (fun tc ->
    ignore (Hegel.draw tc int_gen : int);
    Hegel.note tc marker)
;;

(** A property that fails (so the engine performs a final replay) and notes the
    marker on every case. *)
let run_failing verbosity =
  let settings =
    Hegel.settings ~test_cases:5 ~seed:1 () |> Hegel.Client.with_verbosity verbosity
  in
  try
    Hegel.run_hegel_test ~settings (fun tc ->
      Hegel.note tc marker;
      failwith "boom")
  with
  | _ -> ()
;;

let%expect_test "Quiet suppresses notes entirely" =
  run_passing Quiet;
  [%expect {| |}]
;;

let%expect_test "Normal notes only on the final failing replay" =
  run_passing Normal;
  run_failing Normal;
  [%expect {| NOTE_MARKER |}]
;;

let%expect_test "Verbose notes on every case (not just the final replay)" =
  run_passing Verbose;
  [%expect
    {|
    Starting phase: Generate
    draw_1 = 0
    NOTE_MARKER
    Running test case
    draw_1 = 59
    NOTE_MARKER
    Running test case
    draw_1 = 34
    NOTE_MARKER
    Running test case
    draw_1 = 47
    NOTE_MARKER
    Running test case
    draw_1 = 41
    NOTE_MARKER
    Ending phase: Generate
    |}]
;;

let%expect_test "Debug notes on every case (not just the final replay)" =
  run_passing Debug;
  [%expect
    {|
    Starting phase: Generate
    draw_1 = 0
    NOTE_MARKER
    draw_1 = 59
    NOTE_MARKER
    test case #2: status = Valid, choices = 1
    draw_1 = 34
    NOTE_MARKER
    test case #3: status = Valid, choices = 1
    draw_1 = 47
    NOTE_MARKER
    test case #4: status = Valid, choices = 1
    draw_1 = 41
    NOTE_MARKER
    test case #5: status = Valid, choices = 1
    Ending phase: Generate
    Test done. interesting_test_cases=0
    |}]
;;
