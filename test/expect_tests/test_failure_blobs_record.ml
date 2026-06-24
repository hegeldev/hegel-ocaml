(** Snapshot tests for the [@@failure_blobs ...] recording and replay flows. *)

let prop tc =
  if Hegel.draw tc (Hegel.Generators.booleans ()) then failwith "deliberate failure"
;;

let settings () =
  Hegel.settings ~test_cases:50 ~seed:0 () |> Hegel.Client.with_print_blob true
;;

let contains ~needle s =
  let nl = String.length needle in
  let sl = String.length s in
  let rec go i =
    i + nl <= sl && (String.equal (String.sub s i nl) needle || go (i + 1))
  in
  nl = 0 || go 0
;;

let extract_blob out =
  let i = String.index out '"' in
  let j = String.index_from out (i + 1) '"' in
  String.sub out (i + 1) (j - i - 1)
;;

let count ~needle s =
  let nl = String.length needle in
  let sl = String.length s in
  let rec go i acc =
    if i + nl > sl
    then acc
    else if String.equal (String.sub s i nl) needle
    then go (i + nl) (acc + 1)
    else go (i + 1) acc
  in
  if nl = 0 then 0 else go 0 0
;;

(* Round-trip: recording mode prints a blob on failure; that exact blob, fed
   back through replay mode, must reproduce the original failure. *)
let%expect_test "recording then replay round-trips the failure blob" =
  (try Hegel.run_hegel_test ~settings:(settings ()) ~failure_blobs:[] prop with
   | _ -> ());
  let recorded = [%expect.output] in
  assert (contains ~needle:"failure blob:" recorded);
  let blob = extract_blob recorded in
  (try Hegel.run_hegel_test ~settings:(settings ()) ~failure_blobs:[ blob ] prop with
   | _ -> ());
  let replay_out = [%expect.output] in
  assert (contains ~needle:"The failure blob reproduced an error" replay_out)
;;

let%hegel_test stale_blob _ = () [@@failure_blobs [ "AAEAAAABAQ==" ]]

let%expect_test "a stale blob does not reproduce an error" =
  try stale_blob () with
  | Failure msg ->
    assert (contains ~needle:"The failure blob did not reproduce an error" msg);
    [%expect {||}]
;;

let%hegel_test invalid_blob = prop [@@failure_blobs [ "INVALID_BLOB" ]]

let%expect_test
    "an invalid blob does not reproduce an error and fails with a clear error message"
  =
  try invalid_blob () with
  | Failure msg ->
    assert (
      contains
        ~needle:
          "the supplied failure blob could not be decoded. It may be corrupt or from an \
           incompatible Hegel version."
        msg);
    [%expect {||}]
;;

let%expect_test "only the first blob is actually replayed" =
  (try Hegel.run_hegel_test ~settings:(settings ()) prop with
   | _ -> ());
  let recorded = [%expect.output] in
  let blob = extract_blob recorded in
  (try
     Hegel.run_hegel_test
       ~settings:(settings ())
       ~failure_blobs:[ blob; "INVALID_BLOB" ]
       prop
   with
   | _ -> ());
  let replay_out = [%expect.output] in
  assert (contains ~needle:"The failure blob reproduced an error" replay_out)
;;

exception A
exception B

let multi_prop tc =
  let v = Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ()) in
  if v >= 60 then raise A;
  if v <= 30 then raise B
;;

let%hegel_test multi_fail_test tc =
  let v = Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ()) in
  if v >= 60 then raise A;
  if v <= 30 then raise B
[@@settings
  Hegel.settings ~test_cases:300 ~seed:9 ()
  |> Hegel.Client.with_print_blob true
  |> Hegel.Client.with_report_multiple_failures true]
;;

let%expect_test "recording groups each failure's draws with its diagnostic" =
  match multi_fail_test () with
  | () -> assert false
  | exception Failure msg ->
    Printf.printf "%s" msg;
    [%expect
      {|
      Failure 1:
      v = 60
      Exception: (Expect_tests.Test_failure_blobs_record.A)
      Failure blob: "AAEAAAAACgEAAAA8"

      Failure 2:
      v = 0
      Exception: (Expect_tests.Test_failure_blobs_record.B)
      Failure blob: "AAEAAAAACgEAAAAA"

      2 failures found!
      |}]
;;

(* With [print_blob] off the per-failure blocks still print, but without the
   trailing blob line. *)
let%expect_test "the multi-failure report omits blobs when print_blob is off" =
  let settings =
    Hegel.settings ~test_cases:300 ~seed:9 ()
    |> Hegel.Client.with_report_multiple_failures true
  in
  (match Hegel.run_hegel_test ~settings multi_prop with
   | () -> assert false
   | exception Failure msg -> Printf.printf "%s" msg);
  [%expect
    {|
    Failure 1:
    draw_1 = 60
    Exception: (Expect_tests.Test_failure_blobs_record.A)

    Failure 2:
    draw_1 = 0
    Exception: (Expect_tests.Test_failure_blobs_record.B)

    2 failures found!
    |}]
;;
