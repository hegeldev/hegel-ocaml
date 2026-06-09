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

(* Two distinct failing assertions in one run exercise the multiple-failures
   path: recording prints a blob per distinct failure and raises an aggregated
   report. *)

exception A
exception B

let%hegel_test multi_fail_test tc =
  let int_gen = Hegel.Generators.integers ~min_value:0 ~max_value:100 () in
  let v = Hegel.draw tc int_gen in
  if v >= 60 then raise A;
  if v <= 30 then raise B
[@@settings
  Hegel.settings ~test_cases:300 ~seed:9 ()
  |> Hegel.Client.with_print_blob true
  |> Hegel.Client.with_report_multiple_failures true]
;;

let%expect_test "recording reports multiple distinct failures" =
  match multi_fail_test () with
  | () -> assert false
  | exception Failure msg ->
    assert (contains ~needle:"Multiple failures (2)" msg);
    assert (count ~needle:"failure blob:" msg = 2);
  [%expect {|
    v = 60
    v = 0
    |}]
;;
