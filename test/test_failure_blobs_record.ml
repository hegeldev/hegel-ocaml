(** Snapshot tests for the [@@failure_blobs ...] recording and replay flows. *)

let prop tc =
  if Hegel.draw tc (Hegel.Generators.booleans ()) then failwith "deliberate failure"
;;

let settings () = Hegel.settings ~test_cases:50 ~seed:0 ()

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

(* Round-trip: recording mode prints a blob on failure; that exact blob, fed
   back through replay mode, must reproduce the original failure. *)
let%expect_test "recording then replay round-trips the failure blob" =
  (try Hegel.Session.run_hegel_test ~settings:(settings ()) ~failure_blobs:[] prop with
   | _ -> ());
  let recorded = [%expect.output] in
  assert (contains ~needle:"[hegel] To replay the failure" recorded);
  let blob = extract_blob recorded in
  let reproduced =
    try
      Hegel.Session.run_hegel_test ~settings:(settings ()) ~failure_blobs:[ blob ] prop;
      false
    with
    | _ -> true
  in
  assert reproduced;
  let replay_out = [%expect.output] in
  assert (contains ~needle:"The following blobs reproduced an error" replay_out)
;;

(* Replay mode with multiple blobs reports every blob's outcome: a stale
   blob ahead of a valid one must neither be skipped nor mask the valid
   one, and both results must be printed. *)
let%expect_test "replay reports outcome of every blob (stale then valid)" =
  (try Hegel.Session.run_hegel_test ~settings:(settings ()) ~failure_blobs:[] prop with
   | _ -> ());
  let recorded = [%expect.output] in
  let blob = extract_blob recorded in
  (try
     Hegel.Session.run_hegel_test
       ~settings:(settings ())
       ~failure_blobs:[ "AAA="; blob ]
       prop
   with
   | _ -> ());
  let out = [%expect.output] in
  assert (contains ~needle:{|did not reproduce an error: [ "AAA=" ]|} out);
  assert (contains ~needle:(Printf.sprintf "reproduced an error: [ %S ]" blob) out)
;;

(* A reproducing blob does not stop the replay: a later blob is still
   replayed (and reported) after the first one reproduces, exercising
   reuse of the client connection after [run_test] re-raises. *)
let%expect_test "replay continues past a reproducing blob" =
  (try Hegel.Session.run_hegel_test ~settings:(settings ()) ~failure_blobs:[] prop with
   | _ -> ());
  let recorded = [%expect.output] in
  let blob = extract_blob recorded in
  (try
     Hegel.Session.run_hegel_test
       ~settings:(settings ())
       ~failure_blobs:[ blob; "AAA=" ]
       prop
   with
   | _ -> ());
  let out = [%expect.output] in
  assert (contains ~needle:(Printf.sprintf "reproduced an error: [ %S ]" blob) out);
  assert (contains ~needle:{|did not reproduce an error: [ "AAA=" ]|} out)
;;

(* Replay mode where every blob is stale: the run raises a distinctive
   [Failure] (the stale-only branch prints no summary). *)
let%expect_test "replay mode raises when every blob is stale" =
  let outcome =
    try
      Hegel.Session.run_hegel_test
        ~settings:(settings ())
        ~failure_blobs:[ "AAA=" ]
        (fun _tc -> ());
      "unexpectedly succeeded"
    with
    | Failure msg -> Printf.sprintf "Failure(%S)" msg
    | e -> Printexc.to_string e
  in
  print_endline outcome;
  [%expect {| Failure("[hegel] no failure blob reproduced the original failure") |}]
;;
