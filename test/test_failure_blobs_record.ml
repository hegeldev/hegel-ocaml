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
  assert (contains ~needle:"reproduced the original failure" replay_out)
;;

(* Replay mode with a blob whose failure no longer reproduces: should
   raise a distinctive [Failure] flagging the stale entry, leaving the
   original blob string in the message for diagnosis. *)
let%expect_test "replay mode raises on stale blob" =
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
  [%expect
    {| Failure("[hegel] failure blob AAA= did not reproduce the original failure") |}]
;;
