(** Snapshot tests for the [@@blobs ...] recording and replay flows.

    Uses [let%expect_test] so the printed blob can be captured and
    compared against a [%expect {| ... |}] snapshot — no hand-rolled
    stdout redirection needed. If hegel's server is upgraded and the
    shrunk blob changes, accept the new snapshot with [dune promote]. *)

(* Recording mode: an empty [~blobs:[]] argument enables capture. With
   a fixed seed the server's exploration is deterministic, so the
   printed blob is stable. *)
let%expect_test "recording mode prints failure blob on test failure" =
  (try
     Hegel.Session.run_hegel_test
       ~settings:(Hegel.settings ~test_cases:50 ~seed:0 ())
       ~blobs:[]
       (fun tc ->
          let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
          failwith "deliberate failure")
   with
   | _ -> ());
  [%expect
    {|
    [hegel] failure blob(s) recorded:
      - "AAA="
    [hegel] to replay, add to your test: [@@blobs [ "AAA=" ]]
    |}]
;;

(* Replay mode: a non-empty [~blobs:[...]] list re-runs each blob
   through the server. A blob that still reproduces the failure
   re-raises the original exception, with a one-line note on stderr —
   captured here too. *)
let%expect_test "replay mode reproduces the original failure" =
  let outcome =
    try
      Hegel.Session.run_hegel_test
        ~settings:(Hegel.settings ~test_cases:50 ~seed:0 ())
        ~blobs:[ "AAA=" ]
        (fun tc ->
           let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
           failwith "deliberate failure");
      "unexpectedly succeeded"
    with
    | Failure msg -> Printf.sprintf "Failure(%S)" msg
    | e -> Printexc.to_string e
  in
  print_endline outcome;
  [%expect
    {|
    Hegel: failure blob AAA= reproduced the original failure
    Failure("deliberate failure")
    |}]
;;

(* Replay mode with a blob whose failure no longer reproduces: should
   raise a distinctive [Failure] flagging the stale entry, leaving the
   original blob string in the message for diagnosis. *)
let%expect_test "replay mode raises on stale blob" =
  let outcome =
    try
      Hegel.Session.run_hegel_test
        ~settings:(Hegel.settings ~test_cases:50 ~seed:0 ())
        ~blobs:[ "AAA=" ]
        (fun _tc -> ());
      "unexpectedly succeeded"
    with
    | Failure msg -> Printf.sprintf "Failure(%S)" msg
    | e -> Printexc.to_string e
  in
  print_endline outcome;
  [%expect
    {| Failure("Hegel: failure blob AAA= did not reproduce the original failure") |}]
;;
