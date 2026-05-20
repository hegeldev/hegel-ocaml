(** Tests for [Hegel_test_runtime]. Covers [register]/[registered]/[run_all]
    directly; covers [test_main] by spawning [runtime_exit_demo.exe] and
    inspecting its exit code. *)

(** Marker substring used to identify the tests registered by this test
    file. Other tests in the same process also register entries (e.g. via
    [let%hegel_test]); we only inspect ours. *)
let marker = "TEST_HEGEL_TEST_RUNTIME_"

let mine =
  List.filter (fun (t : Hegel_test_runtime.test) ->
    String.length t.name >= String.length marker
    && String.sub t.name 0 (String.length marker) = marker)
;;

let test_register_preserves_order () =
  Hegel_test_runtime.register
    ~name:(marker ^ "a")
    ~file:__FILE__
    ~line:__LINE__
    (fun () -> ());
  Hegel_test_runtime.register
    ~name:(marker ^ "b")
    ~file:__FILE__
    ~line:__LINE__
    (fun () -> ());
  let names =
    Hegel_test_runtime.registered ()
    |> mine
    |> List.map (fun (t : Hegel_test_runtime.test) -> t.name)
  in
  Alcotest.(check (list string))
    "registered in source order"
    [ marker ^ "a"; marker ^ "b" ]
    names
;;

(** Redirect stdout to /dev/null for the duration of [f] so that [run_all]'s
    chatty PASS/FAIL output doesn't pollute the test runner's stdout. *)
let with_silenced_stdout f =
  let orig = Unix.dup Unix.stdout in
  let devnull = Unix.openfile "/dev/null" [ O_WRONLY ] 0 in
  Unix.dup2 devnull Unix.stdout;
  Unix.close devnull;
  let restore () =
    Unix.dup2 orig Unix.stdout;
    Unix.close orig
  in
  match f () with
  | v ->
    restore ();
    v
  | exception e ->
    restore ();
    raise e
;;

let test_run_all_counts_failures () =
  (* Register tests with their own marker so we can find them later. *)
  let marker_c = marker ^ "pass_c" in
  let marker_d = marker ^ "fail_d" in
  Hegel_test_runtime.register ~name:marker_c ~file:__FILE__ ~line:__LINE__ (fun () -> ());
  Hegel_test_runtime.register ~name:marker_d ~file:__FILE__ ~line:__LINE__ (fun () ->
    failwith "boom");
  let failures = with_silenced_stdout Hegel_test_runtime.run_all in
  Alcotest.(check bool) "exactly one failure counted" true (failures = 1)
;;

(** Locate the [runtime_exit_demo.exe] binary produced by dune. The test
    executable lives at [_build/default/test/test_hegel.exe], so the demo
    binary sits at a sibling path. *)
let demo_exe () =
  let here = Filename.dirname Sys.executable_name in
  let path = Filename.concat here "runtime_exit_demo/runtime_exit_demo.exe" in
  if Sys.file_exists path
  then path
  else Alcotest.fail ("runtime_exit_demo.exe not found at " ^ path)
;;

let spawn_and_wait exe arg =
  let pid =
    Unix.create_process
      exe
      [| exe; arg |]
      Unix.stdin
      (Unix.openfile "/dev/null" [ O_WRONLY ] 0)
      Unix.stderr
  in
  match Unix.waitpid [] pid with
  | _, WEXITED code -> code
  | _, _ -> Alcotest.fail "runtime_exit_demo did not exit normally"
;;

let test_exit_zero_on_pass () =
  let exe = demo_exe () in
  Alcotest.(check int) "exit 0 on all-pass" 0 (spawn_and_wait exe "pass")
;;

let test_exit_one_on_fail () =
  let exe = demo_exe () in
  Alcotest.(check int) "exit 1 on any-fail" 1 (spawn_and_wait exe "fail")
;;

let tests =
  [ Alcotest.test_case "register preserves order" `Quick test_register_preserves_order
  ; Alcotest.test_case "run_all counts failures" `Quick test_run_all_counts_failures
  ; Alcotest.test_case "exit returns 0 on pass" `Quick test_exit_zero_on_pass
  ; Alcotest.test_case "exit returns 1 on fail" `Quick test_exit_one_on_fail
  ]
;;
