open! Core
open Hegel

(* Deterministic, database-disabled run so the failure blob is stable; swallow
   the failure so the expect block only sees the report. *)
let run_failing body =
  let settings =
    { (Settings.create ~test_cases:20 ~seed:0 ()) with
      verbosity = Settings.Normal
    ; database = Settings.Disabled
    }
  in
  try Hegel.run_hegel_test ~settings body with
  | _ -> ()
;;

(* A state machine module: [inc] and [my_inv] are collected into its [rules]
   and [invariants], and the derived [sexp_of_state] traces the state. *)
module%hegel_state_machine Counter = struct
  type state = int Atomic.t

  let sexp_of_state n = sexp_of_int (Atomic.get n)
  let inc _tc (n : int Atomic.t) = Atomic.incr n [@@rule]

  let my_inv tc (n : int Atomic.t) =
    Hegel.note tc (sprintf "checking n = %d" (Atomic.get n));
    assert (Atomic.get n <= 1)
  [@@invariant always_check]
  ;;
end

let%expect_test "state trace; invariant marks the failing step" =
  run_failing (fun tc -> Counter.run tc ~init:(Atomic.make 0));
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    state = 0
    Checking invariants on the initial state.
      checking n = 0
    Step 1: inc
    state = 1
      checking n = 1
    Step 2: inc
    state = 2
      checking n = 2
    Invariant my_inv violated after step 2.

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

let%expect_test "invariant violated in the initial state" =
  let module M = struct
    type state = unit

    let rules = [ Stateful.Rule.create ~name:"noop" ~step:(fun _tc () -> ()) () ]

    let invariants =
      [ Stateful.Invariant.create ~name:"silly_inv" ~inv:(fun _tc () -> assert false) () ]
    ;;
  end
  in
  run_failing (fun tc -> Stateful.run tc (module M) ~init:());
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    Checking invariants on the initial state.
    Invariant silly_inv violated in the initial state.

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

module%hegel_state_machine Stack = struct
  type state = int list Atomic.t

  let sexp_of_state stack = [%sexp_of: int list] (Atomic.get stack)

  let push tc (stack : int list Atomic.t) =
    let n = Hegel.draw tc (integers ~min_value:0 ~max_value:100 ()) in
    Atomic.set stack (n :: Atomic.get stack)
  [@@rule]
  ;;

  let pop tc (stack : int list Atomic.t) =
    match Atomic.get stack with
    | [] -> Hegel.assume tc false
    | top :: rest ->
      assert (top < 50);
      Atomic.set stack rest
  [@@rule]
  ;;
end

let%expect_test "state trace across multiple rules" =
  run_failing (fun tc -> Stack.run tc ~init:(Atomic.make []));
  print_string (Expect_scrub.scrub_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    state = ()
    Step 1: push
      n = 50
    state = (50)
    Step 2: pop

    Exception: File "ppx/test/expect_tests/test_stateful_trace.ml", line LINE, characters C1-C2: Assertion failed
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

module%hegel_state_machine Concurrent_boom = struct
  type state = unit

  let boom tc () =
    ignore (Hegel.draw tc (integers ()) : int);
    failwith "concurrent boom"
  [@@rule]
  ;;
end

let%expect_test "nondeterministic failure reports the discovering execution" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Settings.create ~test_cases:20 ~seed:0 ()) with
           database = Settings.Disabled
         ; verbosity = Settings.Normal
         }
       (fun tc ->
          Hegel.note tc "preamble";
          Concurrent_boom.run
            tc
            ~step_count:5
            ~init:()
            ~min_concurrency:2
            ~max_concurrency:2)
   with
   | Failure message as exn ->
     if String.equal message "concurrent boom" then () else raise exn
   | exn -> raise exn);
  print_string (Expect_scrub.scrub_concurrent_report [%expect.output]);
  [%expect
    {|
    Concurrent state machine detected: this run is nondeterministic, so failures are reported from the execution that discovered them, without shrinking, replay, database persistence, or a reproduce blob.
    --- Failure --------------------------------------------------------------------

    preamble
    Concurrency level: 2
    ---------------- Round 1: group "<anonymous>" ----------------
    [worker 0 +time] Rule: boom
    [worker 0 +time]   draw_1 = 3881432

    Exception: Failure("concurrent boom")
    |}]
;;

let%expect_test "one concurrent worker remains deterministic" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Settings.create ~test_cases:20 ~seed:0 ()) with
           database = Settings.Disabled
         ; verbosity = Settings.Normal
         }
       (fun tc ->
          Concurrent_boom.run
            tc
            ~step_count:5
            ~init:()
            ~min_concurrency:1
            ~max_concurrency:1)
   with
   | Failure message as exn ->
     if String.equal message "concurrent boom" then () else raise exn
   | exn -> raise exn);
  print_string (Expect_scrub.scrub_concurrent_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    Step 1: boom
      draw_1 = 0

    Exception: Failure("concurrent boom")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;

let%expect_test "quiet nondeterministic failure stays quiet" =
  let raised = ref false in
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Settings.create ~test_cases:20 ~seed:0 ()) with
           database = Settings.Disabled
         ; verbosity = Settings.Quiet
         }
       (fun tc ->
          Concurrent_boom.run
            tc
            ~step_count:5
            ~init:()
            ~min_concurrency:1
            ~max_concurrency:4)
   with
   | Failure message as exn ->
     if String.equal message "concurrent boom" then raised := true else raise exn
   | exn -> raise exn);
  let output = [%expect.output] in
  if not !raised then failwith "expected concurrent failure";
  print_string (Expect_scrub.scrub_concurrent_report output);
  [%expect {||}]
;;

module%hegel_state_machine Concurrent_counter = struct
  type state = int Atomic.t

  let sexp_of_state state = Sexplib0.Sexp.Atom (Int.to_string (Atomic.get state))
  let increment _tc state = Atomic.incr state [@@rule "writes"]

  let stays_zero _tc state = if Atomic.get state <> 0 then failwith "invariant boom"
  [@@invariant always_check]
  ;;
end

let%expect_test "concurrent invariant failures retain their names" =
  (try
     Hegel.run_hegel_test
       ~settings:
         { (Settings.create ~test_cases:1 ~seed:0 ()) with database = Settings.Disabled }
       (fun tc ->
          Concurrent_counter.run
            tc
            ~init:(Atomic.make 0)
            ~min_concurrency:1
            ~max_concurrency:1)
   with
   | Failure message when String.equal message "invariant boom" -> ());
  print_string (Expect_scrub.scrub_concurrent_report [%expect.output]);
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    state = 0
    Checking invariants on the initial state.
    Step 1: increment
    state = 1
    Invariant stays_zero violated after step 1.

    Exception: Failure("invariant boom")
    rerun with: ~failure_blobs:[ "<BLOB>" ]
    |}]
;;
