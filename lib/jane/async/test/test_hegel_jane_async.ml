open Core
open Async
module H = Hegel_jane_async

let settings = Hegel.Settings.create ~test_cases:20 ~seed:0 ()

(* Each test drives Async from Alcotest's thread, the way a user's test runner
   would. *)
let run_async f = Thread_safe.block_on_async_exn f

let expect_failure f =
  match run_async f with
  | () -> Alcotest.fail "expected a failure"
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Failure msg -> msg
     | exn -> Alcotest.failf "unexpected exception %s" (Exn.to_string exn))
;;

let body_waits_on_its_deferred () =
  let finished = ref 0 in
  run_async (fun () ->
    H.run_hegel_test ~settings (fun tc ->
      ignore (Hegel.draw tc (Hegel.integers ()) : int);
      Scheduler.yield () >>| fun () -> incr finished));
  Alcotest.(check bool) "every body finished" true (!finished > 0)
;;

let failure_after_a_bind () =
  let msg =
    expect_failure (fun () ->
      H.run_hegel_test ~settings (fun tc ->
        let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:100 ()) in
        Scheduler.yield () >>| fun () -> if n >= 10 then failwith "too big"))
  in
  Alcotest.(check string) "message" "too big" msg
;;

let failure_before_any_bind () =
  let msg =
    expect_failure (fun () ->
      H.run_hegel_test ~settings (fun tc ->
        let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:100 ()) in
        if n >= 10 then failwith "too big";
        return ()))
  in
  Alcotest.(check string) "message" "too big" msg
;;

(* A rejected assumption comes back through Async's monitor. If it stayed
   wrapped, hegel would count it as a failure. *)
let assume_after_a_bind () =
  run_async (fun () ->
    H.run_hegel_test ~settings (fun tc ->
      let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:100 ()) in
      Scheduler.yield ()
      >>| fun () ->
      Hegel.assume tc (n mod 2 = 0);
      assert (n mod 2 = 0)))
;;

let body_keeps_the_execution_context () =
  let key = Univ_map.Key.create ~name:"test" Int.sexp_of_t in
  run_async (fun () ->
    Scheduler.with_local key (Some 42) ~f:(fun () ->
      H.run_hegel_test ~settings (fun _tc ->
        Alcotest.(check (option int)) "local" (Some 42) (Scheduler.find_local key);
        return ())))
;;

module Counter = struct
  type t =
    { mutable value : int
    ; mutable calls : int
    ; buggify : bool
    }

  let create ~buggify = Scheduler.yield () >>| fun () -> { value = 0; calls = 0; buggify }

  let incr t =
    Scheduler.yield ()
    >>| fun () ->
    t.calls <- t.calls + 1;
    if not (t.buggify && t.calls = 3) then t.value <- t.value + 1
  ;;

  let read t = Scheduler.yield () >>| fun () -> t.value
end

module Counter_machine = struct
  type state =
    { counter : Counter.t
    ; mutable model : int
    }

  let rules =
    [ H.Stateful.Rule.create
        ~name:"incr"
        ~weight:3.0
        ~step:(fun _tc s ->
          s.model <- s.model + 1;
          Counter.incr s.counter)
        ()
    ; H.Stateful.Rule.create
        ~name:"read_positive"
        ~step:(fun tc s -> Counter.read s.counter >>| fun n -> Hegel.assume tc (n > 0))
        ()
    ]
  ;;

  let invariants =
    [ H.Stateful.Invariant.create
        ~name:"matches_model"
        ~always_check:true
        ~inv:(fun _tc s ->
          Counter.read s.counter
          >>| fun n -> if n <> s.model then failwithf "counter %d, model %d" n s.model ())
        ()
    ; H.Stateful.Invariant.create
        ~name:"non_negative"
        ~inv:(fun _tc s -> Scheduler.yield () >>| fun () -> assert (s.model >= 0))
        ()
    ]
  ;;
end

let run_counter ~buggify () =
  H.run_hegel_test ~settings (fun tc ->
    Counter.create ~buggify
    >>= fun counter ->
    H.Stateful.run
      ~step_count:10
      ~sexp_of_state:(fun (s : Counter_machine.state) -> Int.sexp_of_t s.model)
      tc
      (module Counter_machine)
      ~init:{ counter; model = 0 })
;;

let stateful_passes () = run_async (run_counter ~buggify:false)

let stateful_finds_lost_update () =
  let msg = expect_failure (run_counter ~buggify:true) in
  Alcotest.(check string) "message" "counter 2, model 3" msg
;;

let stateful_accessors () =
  Alcotest.(check (list string))
    "rule names"
    [ "incr"; "read_positive" ]
    (List.map Counter_machine.rules ~f:H.Stateful.Rule.name);
  Alcotest.(check (list (float 0.)))
    "rule weights"
    [ 3.0; 1.0 ]
    (List.map Counter_machine.rules ~f:H.Stateful.Rule.weight);
  Alcotest.(check (list string))
    "invariant names"
    [ "matches_model"; "non_negative" ]
    (List.map Counter_machine.invariants ~f:H.Stateful.Invariant.name)
;;

let () =
  Alcotest.run
    "hegel-jane-async"
    [ ( "run_hegel_test"
      , [ Alcotest.test_case "waits on the body" `Quick body_waits_on_its_deferred
        ; Alcotest.test_case "failure after a bind" `Quick failure_after_a_bind
        ; Alcotest.test_case "failure before any bind" `Quick failure_before_any_bind
        ; Alcotest.test_case "assume after a bind" `Quick assume_after_a_bind
        ; Alcotest.test_case
            "keeps the execution context"
            `Quick
            body_keeps_the_execution_context
        ] )
    ; ( "stateful"
      , [ Alcotest.test_case "passes" `Quick stateful_passes
        ; Alcotest.test_case "finds a lost update" `Quick stateful_finds_lost_update
        ; Alcotest.test_case "accessors" `Quick stateful_accessors
        ] )
    ]
;;
