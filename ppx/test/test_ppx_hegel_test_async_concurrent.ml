open Core
open Async

module%hegel_concurrent_state_machine [@async] Counter_machine = struct
  type state = { mutable n : int }

  let incr _tc s =
    s.n <- s.n + 1;
    Scheduler.yield ()
  [@@rule { group = "ops"; weight = 2.0 }]
  ;;

  let read _tc _s = Scheduler.yield () [@@rule]

  let non_negative _tc s = Scheduler.yield () >>| fun () -> assert (s.n >= 0)
  [@@invariant { always_check = true }]
  ;;
end

let%hegel_test[@async] runs_machine tc =
  Counter_machine.run tc ~init:{ n = 0 } ~step_count:5 ~max_concurrency:2
[@@settings Hegel.Settings.create ~test_cases:5 ()]
;;

let test_collects_rules_and_invariants () =
  let module R = Hegel_jane_async.Stateful.Concurrent_rule in
  Alcotest.(check (list string))
    "rules"
    [ "incr"; "read" ]
    (List.map Counter_machine.rules ~f:R.name);
  Alcotest.(check (list string))
    "groups"
    [ "ops"; "<anonymous>" ]
    (List.map Counter_machine.rules ~f:R.group);
  Alcotest.(check (list (float 0.)))
    "weights"
    [ 2.0; 1.0 ]
    (List.map Counter_machine.rules ~f:R.weight);
  Alcotest.(check (list string))
    "invariants"
    [ "non_negative" ]
    (List.map Counter_machine.invariants ~f:Hegel_jane_async.Stateful.Invariant.name)
;;

let test_generated_run () = Thread_safe.block_on_async_exn runs_machine

let () =
  Alcotest.run
    "hegel-ppx-hegel-test-async-concurrent"
    [ ( "module%hegel_concurrent_state_machine [@async]"
      , [ Alcotest.test_case
            "collects rules and invariants"
            `Quick
            test_collects_rules_and_invariants
        ; Alcotest.test_case "generated run" `Quick test_generated_run
        ] )
    ]
;;
