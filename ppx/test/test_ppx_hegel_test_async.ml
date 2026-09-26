open Core
open Async

let run_async f = Thread_safe.block_on_async_exn f

let%hegel_test[@async] async_hegel_test tc =
  let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:9 ()) in
  Scheduler.yield () >>| fun () -> assert (n >= 0 && n <= 9)
[@@settings Hegel.Settings.create ~test_cases:5 ()]
;;

module%hegel_state_machine [@async] Counter_machine = struct
  type state = { mutable n : int }

  let incr _tc s =
    s.n <- s.n + 1;
    Scheduler.yield ()
  [@@rule { weight = 3.0 }]
  ;;

  let non_negative _tc s = Scheduler.yield () >>| fun () -> assert (s.n >= 0)
  [@@invariant { always_check = true }]
  ;;
end

let%hegel_test[@async] runs_machine tc =
  Counter_machine.run tc ~init:{ n = 0 } ~step_count:5
[@@settings Hegel.Settings.create ~test_cases:5 ()]
;;

let test_runs_as_a_deferred () = run_async async_hegel_test

let test_collects_rules_and_invariants () =
  let module S = Hegel_jane_async.Stateful in
  Alcotest.(check (list string))
    "rules"
    [ "incr" ]
    (List.map Counter_machine.rules ~f:S.Rule.name);
  Alcotest.(check (list (float 0.)))
    "weights"
    [ 3.0 ]
    (List.map Counter_machine.rules ~f:S.Rule.weight);
  Alcotest.(check (list string))
    "invariants"
    [ "non_negative" ]
    (List.map Counter_machine.invariants ~f:S.Invariant.name)
;;

let test_generated_run () = run_async runs_machine

let () =
  Alcotest.run
    "hegel-ppx-hegel-test-async"
    [ ( "let%hegel_test [@async]"
      , [ Alcotest.test_case "runs as a Deferred" `Quick test_runs_as_a_deferred ] )
    ; ( "module%hegel_state_machine [@async]"
      , [ Alcotest.test_case
            "collects rules and invariants"
            `Quick
            test_collects_rules_and_invariants
        ; Alcotest.test_case "generated run" `Quick test_generated_run
        ] )
    ]
;;
