module S = Hegel.Stateful

module Counter = struct
  type state = int Atomic.t

  let rules =
    [ S.Concurrent_rule.create
        ~name:"increment"
        ~step:(fun _tc (n : int Atomic.t) -> Atomic.incr n)
        ()
    ; S.Concurrent_rule.create
        ~name:"decrement"
        ~step:(fun tc (n : int Atomic.t) ->
          Hegel.assume tc (Atomic.get n > 0);
          Atomic.decr n)
        ()
    ]
  ;;

  let invariants =
    [ S.Invariant.create
        ~name:"nonnegative"
        ~inv:(fun _tc n -> assert (Atomic.get n >= 0))
        ()
    ]
  ;;
end

let settings =
  { (Hegel.Settings.create ~test_cases:5 ~seed:0 ()) with
    database = Hegel.Settings.Disabled
  }
;;

let in_thread_test () =
  let steps = Atomic.make 0 in
  Hegel.run_hegel_test ~settings (fun tc ->
    Concurrent_in_thread.with_blocking Await.Terminator.unkillable ~f:(fun concurrent ->
      (S.run_concurrent
         ~concurrency:(Hegel_jane_concurrent.of_concurrent concurrent)
         tc
         (module struct
           include Counter

           let rules =
             S.Concurrent_rule.create
               ~name:"count"
               ~step:(fun _tc _n -> Atomic.incr steps)
               ()
             :: rules
           ;;
         end)
         ~step_count:10
         ~init:(Atomic.make 0)
         ~min_concurrency:4
         ~max_concurrency:4 [@nontail])));
  Alcotest.(check bool) "rules ran" true (Atomic.get steps > 0)
;;

let parallel_scheduler_test () =
  let scheduler = Parallel_scheduler.scheduler () in
  Hegel.run_hegel_test ~settings (fun tc ->
    Parallel_scheduler.parallel scheduler (fun _ctx concurrent ->
      (S.run_concurrent
         ~concurrency:(Hegel_jane_concurrent.of_concurrent concurrent)
         tc
         (module Counter)
         ~step_count:10
         ~init:(Atomic.make 0)
         ~min_concurrency:4
         ~max_concurrency:4 [@nontail])))
;;

exception Boom of int

let worker_exception_test () =
  let module M = struct
    type state = unit

    let rules =
      [ S.Concurrent_rule.create
          ~name:"boom"
          ~step:(fun tc () -> raise (Boom (Hegel.draw tc (Hegel.integers ()))))
          ()
      ]
    ;;

    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test
      ~settings:{ settings with verbosity = Hegel.Settings.Quiet }
      (fun tc ->
         Concurrent_in_thread.with_blocking
           Await.Terminator.unkillable
           ~f:(fun concurrent ->
             (S.run_concurrent
                ~concurrency:(Hegel_jane_concurrent.of_concurrent concurrent)
                tc
                (module M)
                ~step_count:5
                ~init:()
                ~min_concurrency:2
                ~max_concurrency:2 [@nontail])))
  with
  | () -> Alcotest.fail "expected Boom"
  | exception Boom _ -> ()
;;

let () =
  Alcotest.run
    "hegel-jane-concurrent"
    [ ( "hegel_jane_concurrent"
      , [ Alcotest.test_case
            "of_concurrent runs on Concurrent_in_thread"
            `Quick
            in_thread_test
        ; Alcotest.test_case
            "of_concurrent runs on a Parallel scheduler"
            `Quick
            parallel_scheduler_test
        ; Alcotest.test_case
            "worker exceptions reach the caller"
            `Quick
            worker_exception_test
        ] )
    ]
;;
