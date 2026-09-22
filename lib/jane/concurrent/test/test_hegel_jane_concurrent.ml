module S = Hegel.Stateful
module J = Hegel_jane_concurrent

let rec try_decrement n =
  let v = Atomic.get n in
  v > 0 && (Atomic.compare_and_set n v (v - 1) || try_decrement n)
;;

let rules () =
  [ S.Concurrent_rule.create
      ~name:"increment"
      ~step:(fun _tc (_ : _ J.ctx) (n : int Atomic.t) -> Atomic.incr n)
      ()
  ; S.Concurrent_rule.create
      ~name:"decrement"
      ~step:(fun tc (_ : _ J.ctx) (n : int Atomic.t) -> Hegel.assume tc (try_decrement n))
      ()
  ; S.Concurrent_rule.create
      ~name:"nested"
      ~step:(fun _tc (ctx : _ J.ctx) (n : int Atomic.t) ->
        Concurrent.spawn_join ctx.concurrent () ~f:(fun _scope _ctx _concurrent ->
          Atomic.incr n))
      ()
  ]
;;

let invariants =
  [ S.Invariant.create ~name:"nonnegative" ~inv:(fun _tc n -> assert (Atomic.get n >= 0)) () ]
;;

module In_thread = struct
  type ctx = unit J.ctx
  type state = int Atomic.t

  let rules = rules ()
  let invariants = invariants
end

module On_parallel = struct
  type ctx = Parallel_kernel.t J.ctx
  type state = int Atomic.t

  let rules =
    S.Concurrent_rule.create
      ~name:"bump_twice"
      ~step:(fun _tc (ctx : ctx) (n : int Atomic.t) ->
        let #((), ()) =
          Parallel_kernel.fork_join2
            ctx.context
            (fun _ -> Atomic.incr n)
            (fun _ -> Atomic.incr n)
        in
        ())
      ()
    :: rules ()
  ;;

  let invariants = invariants
end

let settings =
  { (Hegel.Settings.create ~test_cases:5 ~seed:0 ()) with database = Hegel.Settings.Disabled }
;;

let in_thread_test () =
  Hegel.run_hegel_test ~settings (fun tc ->
    Concurrent_in_thread.with_blocking Await.Terminator.unkillable ~f:(fun concurrent ->
      S.run_concurrent
        ~concurrency:(J.of_concurrent concurrent)
        tc
        (module In_thread)
        ~step_count:10
        ~init:(Atomic.make 0)
        ~min_concurrency:4
        ~max_concurrency:4 [@nontail]))
;;

let parallel_scheduler_test () =
  let scheduler = Parallel_scheduler.scheduler () in
  Hegel.run_hegel_test ~settings (fun tc ->
    Parallel_scheduler.parallel scheduler (fun _kernel concurrent ->
      S.run_concurrent
        ~concurrency:(J.of_concurrent concurrent)
        tc
        (module On_parallel)
        ~step_count:10
        ~init:(Atomic.make 0)
        ~min_concurrency:4
        ~max_concurrency:4 [@nontail]))
;;

exception Boom of int

let worker_exception_test () =
  let module M = struct
    type ctx = unit J.ctx
    type state = unit

    let rules =
      [ S.Concurrent_rule.create
          ~name:"boom"
          ~step:(fun tc (_ : ctx) () -> raise (Boom (Hegel.draw tc (Hegel.integers ()))))
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
        Concurrent_in_thread.with_blocking Await.Terminator.unkillable ~f:(fun concurrent ->
          S.run_concurrent
            ~concurrency:(J.of_concurrent concurrent)
            tc
            (module M)
            ~step_count:5
            ~init:()
            ~min_concurrency:2
            ~max_concurrency:2 [@nontail]))
  with
  | () -> Alcotest.fail "expected Boom"
  | exception Boom _ -> ()
;;

let () =
  Alcotest.run
    "hegel-jane-concurrent"
    [ ( "hegel_jane_concurrent"
      , [ Alcotest.test_case "runs on Concurrent_in_thread" `Quick in_thread_test
        ; Alcotest.test_case "runs on a Parallel scheduler" `Quick parallel_scheduler_test
        ; Alcotest.test_case "worker exceptions reach the caller" `Quick worker_exception_test
        ] )
    ]
;;
