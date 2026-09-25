open Core
open Async
module S = Hegel_jane_async.Stateful

let settings = Hegel.Settings.create ~test_cases:20 ~seed:0 ()
let run_async f = Thread_safe.block_on_async_exn f

let expect_failure f =
  match run_async f with
  | () -> Alcotest.fail "expected a failure"
  | exception exn ->
    (match Monitor.extract_exn exn with
     | Failure msg -> msg
     | exn -> Alcotest.failf "unexpected exception %s" (Exn.to_string exn))
;;

(* Reads, yields, then writes, so two workers interleaving at the yield lose an
   increment. *)
module Counter = struct
  type t = { mutable value : int }

  let create () = { value = 0 }

  let incr t =
    let value = t.value in
    Scheduler.yield () >>| fun () -> t.value <- value + 1
  ;;

  let read t = Scheduler.yield () >>| fun () -> t.value
end

type state =
  { counter : Counter.t
  ; mutable model : int
  }

let incr_rule =
  S.Concurrent_rule.create
    ~name:"incr"
    ~step:(fun _tc s ->
      s.model <- s.model + 1;
      Counter.incr s.counter)
    ()
;;

(* Waits on a Deferred, so it runs through the runner's own await. *)
let matches_model =
  S.Invariant.create
    ~name:"matches_model"
    ~always_check:true
    ~inv:(fun _tc s ->
      Counter.read s.counter
      >>| fun n -> if n <> s.model then failwithf "counter %d, model %d" n s.model ())
    ()
;;

let run_counter ?(invariants = [ matches_model ]) ~rules ~max_concurrency () =
  let module Machine = struct
    type nonrec state = state

    let rules = rules
    let invariants = invariants
  end
  in
  Hegel_jane_async.run_hegel_test ~settings (fun tc ->
    S.run_concurrent
      tc
      (module Machine)
      ~init:{ counter = Counter.create (); model = 0 }
      ~step_count:10
      ~max_concurrency)
;;

let finds_lost_update () =
  let msg = expect_failure (run_counter ~rules:[ incr_rule ] ~max_concurrency:4) in
  Alcotest.(check bool)
    "reported by the invariant"
    true
    (String.is_prefix msg ~prefix:"counter ")
;;

let one_worker_does_not_interleave () =
  run_async (run_counter ~rules:[ incr_rule ] ~max_concurrency:1)
;;

let failing_rule_fails_the_test () =
  let boom =
    S.Concurrent_rule.create
      ~name:"boom"
      ~step:(fun _tc _s -> Scheduler.yield () >>| fun () -> failwith "boom")
      ()
  in
  Alcotest.(check string)
    "message"
    "boom"
    (expect_failure (run_counter ~invariants:[] ~rules:[ boom ] ~max_concurrency:2))
;;

let assume_rejects_the_rule () =
  let reject =
    S.Concurrent_rule.create
      ~name:"reject"
      ~step:(fun tc _s -> Scheduler.yield () >>| fun () -> Hegel.assume tc false)
      ()
  in
  run_async (run_counter ~invariants:[] ~rules:[ reject; incr_rule ] ~max_concurrency:1)
;;

let rules_see_the_execution_context () =
  let key = Univ_map.Key.create ~name:"test" Int.sexp_of_t in
  let check_local =
    S.Concurrent_rule.create
      ~name:"check_local"
      ~step:(fun _tc _s ->
        Scheduler.yield ()
        >>| fun () ->
        if not (Option.equal Int.equal (Scheduler.find_local key) (Some 42))
        then failwith "local lost")
      ()
  in
  run_async (fun () ->
    Scheduler.with_local key (Some 42) ~f:(fun () ->
      run_counter ~invariants:[] ~rules:[ check_local ] ~max_concurrency:2 ()))
;;

module Callbacks = struct
  type state =
    { pool : (unit -> unit) S.Concurrent_pool.t
    ; calls : int ref
    }

  let add =
    S.Concurrent_rule.create
      ~name:"add"
      ~step:(fun tc s ->
        S.Concurrent_pool.add s.pool tc (fun () -> incr s.calls);
        return ())
      ()
  ;;

  let call =
    S.Concurrent_rule.create
      ~name:"call"
      ~step:(fun tc s ->
        let callback = Hegel.draw_silent tc (S.Concurrent_pool.values_consumed s.pool) in
        Scheduler.yield () >>| callback)
      ()
  ;;

  let rules = [ add; call ]
  let invariants = []
end

let pool_holds_callbacks () =
  let calls = ref 0 in
  run_async (fun () ->
    Hegel_jane_async.run_hegel_test ~settings (fun tc ->
      S.run_concurrent
        tc
        (module Callbacks)
        ~init:{ pool = S.Concurrent_pool.create tc; calls }
        ~step_count:10
        ~max_concurrency:2));
  Alcotest.(check bool) "callbacks ran" true (!calls > 0)
;;

let () =
  Alcotest.run
    "hegel-jane-async-concurrent"
    [ ( "run_concurrent"
      , [ Alcotest.test_case "finds a lost update" `Quick finds_lost_update
        ; Alcotest.test_case
            "one worker does not interleave"
            `Quick
            one_worker_does_not_interleave
        ; Alcotest.test_case
            "a failing rule fails the test"
            `Quick
            failing_rule_fails_the_test
        ; Alcotest.test_case "assume rejects the rule" `Quick assume_rejects_the_rule
        ; Alcotest.test_case
            "rules see the execution context"
            `Quick
            rules_see_the_execution_context
        ; Alcotest.test_case "pool holds callbacks" `Quick pool_holds_callbacks
        ] )
    ]
;;
