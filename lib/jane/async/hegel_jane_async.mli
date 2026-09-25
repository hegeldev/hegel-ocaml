(** Hegel tests whose bodies are Async code.

    A test body, rule, or invariant returns [unit Deferred.t]. Hegel waits on
    each one before moving on. Test bodies run in the execution context of the
    code that started the test.

    {[
    let test () =
      Hegel_jane_async.run_hegel_test (fun tc ->
        let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:9 ()) in
        Store.put_and_get store n >>| fun stored -> assert (stored = n))
    ;;
    ]}

    Sequential state machines have rules and invariants that return [unit Deferred.t]:

    {[
    module Counter_machine = struct
      type state =
        { counter : Counter.t
        ; mutable model : int
        }

      let rules =
        [ Hegel_jane_async.Stateful.Rule.create
            ~name:"incr"
            ~step:(fun _tc s ->
              s.model <- s.model + 1;
              Counter.incr s.counter)
            ()
        ]
      ;;

      let invariants =
        [ Hegel_jane_async.Stateful.Invariant.create
            ~name:"matches"
            ~inv:(fun _tc s -> Counter.read s.counter >>| fun n -> assert (n = s.model))
            ()
        ]
      ;;
    end

    let test () =
      Hegel_jane_async.run_hegel_test (fun tc ->
        Counter.create ()
        >>= fun counter ->
        Hegel_jane_async.Stateful.run
          tc
          (module Counter_machine)
          ~init:{ counter; model = 0 })
    ;;
    ]} *)

(** The [Hegel.Io] instance for Async. *)
module Async_io : Hegel.Io with type 'a t = 'a Async.Deferred.t

include module type of Hegel.Make (Async_io)
