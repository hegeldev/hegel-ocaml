RELEASE_TYPE: patch

This patch adds the optional `hegel.jane.async` sublibrary for testing code
using Jane Street's `Async` library.

A test body, rule, or invariant can now return `unit Deferred.t`. Hegel waits on
each Deferred on the Async scheduler in the execution context
when the test started.

`Hegel_jane_async.Stateful` has the sequential API of `Hegel.Stateful` with
the same names (e.g. `Rule`, `Invariant`, `run`).

With the `ppx_hegel_test` PPX, mark a test or a state machine `[@async]`:

```ocaml
module%hegel_state_machine [@async] Counter_machine = struct
  type state =
    { counter : Counter.t
    ; mutable model : int
    }

  let incr _tc s =
    s.model <- s.model + 1;
    Counter.incr s.counter
  [@@rule]

  let matches _tc s =
    Counter.read s.counter >>| fun n -> assert (n = s.model)
  [@@invariant]
end

let%hegel_test [@async] counter tc =
  Counter.create () >>= fun counter ->
  Counter_machine.run tc ~init:{ counter; model = 0 }
;;
```

On OxCaml with Jane Street's `concurrent` library installed, concurrent state
machines can be Async too:

```ocaml
module%hegel_concurrent_state_machine [@async] Counter_machine = struct
  type state =
    { counter : Counter.t
    ; mutable model : int
    }

  let incr _tc s =
    s.model <- s.model + 1;
    Counter.incr s.counter
  [@@rule { group = "ops" }]

  let matches _tc s =
    Counter.read s.counter >>| fun n -> assert (n = s.model)
  [@@invariant { always_check = true }]
end

let%hegel_test [@async] counter tc =
  Counter.create () >>= fun counter ->
  Counter_machine.run tc ~init:{ counter; model = 0 } ~max_concurrency:4
;;
```

On OxCaml, `Hegel_jane_async.Stateful` also has `Concurrent_rule`,
`Concurrent_pool`, and `run_concurrent`. Unlike their `Hegel.Stateful`
counterparts, they do not take a `ctx` or `concurrency` argument,
