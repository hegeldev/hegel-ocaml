RELEASE_TYPE: patch

This patch adds concurrent stateful testing. In a step of the test,
multiple workers may each run multiple rules concurrently. Use
`module%hegel_concurrent_state_machine` to mark rules and invariants, and
pass explicit concurrency bounds when running it:

```ocaml
module%hegel_concurrent_state_machine Counter = struct
  let increment _tc counter = Atomic.incr counter [@@rule]

  let nonnegative _tc counter = assert (Atomic.get counter >= 0)
  [@@invariant always_check]
end

let%hegel_test counter tc =
  Counter.run tc ~init:(Atomic.make 0) ~min_concurrency:2 ~max_concurrency:4
```


Failure reports no longer print `Checking invariants on the initial state.`
or the `Falsified after ...` count. Failure-blob replay preserves the original
exception backtrace, and a malformed blob is a usage error.
