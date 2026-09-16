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

Concurrent rules can specify a group with `[@@rule "group_name"]`. Only rules
in the same group may run concurrently. Ungrouped rules share an anonymous
group. Sequential state machines do not accept rule groups.

Concurrent state machines may also be created without the PPX.

When `max_concurrency > 1`, failures are reported without shrinking
or reproduction blobs. `Stateful.Concurrent_pool` provides thread-safe pools
for sharing values between rules. Both pool types accept `~clone` to copy
mutable values on reusable draws.

Failure-blob replay now preserves the original exception backtrace. Malformed
blobs produce usage errors. Failure reports no longer print the
`Falsified after ...` count.
