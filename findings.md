# Findings: rule context (`ctx`) for concurrent stateful testing

Notes from investigating the Jane Street transcript that proposes threading a
per-task context into concurrent rule bodies. Sources checked: the installed
`concurrent`, `await`, and `parallel` libraries in the `oxcaml` switch (all
v0.18~preview.130.106+341), `hegel-c/include/hegel.h` and
`hegel-c/src/native/core/state_machine.rs` in the sibling `hegel-rust`
checkout, and this repo's `lib/stateful.ml.in`, `lib/internal.ml.in`,
`lib/concurrency.ml.in`, and `lib/jane/concurrent/`.

## 1. What `ctx` is

`ctx` is the per-task context of Jane Street's `Concurrent` library, the type
parameter on `'concurrent_ctx Concurrent.t`. From `concurrent.mli`:

> The `'concurrent_ctx` type parameter is the per-task context type - a value
> of type `'concurrent_ctx` will be passed in `@ local` as the second argument
> to each spawned task.

Every spawned task receives three arguments:

1. the `Scope.t` it was spawned into (the join it belongs to),
2. the scheduler's `'concurrent_ctx @ local`,
3. a fresh `'concurrent_ctx Concurrent.t @ local` for opening child scopes.

The context is the scheduler's channel for handing each task a capability that
a portable closure could not capture (local, unique, or mode-restricted
values), minted when the task starts.

Concrete instantiations:

- `Concurrent_in_thread.with_blocking` gives a `unit Concurrent.t`: no context.
- `Parallel_scheduler.parallel` gives a `Parallel_kernel.t Concurrent.t`: each
  task receives the `Parallel_kernel.t` that `Parallel_kernel.fork_join*` and
  the `Parallel` APIs take as their first argument.
- `Spawn.spawn_nonportable ~access` uses `'k Capsule.Access.boxed` as the
  context to grant a nonportable task capsule access.
- `Scheduler.with_context` wraps any scheduler with a per-task context builder.

`Parallel_kernel` and `Parallel_scheduler` are real (`parallel.kernel`,
`parallel.scheduler`), already listed in `.github/oxcaml-ci.opam` and linked by
`lib/jane/concurrent/test/dune`. The transcript's `Parallel.Arrays.length
ctx.context ...` line is a placeholder; `Parallel.Arrays.length` takes no
kernel. Real consumers are `Parallel_kernel.fork_join2 ctx.context f g` etc.

Today `of_concurrent` in `lib/jane/concurrent/hegel_jane_concurrent.ml`
discards all three task arguments (`fun _scope _ctx _concurrent i -> f i`) and
passes `()` as the scope context (second positional argument of
`Concurrent.spawn_join_n`).

## 2. What the adapter's `ctx` record should hold

Exactly the two task arguments a rule can legitimately use:

```ocaml
type 'ctx ctx =
  { context : 'ctx                     (* the scheduler's per-task value *)
  ; concurrent : 'ctx Concurrent.t     (* for spawning nested tasks *)
  }

val of_concurrent : 'ctx Concurrent.t @ local -> 'ctx ctx Hegel.Concurrency.t @ local
```

- No modality on the fields (`@@ local` is not a modality; `@@ global` is the
  opposite of what is wanted). A record built from local values is local and
  its fields are local.
- Neither field is recoverable from the other: the `Concurrent.t` does not
  expose the per-task context, and the context alone cannot spawn.
- Under `Concurrent_in_thread` the record is `unit ctx`; `concurrent` is still
  useful for handing to a system under test.
- Keep the record in the adapter only. `Hegel.Concurrency.t` becomes
  `'ctx Concurrency.t` and passes the value through opaquely; `threads` and
  `domains` are `unit Concurrency.t` and call `f () i`.
- `Concurrency.t`'s `f` must take the context `@ local`:
  `f:('ctx @ local -> int -> outcome) @ portable`.
- `Concurrent_rule.t.step` becomes
  `(test_case -> 'ctx @ local -> 'state @ contended -> unit) @@ portable`.
- `Internal.with_block` runs the body under `Fun.protect`; a body closing over
  a local record cannot be passed to it, so `with_block` needs a
  local-accepting callback with a hand-written try/reraise.

Adapter body, as sketched in the conversation:

```ocaml
let of_concurrent (c @ local) = exclave_
  { Hegel.Concurrency.spawn_join_n =
      (fun ~n ~f ->
        Concurrent.spawn_join_n c () ~n ~f:(fun _scope ctx concurrent i ->
          f { context = ctx; concurrent } i)
        |> Base.Iarray.to_list)
  }
```

`i` is the worker index from `Concurrent.spawn_join_n`, not the test case.
`dispatch_round` (`lib/stateful.ml.in`) clones the test case once per worker
before spawning and looks clone `i` up by index inside the closure.

## 3. Scopes

A `Scope.t` (`Await.Scope`) is the join of structured concurrency: `with_`
returns only when every task added to the scope has exited; an uncaught
exception in any task terminates the scope, cancels siblings, and re-raises
from the join; `terminator`/`terminate` carry cancellation; `context` returns
the scope's own context (hegel passes `()`); `add` registers more tasks.

Every `spawn_join_n` call opens a fresh scope and drains it before returning.
In hegel that is one scope per round. Scopes nest: a rule calling
`Concurrent.spawn_join2 ctx.concurrent () f g` opens a child scope that closes
before the rule body returns. `Concurrent.into_scope concurrent scope` does
NOT open a scope; it spawns peers directly into the given (round's) scope.

Recommendation: do not expose the round's `Scope.t` to rules. Exposing
`Scope.terminator` (observe-only) is harmless. Everything a rule needs for
nested work comes from `ctx.concurrent`.

## 4. Empirical check (standalone OxCaml program)

`scratchpad/scope_demo/bin/demo.ml` models `dispatch_round`: two workers under
`Concurrent.spawn_join_n`, each wrapped in
`match ... with exception exn -> Some exn`; worker 1 does 20 timed steps with
`Await.Await.check_terminated` between them.

```
== A. detached task on the round's scope raises
  spawn_join_n RAISED: Failure("detached")
  sibling steps: 4/20, detached finished: 0
== B. rule terminates the round's scope
  spawn_join_n returned 2 outcomes
  worker 0 outcome: None
  worker 1 outcome: Some Terminated
  sibling steps: 0/20, detached finished: 0
== C. detached task only mutates late
  spawn_join_n returned 2 outcomes
  worker 0 outcome: None
  worker 1 outcome: None
  sibling steps: 20/20, detached finished: 1
== D. control: nested spawn_join through ctx.concurrent raises
  spawn_join_n returned 2 outcomes
  worker 0 outcome: Some Failure("nested")
  worker 1 outcome: None
  sibling steps: 20/20, detached finished: 0
```

- A: a task spawned onto the round's scope fails after its rule returned; the
  failure comes out of `spawn_join_n` in `dispatch_round`, not out of any
  worker; the sibling is cut off mid-round; the outcomes list is never
  returned.
- B: `Scope.terminate` from a rule makes a cooperating sibling raise
  `Terminated`, which the wrapper records as that sibling's outcome; hegel
  would report it as a test failure. A sibling that never checks the
  terminator runs to completion with `None`.
- C: the join does wait for detached scope members.
- D: the same failing work through `ctx.concurrent` is caught inside the rule
  body and recorded as worker 0's outcome; the sibling is untouched.

Build/run: `opam exec --switch=oxcaml -- dune exec ./bin/demo.exe` in the demo
directory (`[@nontail]` needed on the local spawn calls).

## 5. Corrections made during the conversation

- **Worker attribution does not matter.** `reraise_worker_failure` picks one
  exception by precedence and re-raises it; no worker index is ever surfaced.
  Notes made on a captured clone land in a region anchored inside the
  spawning rule's block, so the printed trace still points at the right rule.
- **There is no replay for concurrent machines.** `max_concurrency > 1`
  declares the run nondeterministic (`HEGEL_RUN_STATUS_FAILED_NONDETERMINISTIC`):
  no shrinking, no final replay, no blob; every case is stamped
  nondeterministic up front and `should_print` prints every case. All
  determinism/shrinker arguments against mid-rule cancellation are void, and
  the proposed libhegel `hegel_state_machine_end_round` call is unnecessary; a
  client-side flag checked between rules does the same job.
- **Cloning fixes the memory hazards.** A clone is owned by the whole test
  case (`own_clone` → `free_owned` at case completion), has its own choice
  stream, and its own print region; the engine allows a clone and its source
  to be driven concurrently. Clone-per-task is the right pattern for any
  concurrency a rule starts.

## 6. What actually breaks if rules can reach the round's scope

- A detached raise drops the sibling outcomes (a sibling's usage error or
  overrun in the same round is lost).
- `Scope.terminate` from a rule yields a spurious `Terminated` test failure
  for cooperating siblings (scenario B) unless cancellation is a recognized
  outcome.
- `Assume_rejected` from a detached task marks the whole case INVALID instead
  of retrying that rule's slot (`run_rules` handles rejection per rule).
- Without cloning, a detached task using the rule's `tc` is a use after free
  (the block handle is freed when the body returns).
- Mid-body interruption leaves half-mutated state; this matters only when the
  round then continues to invariants (the terminate case).

Detached spawning onto the round's scope is the one pattern whose failures
bypass the outcome list entirely and remains the thing to keep out.

## 7. Failure precedence in `reraise_worker_failure`

Order: usage/internal error > overrun > invalidation > test failure; lowest
worker index within a category. Rationale: the higher categories mean a
worker did not finish its rule for a reason that is not a bug, so a failure
observed by another worker in the same round may be downstream of
half-mutated state. The precedence does not repair state; once it raises, the
round ends (`print_state`/`check_invariants` are skipped, `run_machine` frees
the machine, `run_test_case` marks INVALID/OVERRUN or aborts on a usage
error), and the next case starts from a fresh `init`.

If cancellation is added, a `Cancelled` outcome belongs above test failure
(alongside invalidation) for the same reason.

## 8. Ending a round early (design options, no engine change needed)

- Rule granularity, core only: `Stateful.end_round : test_case -> unit` sets a
  per-round `bool Atomic.t` created in `dispatch_round`; `run_rules` checks it
  before each `next_rule_for_worker` call and notes "Round ended early by
  <rule>". Works identically under `threads`, `domains`, and the adapter.
- Mid-rule (for bodies blocking on `Await`): extend the capability with a
  runner-owned `cancel : unit -> unit` hook and a
  `Completed | Cancelled | Failed of exn * raw_backtrace` outcome; the adapter
  implements `cancel` as `Scope.terminate` and maps `Await.Terminated` to
  `Cancelled`; ctx exposes `terminator` observe-only.
- Fail-fast: today a failing worker's siblings run their whole round; the
  wrapper in `dispatch_round` could end the round before returning a `Some`
  outcome. In a mode where the discovering run is the only evidence, keeping
  the trace short and pointed at the failure is the main motivation.

## 9. Engine facts (hegel-rust `state_machine.rs`, `state.rs`)

- Each worker's continue/stop decision is a boolean draw on that worker's
  own clone stream; stop is the simplest value.
- `next_group` resets all per-worker state and never checks that streams were
  exhausted; a worker stopping early is tolerated.
- Forced draws are recorded as nodes (`was_forced`).
- A replay drawing past its recorded prefix falls through to the trailing
  template or fresh random draws; it does not overrun.

## 10. Assumptions in rules

- A rejected rule does not discard the case: `run_rules` reports the rejection
  to the engine, notes it, and continues the round. Mutations before the
  `assume` persist and are seen by later rules and invariants. `assume` is a
  precondition and must run before the first mutation (the stack example in
  `stateful.mli.in` does this).
- In a concurrent rule, check-then-act races with other workers, so the
  precondition and the effect must be one atomic step: let the system under
  test decide (`assume tc (Sut.try_op ...)`) or use a compare-and-set loop.
  Assumptions about freshly drawn values are safe.
- **Latent race:** the `decrement` rule at
  `lib/jane/concurrent/test/test_hegel_jane_concurrent.ml:14`
  (`assume tc (Atomic.get n > 0); Atomic.decr n`) can drive `n` to -1 with two
  workers and fail the `nonnegative` invariant spuriously. Rare under
  systhreads, real under `Parallel_scheduler`.

## 11. Defaults

- `Concurrency.threads` is the default on both compilers (`CONCURRENCY_ARG`
  in `stateful.ml.in`), one systhread per worker per round: interleaving, not
  parallelism.
- `Concurrency.domains` is upstream-only (`#ifndef OXCAML`); on OxCaml the
  only alternative is the `hegel.jane.concurrent` adapter passed explicitly.
- The runners stay nonportable because `Thread.create` is nonportable; this is
  the constraint behind the transcript's "`~concurrency` required for
  non-unit ctx" PPX rule.
