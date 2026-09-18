RELEASE_TYPE: minor

This release changes how stateful rules update their state, adds concurrent
stateful testing, and removes `Hegel.spawn` and `Hegel.join`.

A rule now mutates its state in place instead of returning a new one, so 
rules now return `unit`:

```ocaml
(* before *)
module%hegel_state_machine Stack = struct
  type state = int list [@@deriving sexp_of]

  let push tc stack =
    let n = draw tc (integers ~min_value:0 ~max_value:9 ()) in
    n :: stack
  [@@rule]
end

let%hegel_test stack_model tc = Stack.run tc ~init:[]

(* after *)
module%hegel_state_machine Stack = struct
  type state = int list ref

  let sexp_of_state stack = sexp_of_list sexp_of_int !stack

  let push tc stack =
    let n = draw tc (integers ~min_value:0 ~max_value:9 ()) in
    stack := n :: !stack
  [@@rule]
end

let%hegel_test stack_model tc = Stack.run tc ~init:(ref [])
```

`Stateful.Pool.add` now takes the test case of the rule adding the value.
`Pool.create` accepts `~clone` to copy mutable values on reusable draws. One
pool type serves sequential and concurrent machines, and all of its
operations are safe to call from several workers.

Concurrent stateful testing runs the rules of a round on several workers at
once. Write a `module%hegel_concurrent_state_machine` and pass concurrency
bounds to its `run`; the engine draws the number of workers in that range:

```ocaml
module%hegel_concurrent_state_machine Counter = struct
  let increment _tc counter = Atomic.incr counter [@@rule]

  let nonnegative _tc counter = assert (Atomic.get counter >= 0)
  [@@invariant always_check]
end

let%hegel_test counter tc =
  Counter.run tc ~init:(Atomic.make 0) ~min_concurrency:2 ~max_concurrency:4
```

Concurrent rules share their state, so they synchronize their own mutations.
A rule may name a group with `[@@rule "group_name"]`: only rules in the same
group run concurrently, and ungrouped rules share an anonymous group.
Sequential state machines do not accept groups. Without the PPX, use
`Stateful.Concurrent_rule.create ?group`, a `Stateful.Concurrent_state_machine`
module, and `Stateful.run_concurrent`. When `max_concurrency > 1` the run is
nondeterministic, so failures are reported from the execution that found them,
without shrinking, replay, or a reproduction blob, and each line of the report
carries the worker that produced it and the time since the test case began.

How the workers run is a `Hegel.Concurrency.t`, passed as `~concurrency`. The
default, `Concurrency.threads`, runs each worker on a systhread, which
interleaves workers but does not run them in parallel. On standard OCaml,
`Concurrency.domains` runs them in parallel on a pool of domains. Any
implementation of `spawn_join_n` works, so a program can run rounds on the
scheduler it already uses; on OxCaml, the optional `hegel.jane.concurrent`
library wraps a Jane Street `Concurrent.t` with
`Hegel_jane_concurrent.of_concurrent`.

Hegel no longer spawns threads itself: `Hegel.spawn`, `Hegel.join`, and the
`worker` type are removed. Use `Hegel.clone` with the threads or domains of
your choice, or a concurrent state machine.

On OxCaml, the library is annotated with modes. A concurrent rule body must be
portable and sees its state contended, so it keeps mutable state in atomics or
capsules; a sequential rule body is an ordinary function. `hegel.jane`
exposes a portable interface, so generators derived with `Hegel_jane.Derive`
work from portable code.

Failure reports no longer print `Checking invariants on the initial state.`
or the `Falsified after ...` count. Failure-blob replay preserves the original
exception backtrace, and a malformed blob is a usage error.
