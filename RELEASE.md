RELEASE_TYPE: minor

This release changes how stateful rules update their state, adds concurrent
stateful testing, and removes `Hegel.spawn`, `Hegel.join` and `Hegel.worker`.

A rule now mutates its state in place instead of returning a new one, so 
rules return `unit`:

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

In concurrent stateful testing, stateful tests run rules concurrently 
from a number of workers. See the Stateful module documentation for details.

`Stateful.Pool.add` now takes the test case of the rule adding the value.
`Pool.create` accepts `?clone` to copy mutable values on reusable draws. 
Pool operations are safe to call from multiple workers.

The optional `hegel.jane.concurrent` sublibrary provides 
`Hegel_jane_concurrent.of_concurrent` to wrap a Jane Street `Concurrent.t`.

Failure reports no longer print `Checking invariants on the initial state.`
or the `Falsified after ...` count. Failure-blob replay preserves the original
exception backtrace, and a malformed blob is a usage error.
