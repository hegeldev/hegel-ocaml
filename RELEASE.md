RELEASE_TYPE: minor

This release adds weights to stateful rules, adds concurrency capabilities to concurrent
rules, and changes how options are passed to `[@@rule]` and `[@@invariant]`.

A rule's weight is a hint about how often libhegel should pick that rule
relative to other rules. It defaults to `1.0`, and may be written as either an
integer or a float:

```ocaml
module%hegel_state_machine Stack = struct
  let push tc stack =
    let n = draw tc (integers ~min_value:0 ~max_value:9 ()) in
    stack := n :: !stack
  [@@rule { weight = 3.0 }]
end
```
Without the PPX, pass `?weight` to `Stateful.Rule.create`/`Stateful.Concurrent_rule.create`. 
A weight must be finite and strictly positive.

`Stateful.Rule.create` now takes a trailing unit argument.

Options on `[@@rule]` and `[@@invariant]` are now a record:

```ocaml
(* before *)
let withdraw tc account = ... [@@rule "money"]
let positive _tc account = ... [@@invariant always_check]

(* after *)
let withdraw tc ctx account = ... [@@rule { group = "money"; weight = 2.0 }]
let positive _tc account = ... [@@invariant { always_check = true }]
```
The options may be given in any order or left out.

Concurrent rules now receive the worker's context `ctx`. 

`Hegel.Concurrency.t` takes the context's type as a parameter. `Stateful.Concurrent_state_machine`
declares `type ctx`, and `Stateful.run_concurrent` takes `~concurrency` as a required argument. 

A `module%hegel_concurrent_state_machine` that does not declare `type ctx` uses threads by default
and its rules take `()` as the context.

On OxCaml, `Hegel_jane_concurrent.of_concurrent` passes `Hegel_jane_concurrent.ctx` defined as follows:

```
type 'a ctx =
  { context : 'a
  ; concurrent : 'a Concurrent.t
  }
```

`context` is the scheduler's per-task context and `concurrent` is a concurrency capability. 
