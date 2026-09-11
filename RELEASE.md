RELEASE_TYPE: minor

This release reworks stateful testing around a state machine module. 
`Stateful.run` now takes a module of the new type `Stateful.State_machine`:

```
module type State_machine = sig
  type state

  val rules : state Rule.t list
  val invariants : state Invariant.t list
end
```

The step count is now set per test. `Stateful.run` takes an optional `?step_count`
argument (default 50). The `stateful_step_count` settings field and
`with_stateful_step_count` have been removed. 

Invariants now take a test case.

State machines can now be defined using `module%hegel_state_machine M = struct … end`. Rules are marked with `[@@rule]` and invariants with `[@@invariant]` or 
`[@@invariant always_check]`. Each rule and invariant is named after its binding,
and draws in its body print under their `let`-bound names. If the module defines `sexp_of_state`, `run` uses it to print the state after each step.

A local libhegel built by `cargo build -p hegeltest-c` is now picked up by the library 
loader. Previously it looked for a nonexistent file, so the sibling checkout was never used.
