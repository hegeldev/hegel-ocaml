RELEASE_TYPE: minor

This release makes the stateful step count a per-test parameter. `Stateful.run` now takes
an optional `?step_count` argument (default 50). The `stateful_step_count` settings field 
and `with_stateful_step_count` have been removed.

Stateful invariants now take a test case.

Two new `ppx` extensions `let%hegel_rule` and `let%hegel_invariant` have been added.
Draws in the body print under their `let` names, as they do in a `let%hegel_test` body.
`[@@always_check]` marks an invariant to be checked after every step.

A local libhegel built by `cargo build -p hegeltest-c` is now picked up by the library 
loader. Previously it looked for a nonexistent file, so the sibling checkout was never used.
