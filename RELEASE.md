RELEASE_TYPE: minor

This release makes the stateful step count a per-test parameter. `Stateful.run` now takes
an optional `?step_count` argument (default 50). The `stateful_step_count` settings field 
and `with_stateful_step_count` have been removed.

A local libhegel built by `cargo build -p hegeltest-c` is now picked up by the library 
loader. Previously it looked for a nonexistent file, so the sibling checkout was never used.
