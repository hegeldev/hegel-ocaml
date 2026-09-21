RELEASE_TYPE: patch

This patch makes `let%hegel_test` and `module%hegel_state_machine` well-behaved
when used with Merlin.  

A documentation comment above a `let%hegel_test` binding is now kept on the
function the PPX generates, so Merlin and odoc show it.

A mistyped or misplaced attribute is now reported rather than ignored.
Every Hegel attribute can be qualified with `hegel` (e.g. `[@@hegel.rule]`).
