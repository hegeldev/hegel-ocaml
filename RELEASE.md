RELEASE_TYPE: minor

This release overhauls what Hegel prints when a property fails.

Failing runs now produce a framed report: a header naming the test and its
source location, a `Falsified after N test cases (M discarded):` line, an
indented body of the drawn values and `note`s, the exception, and 
`rerun with: [@@failure_blobs "..."]`. Failure blobs are now printed by default. 
A run that finds multiple failures reports each in its own numbered section, and
on a terminal the report is colorized (`HEGEL_COLOR=1|0` forces it on or off).

In stateful testing, the draws a rule makes are nested under its step header, 
and an invariant violation is attributed to the index of the invariant in the 
invariant list (`Invariant N violated after step M`).

It also adds two ways to make a failure more informative:

- `require` and `require_equal` for assertions inside a test. `require_equal`
  renders a structural s-expression diff of the two values in the report

- `?sexp_of_state` on `Stateful.run`, which prints the model state after the
  initial state and after each step. 
