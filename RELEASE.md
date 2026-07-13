RELEASE_TYPE: minor

This release overhauls what Hegel prints when a property fails.

Failing runs now produce a framed report: a header naming the test and its
source location, a `Falsified after N test cases (M discarded):` line, an
indented body of the drawn values and `note`s that led to the failure, the
exception, and a copy-pasteable `rerun with: [@@failure_blobs "..."]` line
(printed by default — disable with `with_print_blob false`). A run that finds
multiple failures reports each in its own numbered section, and on a terminal
the report is colorized (`HEGEL_COLOR=1|0` forces it on or off).

It also adds two ways to make a failure more informative:

- `require` and `require_equal` for assertions inside a test. `require_equal`
  renders a structural s-expression diff of the two values in the report, so you
  can see exactly which parts differ:

  ```ocaml
  let%hegel_test rev_involutive tc =
    let xs =
      draw tc (Generators.lists (Generators.integers ~min_value:0 ~max_value:9 ()) ())
    in
    require_equal tc (Core.List.sexp_of_t Core.Int.sexp_of_t) (List.rev (List.rev xs)) xs
  ```

- `?sexp_of_state` on `Stateful.run`, which prints the model state after the
  initial state and after each step so a failing sequence shows how the state
  evolved. The draws a rule makes are nested under its `Step N` header, and an
  invariant that fails is attributed to the step it broke on
  (`Invariant N violated after step M`).

`require_equal`'s diff is rendered with `sexp_diff`.
