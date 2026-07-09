RELEASE_TYPE: minor

Draw-time printing through the engine's document printer, and the explain
phase.

A failing test's report is now a document the engine lays out: drawn values
and notes render together when the test case completes, wrapping long values
across lines. Values print as OCaml source rather than sexps: lists as
`[ 0; 1 ]` (broken across lines behind leading semicolons when too wide),
tuples as `(0, false)`, hashmaps as association lists of pairs, options as
`None` / `Some (…)`, strings quoted and escaped, and float specials as
`nan` / `infinity` / `neg_infinity`. Structural generators (`lists`,
`tuples*`, `one_of`, `optional`, `hashmaps`, `filter`) print
compositionally — emitting their delimiters around their component
generators' own printing draws — so explain-phase annotations attach to
individual elements.

The engine's new explain phase runs after shrinking (enabled by default;
disable by passing `with_phases` a list without `Explain`): parts of the
minimal counterexample whose value is irrelevant to the failure are
annotated in the report with `(* or any other generated value *)`, down to
a single list element or tuple component. When several parts vary freely, a
leading whole-test note reports whether varying them together still always
failed.

Requires the next libhegel release (the `hegel_printer_*` document API,
`hegel_note`, `hegel_failure_comment*`, and `hegel_test_case_choice_count`).
