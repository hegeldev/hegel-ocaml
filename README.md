# Hegel for OCaml

Hegel is a property-based testing library for OCaml. Hegel is based on
[Hypothesis](https://github.com/hypothesisworks/hypothesis), using the
[Hegel](https://hegel.dev/) protocol.

## Install Hegel

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

To use the `let%hegel_test` extension shown below, also install the PPX
package.

```bash
opam pin add ppx_hegel_test "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

You will need [`uv`](https://docs.astral.sh/uv/) installed and on your PATH.

## Quick start

Add `hegel` to your dune library dependencies:

```
(library
 (name my_tests)
 (libraries hegel)
 (inline_tests (backend ppx_hegel_test))
 (preprocess (pps ppx_hegel_test)))
```

Write a property test using `let%hegel_test`:

```ocaml
open Hegel
open Hegel.Generators

let%hegel_test commutative_addition tc =
  let a = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  let b = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
  assert (a + b = b + a)
;;
```

Run `dune runtest`. Hegel generates 100 random input pairs and reports the
minimal counterexample if it finds one.

To override the default settings, attach a `[@@settings ...]` attribute:

```ocaml
let%hegel_test commutative_addition tc =
  let a = draw tc (integers ()) in
  let b = draw tc (integers ()) in
  assert (a + b = b + a)
[@@settings Hegel.settings ~test_cases:500 ()]
;;
```

`let%hegel_test name tc = body` also defines `name` as a plain
`unit -> unit` function, so you can still call it directly from an
executable or hand it to another test harness like Alcotest.

For a full walkthrough, see [docs/getting-started.md](docs/getting-started.md).

## Development

```bash
just setup       # Install dependencies
just check       # Full CI: lint + docs + tests with 100% coverage
just test        # Run tests only
just conformance # Run cross-language conformance tests
```
