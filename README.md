# Hegel for OCaml

Hegel is a property-based testing library for OCaml. Hegel is based on
[Hypothesis](https://github.com/hypothesisworks/hypothesis), using the
[Hegel](https://hegel.dev/) protocol.

## Install Hegel

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

You will need [`uv`](https://docs.astral.sh/uv/) installed and on your PATH.

## Quick start

Add `hegel` to your dune library dependencies:

```
(executable
 (name my_tests)
 (libraries hegel))
```

Write a property test:

```ocaml
open Hegel
open Hegel.Generators

let () =
  run_hegel_test (fun tc ->
    let a = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
    let b = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
    assert (a + b = b + a))
```

Run with `dune exec` as normal. Hegel generates 100 random input pairs and
reports the minimal counterexample if it finds one.

For a full walkthrough, see [docs/getting-started.md](docs/getting-started.md).

## Development

```bash
just setup       # Install dependencies
just check       # Full CI: lint + docs + tests with 100% coverage
just test        # Run tests only
just conformance # Run cross-language conformance tests
```
