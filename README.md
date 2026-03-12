# hegel-ocaml

An OCaml SDK for [Hegel](https://github.com/hegeldev/hegel-core) —
universal property-based testing powered by
[Hypothesis](https://hypothesis.works/).

Hegel generates random inputs for your tests, finds failures, and automatically
shrinks them to minimal counterexamples.

## Installation

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

### Hegel server

The SDK automatically manages the `hegel` server binary. On first use it
creates a project-local `.hegel/venv` virtualenv and installs the pinned
version of [hegel-core](https://github.com/antithesishq/hegel-core) into it.
Subsequent runs reuse the cached binary unless the pinned version changes.

To use your own `hegel` binary instead (e.g. a local development build), set
the `HEGEL_SERVER_COMMAND` environment variable:

```bash
export HEGEL_SERVER_COMMAND=/path/to/hegel
```

The SDK also requires [`uv`](https://docs.astral.sh/uv/) to be installed for
automatic server management.

## Quick Start

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
  run_hegel_test ~test_cases:100
    (fun () ->
      let a = generate (integers ~min_value:(-1000) ~max_value:1000 ()) in
      let b = generate (integers ~min_value:(-1000) ~max_value:1000 ()) in
      assert (a + b = b + a))
```

Run with `dune exec` as normal. Hegel generates 100 random input pairs and
reports the minimal counterexample if it finds one.

For a full walkthrough, see [docs/getting-started.md](docs/getting-started.md).

## Development

```bash
just setup       # Install dependencies (hegel binary + opam packages)
just check       # Full CI: lint + docs + tests with 100% coverage
just test        # Run tests only
just conformance # Run cross-language conformance tests
```
