# Hegel for OCaml

Hegel is a property-based testing library for OCaml based on
[Hypothesis](https://github.com/hypothesisworks/hypothesis), and runs the native
[Hegel](https://hegel.dev/) engine in-process via the `libhegel` C library.

## Install Hegel

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

Hegel calls the native `libhegel` shared library and locates it automatically
at runtime — no separate install step. It looks, in order, for:

1. `$HEGEL_LIBHEGEL_PATH` — an explicit path to the library (or a directory
   containing it);
2. a sibling [hegel-rust](https://github.com/hegeldev/hegel-rust) checkout at
   `../hegel-rust/target/release/` (then `.../debug/`) relative to your project;
3. a checksum-verified download of the matching version from hegel-rust's GitHub
   releases, cached under `~/.cache/hegel-ocaml/libhegel/<version>/`.

Set `HEGEL_LIBHEGEL_NO_DOWNLOAD=1` to opt out of the download fallback.

Hegel for OCaml supports **Linux** (amd64/arm64) and **macOS** (Apple Silicon).
macOS amd64 (Intel) has no published `libhegel` artifact, so on that platform point
`HEGEL_LIBHEGEL_PATH` at a locally built `libhegel.dylib`.

## Quick start

Add `hegel` to your dune library dependencies:

```
(library
 (name my_tests)
 (libraries hegel)
 (inline_tests (backend ppx_hegel_test))
 (preprocess (pps ppx_hegel_test)))
```

`ppx_hegel_test` is not required to use Hegel, but strongly recommended as it 
adds many convenience features and integration with `dune runtest`. The examples
below assume `ppx_hegel_test` is used.

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

Run `dune runtest`. Hegel generates up to 100 random input pairs and reports the
minimal counterexample if it finds one. When a test fails, Hegel prints each
value you drew from the failing case, named after the `let` binding it was
bound to (`a = …`, `b = …`). See [Debugging failures](docs/getting-started.md#debug-your-failing-test-cases)
for details.

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
just check       # Full CI: lint + docs + tests with 100% coverage
just test        # Run tests only
```

`libhegel` is located (and downloaded + cached if needed) automatically at
runtime — there is no separate setup step.
