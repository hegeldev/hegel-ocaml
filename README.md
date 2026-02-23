# Hegel SDK for OCaml

A property-based testing SDK for OCaml that communicates with the
[Hegel](https://github.com/antithesishq/hegel-core) server (`hegeld`) — a
universal property-based testing engine powered by
[Hypothesis](https://hypothesis.readthedocs.io/). Hegel explores your test
inputs intelligently, finds failing cases, and automatically minimises them to
the simplest counterexample.

## Installation

1. Install opam dependencies:

```bash
opam install hegel
```

2. Run setup to fetch the `hegel` binary:

```bash
just setup
```

If you already have the `hegel` binary, point `HEGEL_BINARY` at it:

```bash
HEGEL_BINARY=/path/to/hegel just setup
```

## Quick Start

Add `hegel` to your library's dependencies in `dune`:

```
(executable
 (name my_tests)
 (libraries hegel alcotest))
```

Then write a property test:

```ocaml
open Hegel.Generators

let test_addition_commutative () =
  Hegel.Session.run_hegel_test ~name:"add_commutative" ~test_cases:100
    (fun () ->
      let a = Hegel.Cbor_helpers.extract_int (generate (integers ~min_value:(-1000) ~max_value:1000 ())) in
      let b = Hegel.Cbor_helpers.extract_int (generate (integers ~min_value:(-1000) ~max_value:1000 ())) in
      assert (a + b = b + a))
```

Run with:

```bash
just test
```

Hegel generates 100 random pairs `(a, b)`, checks the property, and reports
any counterexample it finds — automatically simplified to the smallest failing
values.

## Main API

### Running tests

```ocaml
Hegel.Session.run_hegel_test
  ~name:"my_property"   (* test name shown in reports *)
  ~test_cases:100        (* number of inputs to try *)
  (fun () ->
    (* generate values and assert properties here *)
    ())
```

### Generators

All generators live in `Hegel.Generators`. Call `generate gen` inside a test
to draw a value:

| Generator | Description |
|-----------|-------------|
| `booleans ()` | `true` or `false` |
| `integers ?min_value ?max_value ()` | Integers, optionally bounded |
| `floats ?min_value ?max_value ?allow_nan ?allow_infinity ()` | Floats |
| `text ?min_size ?max_size ()` | Unicode strings |
| `binary ?min_size ?max_size ()` | Byte strings |
| `sampled_from options` | One value from a list |
| `lists elements ?min_size ?max_size ()` | Lists of generated elements |
| `hashmaps keys values ?min_size ?max_size ()` | Dictionaries |

### Combinators

```ocaml
(* Transform values *)
let doubled = map (fun v -> `Int (Hegel.Cbor_helpers.extract_int v * 2))
                  (integers ~min_value:1 ~max_value:10 ())

(* Filter values *)
let even = filter (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
                  (integers ())

(* Dependent generation *)
let pair = flat_map
  (fun n_v ->
    let n = Hegel.Cbor_helpers.extract_int n_v in
    map (fun lst -> `Array [n_v; lst])
        (lists (integers ()) ~min_size:n ~max_size:n ()))
  (integers ~min_value:1 ~max_value:5 ())
```

### Helpers

```ocaml
Hegel.Client.assume (x <> 0)         (* discard this test case if false *)
Hegel.Client.note (Printf.sprintf "x=%d" x)  (* print only on failure *)
Hegel.Client.target score "label"    (* guide search toward higher scores *)
```

## Project Layout

```
lib/          SDK source (hegel, hegel.generators, hegel.session, …)
test/         Alcotest test suite
examples/     Standalone example programs
docs/         Getting Started tutorial
conformance/  Compiled conformance test binaries
```

## Building and Testing

```bash
just check      # Full CI: lint + docs + tests with 100% coverage
just docs       # Build API docs (output in _build/default/_doc/)
just format     # Auto-format all source files
```

## Full API Reference

Run `just docs` to build the full API documentation with odoc. The generated
HTML is written to `_build/default/_doc/_html/hegel/`. Open
`_build/default/_doc/_html/hegel/index.html` in your browser.

See `docs/getting-started.md` for a step-by-step tutorial.
