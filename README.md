# Hegel SDK for OCaml

A property-based testing SDK for OCaml that communicates with the
[Hegel](https://github.com/antithesishq/hegel-core) server (`hegeld`) — a
universal property-based testing engine powered by
[Hypothesis](https://hypothesis.readthedocs.io/). Hegel explores your test
inputs intelligently, finds failing cases, and automatically minimises them to
the simplest counterexample.

## Installation

1. Install the opam package:

```bash
opam install hegel
```

2. Run setup to fetch the `hegel` binary and create a Python venv:

```bash
just setup
```

If you already have the `hegel` binary, point `HEGEL_BINARY` at it:

```bash
HEGEL_BINARY=/path/to/hegel just setup
```

## Quick Start

Add `hegel` to your dune library dependencies:

```
(executable
 (name my_tests)
 (libraries hegel alcotest))
```

Write a property test:

```ocaml
open Hegel.Generators

let test_addition_commutative () =
  Hegel.Session.run_hegel_test ~name:"add_commutative" ~test_cases:100
    (fun () ->
      let a = generate (integers ~min_value:(-1000) ~max_value:1000 ()) in
      let b = generate (integers ~min_value:(-1000) ~max_value:1000 ()) in
      assert (a + b = b + a))

let () = test_addition_commutative ()
```

Hegel generates 100 random pairs `(a, b)`, checks the property, and reports
any counterexample it finds — automatically simplified to the smallest failing
values.

## Main API

### Running tests

```ocaml
Hegel.Session.run_hegel_test
  ~name:"my_property"   (* test name shown in reports *)
  ~test_cases:100        (* number of inputs to try, default 100 *)
  (fun () ->
    (* generate values and assert properties here *)
    ())
```

### Generators

All generators live in `Hegel.Generators`. Call `generate gen` inside a test
body to draw a typed OCaml value:

| Generator | Type | Description |
|-----------|------|-------------|
| `booleans ()` | `bool` | `true` or `false` |
| `integers ?min_value ?max_value ()` | `int` | Integers, optionally bounded |
| `floats ?min_value ?max_value ?allow_nan ?allow_infinity ()` | `float` | Floats |
| `text ?min_size ?max_size ()` | `string` | Unicode strings |
| `binary ?min_size ?max_size ()` | `string` | Byte strings |
| `just value` | `'a` | Constant value |
| `sampled_from options` | `'a` | One value from a list |
| `from_regex pattern ()` | `string` | Strings matching a regex |
| `lists elements ?min_size ?max_size ()` | `'a list` | Lists of generated elements |
| `hashmaps keys values ?min_size ?max_size ()` | `('k * 'v) list` | Dictionaries |
| `tuples2 g1 g2` | `'a * 'b` | 2-element tuples |
| `one_of generators` | `'a` | Choose from multiple generators |
| `optional element` | `'a option` | `None` or `Some value` |
| `emails ()`, `urls ()`, `domains ()` | `string` | Format-specific strings |
| `dates ()`, `times ()`, `datetimes ()` | `string` | ISO date/time strings |
| `ip_addresses ()` | `string` | IPv4 or IPv6 addresses |

### Combinators

```ocaml
(* Transform values *)
let doubled = map (fun n -> n * 2) (integers ~min_value:1 ~max_value:10 ())

(* Filter values — retries up to 3 times before discarding the test case *)
let even = filter (fun n -> n mod 2 = 0) (integers ())

(* Dependent generation — second generator depends on the first *)
let pair = flat_map
  (fun n ->
    map (fun lst -> (n, lst))
        (lists (integers ()) ~min_size:n ~max_size:n ()))
  (integers ~min_value:1 ~max_value:5 ())
```

### Helpers

```ocaml
Hegel.Client.assume (x <> 0)              (* discard this test case if false *)
Hegel.Client.note (Printf.sprintf "x=%d" x)  (* print only on failure *)
Hegel.Client.target score "label"         (* guide search toward higher scores *)
```

### Type-directed generation (PPX)

Add `ppx_hegel_generator` to your dune file:

```
(executable
 (name my_tests)
 (libraries hegel alcotest)
 (preprocess (pps ppx_hegel_generator)))
```

Then annotate types with `[@@deriving generator]`:

```ocaml
type point = { x : int; y : int } [@@deriving generator]
type color = Red | Green | Blue [@@deriving generator]

let () =
  Hegel.Session.run_hegel_test ~name:"point_test" ~test_cases:100 (fun () ->
    let p = point_generator () in
    let c = color_generator () in
    ignore (p, c))
```

The PPX synthesises `<type>_generator : unit -> <type>` functions for records,
variants, type aliases, and nested types. Supported field types: `int`, `bool`,
`float`, `string`, `t list`, `t option`, and named types.

## Project Layout

```
lib/          SDK source (hegel library — protocol, connection, generators, …)
ppx/          PPX deriver (ppx_hegel_generator)
test/         Alcotest unit and end-to-end test suite
examples/     Standalone example programs
docs/         Getting Started tutorial
conformance/  Compiled conformance test binaries
```

## Building and Testing

```bash
just check        # Full CI: lint + docs + tests with 100% coverage
just test         # Tests only (with coverage check)
just docs         # Build API docs (output in _build/default/_doc/)
just format       # Auto-format all source files
just lint         # Check formatting only
just conformance  # Run conformance tests against the Hegel server
```

## Full API Reference

Run `just docs` to build the full API documentation with odoc. The generated
HTML is written to `_build/default/_doc/_html/hegel/`. Open
`_build/default/_doc/_html/hegel/index.html` in your browser.

See `docs/getting-started.md` for a step-by-step tutorial with examples for
every major feature.
