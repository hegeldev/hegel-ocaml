# Getting Started with Hegel for OCaml

## Install Hegel

Install the opam package and fetch the `hegel` binary:

```bash
opam install hegel
just setup
```

If you already have the `hegel` binary available, set `HEGEL_BINARY` before
running `just setup`:

```bash
HEGEL_BINARY=/path/to/hegel just setup
```

## Write your first test

Create `example.ml`:

```ocaml
open Hegel.Generators

let test_integers () =
  Hegel.Session.run_hegel_test ~name:"test_integers" ~test_cases:100
    (fun () ->
      let n = generate (integers ()) in
      Printf.printf "called with %d\n%!" n;
      assert (n = n))  (* integers are always equal to themselves *)

let () = test_integers ()
```

Inside the test body you call `generate` on generators to produce random values.
The `generate` function returns a typed OCaml value directly — no extraction
step is needed. When executed, Hegel generates random inputs matching the
generator's specification. Running `./example.exe` produces output like:

```
called with 0
called with -18588
called with -672780074
called with 32616
...
```

By default, `run_hegel_test` generates 100 random inputs. Control this with
the `~test_cases` parameter: `run_hegel_test ~test_cases:500 ...`.

> **Note:** The Python SDK uses a `@hegel` decorator to mark test functions,
> and `.generate()` method calls to draw values. In OCaml there are no
> decorators, so you call `run_hegel_test` directly and use `generate gen`
> (a plain function call) inside the body.

## Running in a test suite

Hegel integrates with [Alcotest](https://github.com/mirage/alcotest):

```ocaml
open Hegel.Generators

let test_integers_bounded () =
  Hegel.Session.run_hegel_test ~name:"integers_bounded" ~test_cases:100
    (fun () ->
      let n = generate (integers ~min_value:0 ~max_value:200 ()) in
      assert (n < 50))  (* this will fail! *)

let () =
  Alcotest.run "example"
    [("properties", [Alcotest.test_case "integers < 50" `Quick test_integers_bounded])]
```

Running the tests demonstrates failure detection and simplification. Hegel
will find the smallest counterexample — in this case, `n = 50`.

## Generating multiple values

Call `generate` multiple times to produce multiple values in a single test:

```ocaml
open Hegel.Generators

let test_multiple_values () =
  Hegel.Session.run_hegel_test ~name:"multiple_values" ~test_cases:50
    (fun () ->
      let n = generate (integers ()) in
      let s = generate (text ()) in
      assert (n = n);          (* integers are reflexive *)
      assert (String.length s >= 0))  (* strings have non-negative length *)
```

Because generation is imperative, you can generate values at any point —
including conditionally or inside loops. This is more flexible than
Hypothesis's `@given` decorator, which requires all inputs to be declared
upfront as function parameters.

## Filtering

Use `filter` on a generator for simple conditions:

```ocaml
open Hegel.Generators

let test_even_integers () =
  Hegel.Session.run_hegel_test ~name:"even_integers" ~test_cases:50
    (fun () ->
      let n = generate (filter (fun v -> v mod 2 = 0) (integers ())) in
      assert (n mod 2 = 0))
```

For conditions that depend on multiple generated values, use `assume` inside
the test body:

```ocaml
open Hegel.Generators
open Hegel.Client

let test_division () =
  Hegel.Session.run_hegel_test ~name:"division" ~test_cases:100
    (fun () ->
      let n1 = generate (integers ()) in
      let n2 = generate (integers ()) in
      assume (n2 <> 0);
      (* n2 is guaranteed non-zero here *)
      assert (n1 = (n1 / n2) * n2 + (n1 mod n2)))
```

> **Note:** Python's `assume()` is a free function imported from `hegel_sdk`.
> In OCaml it lives in `Hegel.Client.assume`.

## Transforming generated values

Use `map` to transform values after generation:

```ocaml
open Hegel.Generators

let test_string_integers () =
  Hegel.Session.run_hegel_test ~name:"string_integers" ~test_cases:50
    (fun () ->
      let s =
        generate
          (map
            (fun n -> string_of_int n)
            (integers ~min_value:0 ~max_value:100 ()))
      in
      assert (int_of_string s >= 0))
```

## Dependent generation

Because generation in Hegel is imperative, you can use earlier results to
configure later generators directly:

```ocaml
open Hegel.Generators

let test_list_with_valid_index () =
  Hegel.Session.run_hegel_test ~name:"list_index" ~test_cases:50
    (fun () ->
      let n = generate (integers ~min_value:1 ~max_value:10 ()) in
      let lst = generate (lists (integers ()) ~min_size:n ~max_size:n ()) in
      let index = generate (integers ~min_value:0 ~max_value:(n - 1) ()) in
      assert (index >= 0 && index < List.length lst))
```

> **Note:** In Hypothesis, this pattern requires `@composite` or `data()`.
> In Hegel it falls out naturally from the imperative `generate` style —
> just use earlier values to set bounds on later generators.

You can also use `flat_map` for dependent generation within a single
generator expression:

```ocaml
open Hegel.Generators

let test_list_with_valid_index_flatmap () =
  Hegel.Session.run_hegel_test ~name:"list_index_flatmap" ~test_cases:50
    (fun () ->
      let n, lst =
        generate
          (flat_map
            (fun n ->
              map (fun lst -> (n, lst))
                (lists (integers ()) ~min_size:n ~max_size:n ()))
            (integers ~min_value:1 ~max_value:5 ()))
      in
      assert (List.length lst = n))
```

## What you can generate

Hegel provides generators for all common data types.

### Primitive types

```ocaml
booleans ()
(* Produces: true or false — type: bool generator *)

integers ~min_value:(-100) ~max_value:100 ()
(* Produces: integer values, optionally bounded — type: int generator *)

floats ~min_value:0.0 ~max_value:1.0 ~allow_nan:false ~allow_infinity:false ()
(* Produces: floating-point numbers — type: float generator *)
(* allow_nan defaults to true only when no bounds are set *)
(* allow_infinity defaults to true when at most one bound is set *)

text ~min_size:0 ~max_size:100 ()
(* Produces: Unicode strings — type: string generator *)

binary ~min_size:0 ~max_size:64 ()
(* Produces: byte strings — type: string generator *)
```

### Constants and choices

```ocaml
just 42
(* type: int generator — always produces 42 *)

sampled_from [1; 2; 3]
(* Picks uniformly from the given list — type: int generator *)

from_regex "^[a-z]{3,8}$" ()
(* Produces: strings matching the regex — type: string generator *)
```

### Collections

```ocaml
lists (integers ~min_value:0 ~max_value:100 ()) ~min_size:1 ~max_size:10 ()
(* Produces: int list — a list of integers *)

hashmaps
  (integers ~min_value:0 ~max_value:999 ())  (* keys: int generator *)
  (text ~max_size:20 ())                      (* values: string generator *)
  ~min_size:0 ~max_size:5 ()
(* Produces: (int * string) list — a list of key-value pairs *)

tuples2 (integers ()) (text ())
(* Produces: (int * string) — a 2-element tuple *)

tuples3 (booleans ()) (integers ()) (text ())
(* Produces: (bool * int * string) — a 3-element tuple *)
```

### Combinators

```ocaml
one_of [integers (); map float_of_int (integers ())]
(* Picks from one of multiple generators *)

optional (text ())
(* Produces: string option — None or Some value *)

map f gen          (* transform: apply f to each generated value *)
flat_map f gen     (* chain: f receives a value and returns a new generator *)
filter pred gen    (* keep only values satisfying pred *)
```

### Formats and addresses

```ocaml
emails ()          (* Email address strings *)
urls ()            (* URL strings *)
domains ()         (* Domain name strings *)
dates ()           (* ISO 8601 date strings: YYYY-MM-DD *)
times ()           (* Time strings *)
datetimes ()       (* ISO 8601 datetime strings *)
ip_addresses ()    (* IPv4 or IPv6 address strings *)
```

## Debugging with note()

Use `Hegel.Client.note` to print debug information. Messages appear only when
Hegel replays the minimal failing example:

```ocaml
open Hegel.Generators
open Hegel.Client

let test_something () =
  Hegel.Session.run_hegel_test ~name:"note_example" ~test_cases:100
    (fun () ->
      let x = generate (integers ()) in
      let y = generate (integers ()) in
      note (Printf.sprintf "trying x=%d, y=%d" x y);
      assert (x + y <> x - y || x = 0 || y = 0))
```

## Guiding generation with target()

Use `Hegel.Client.target` to guide Hegel toward interesting values:

```ocaml
open Hegel.Generators
open Hegel.Client

let test_optimization () =
  Hegel.Session.run_hegel_test ~name:"optimization" ~test_cases:1000
    (fun () ->
      let x =
        generate
          (floats ~min_value:0.0 ~max_value:10000.0
             ~allow_nan:false ~allow_infinity:false ())
      in
      target x "maximize_x";
      assert (x < 9999.0))
```

`target value label` sends a floating-point score to the Hegel engine, which
uses it to guide subsequent generation toward higher scores — making it more
likely to find boundary cases.

## Type-directed derivation

Add `ppx_hegel_generator` to your dune file:

```
(executable
 (name my_tests)
 (libraries hegel)
 (preprocess (pps ppx_hegel_generator)))
```

Then annotate types with `[@@deriving generator]`:

```ocaml
type point = { x : int; y : int } [@@deriving generator]
type color = Red | Green | Blue [@@deriving generator]

let () =
  Hegel.Session.run_hegel_test ~name:"derived_test" ~test_cases:100 (fun () ->
    let p = point_generator () in
    let c = color_generator () in
    assert (p.x = p.x);
    ignore c)
```

The PPX synthesises `<type>_generator : unit -> <type>` functions for records,
variants, type aliases, and nested types. Supported field types: `int`, `bool`,
`float`, `string`, `t list`, `t option`, tuples, and named types.

## Next steps

- Run `just docs` to build the full odoc API documentation. The generated HTML is
  written to `_build/default/_doc/_html/hegel/`.
- Browse the [`examples/`](../examples/) directory for runnable programs.
- Read the [Hypothesis documentation](https://hypothesis.readthedocs.io/) for deeper
  background on property-based testing strategies.
