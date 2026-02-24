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
      let n = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
      Printf.printf "called with %d\n%!" n;
      assert (n = n))  (* integers are always equal to themselves *)

let () = test_integers ()
```

Inside the test body you call `generate` on generators to produce random values.
When executed, Hegel generates random inputs matching the generator's
specification. Running `./example.exe` produces output like:

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
      let n = Hegel.Cbor_helpers.extract_int
                (generate (integers ~min_value:0 ~max_value:200 ())) in
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
      let n = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
      let s = Hegel.Cbor_helpers.extract_string (generate (text ())) in
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
      let n = Hegel.Cbor_helpers.extract_int
                (generate (filter
                  (fun v -> Hegel.Cbor_helpers.extract_int v mod 2 = 0)
                  (integers ()))) in
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
      let n1 = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
      let n2 = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
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
      let s = Hegel.Cbor_helpers.extract_string
                (generate (map
                  (fun v ->
                    `Text (string_of_int (Hegel.Cbor_helpers.extract_int v)))
                  (integers ~min_value:0 ~max_value:100 ()))) in
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
      let n = Hegel.Cbor_helpers.extract_int
                (generate (integers ~min_value:1 ~max_value:10 ())) in
      let lst = Hegel.Cbor_helpers.extract_list
                  (generate (lists (integers ()) ~min_size:n ~max_size:n ())) in
      let index = Hegel.Cbor_helpers.extract_int
                    (generate (integers ~min_value:0 ~max_value:(n - 1) ())) in
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
      let pair = generate (flat_map
        (fun n_v ->
          let n = Hegel.Cbor_helpers.extract_int n_v in
          map
            (fun lst_v -> `Array [lst_v; `Int (n - 1)])
            (lists (integers ()) ~min_size:n ~max_size:n ()))
        (integers ~min_value:1 ~max_value:5 ())) in
      let items = Hegel.Cbor_helpers.extract_list pair in
      let lst = Hegel.Cbor_helpers.extract_list (List.nth items 0) in
      let index = Hegel.Cbor_helpers.extract_int (List.nth items 1) in
      assert (index >= 0 && index < List.length lst))
```

## What you can generate

Hegel provides generators for all common data types.

### Primitive types

```ocaml
booleans ()
(* Produces: true or false *)

integers ~min_value:(-100) ~max_value:100 ()
(* Produces: integer values, optionally bounded *)

floats ~min_value:0.0 ~max_value:1.0 ~allow_nan:false ~allow_infinity:false ()
(* Produces: floating-point numbers *)
(* allow_nan defaults to true only when no bounds are set *)
(* allow_infinity defaults to true when at most one bound is set *)

text ~min_size:0 ~max_size:100 ()
(* Produces: Unicode strings *)

binary ~min_size:0 ~max_size:64 ()
(* Produces: byte strings *)
```

### Constants and choices

```ocaml
(* OCaml has no just() built-in, but you can wrap with map: *)
map (fun _ -> `Int 42) (booleans ())
(* or generate a constant directly via generate_from_schema *)

sampled_from [`Int 1; `Int 2; `Int 3]
(* Picks uniformly from the given list of CBOR values *)
```

> **Note:** The Python SDK provides `just(value)` and `tuples(...)` as
> top-level generators. The OCaml SDK does not expose these directly; use
> `map` and `flat_map` to compose dependent generators, and use multiple
> `generate` calls for tuple-like patterns.

### Collections

```ocaml
lists (integers ~min_value:0 ~max_value:100 ()) ~min_size:1 ~max_size:10 ()
(* Produces: lists of integers *)

hashmaps
  (integers ~min_value:0 ~max_value:999 ())  (* keys *)
  (text ~max_size:20 ())                      (* values *)
  ~min_size:0 ~max_size:5 ()
(* Produces: dictionaries as CBOR maps *)
```

### Combinators

```ocaml
map f gen          (* transform: apply f to each generated value *)
flat_map f gen     (* chain: f receives a value and returns a new generator *)
filter pred gen    (* keep only values satisfying pred *)
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
      let x = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
      let y = Hegel.Cbor_helpers.extract_int (generate (integers ())) in
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
      let x = Hegel.Cbor_helpers.extract_float
                (generate (floats ~min_value:0.0 ~max_value:10000.0
                             ~allow_nan:false ~allow_infinity:false ())) in
      target x "maximize_x";
      assert (x < 9999.0))
```

`target value label` sends a floating-point score to the Hegel engine, which
uses it to guide subsequent generation toward higher scores — making it more
likely to find boundary cases.

## Full API Reference

Run `just docs` to build the full odoc API documentation. The output is
written to `_build/default/_doc/_html/hegel/`. Open `index.html` in your
browser for the complete reference.
