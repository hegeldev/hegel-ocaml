# Getting Started with Hegel for OCaml

## Install Hegel

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

The SDK requires the `hegel` CLI on your PATH:

```bash
pip install "git+https://github.com/hegeldev/hegel-core"
```

If you are working inside this repository, `just setup` handles the CLI
installation.

## Write your first test

Create `example.ml`:

```ocaml
open Hegel
open Hegel.Generators

let test_integers () =
  run_hegel_test ~test_cases:100
    (fun () ->
      let n = generate (integers ()) in
      Printf.printf "called with %d\n%!" n;
      assert (n = n))  (* integers are always equal to themselves *)

let () = test_integers ()
```

Inside the test body you call `generate` on generators to produce random values.
The `generate` function returns a typed OCaml value directly — no extraction
step is needed. When executed, Hegel generates random inputs matching the
generator's specification. If any assertion fails, Hegel shrinks the inputs to
a minimal counterexample.

By default, `run_hegel_test` generates **100 test cases**. Override this with
the `~test_cases` parameter: `run_hegel_test ~test_cases:500 ...`.

## Running in a test suite

Hegel integrates with [Alcotest](https://github.com/mirage/alcotest):

```ocaml
open Hegel
open Hegel.Generators

let test_integers_bounded () =
  run_hegel_test ~test_cases:100
    (fun () ->
      let n = generate (integers ~min_value:0 ~max_value:200 ()) in
      assert (n < 50))  (* this will fail! *)

let () =
  Alcotest.run "example"
    [("properties", [Alcotest.test_case "integers < 50" `Quick test_integers_bounded])]
```

When a test fails, Hegel shrinks the counterexample to the smallest value that
still triggers the failure — in this case, `n = 50`.

## Generating multiple values

Call `generate` multiple times to produce multiple values in a single test:

```ocaml
open Hegel
open Hegel.Generators

let test_multiple_values () =
  run_hegel_test ~test_cases:50
    (fun () ->
      let n = generate (integers ()) in
      let s = generate (text ()) in
      assert (n = n);
      assert (String.length s >= 0))
```

Because generation is imperative, you can generate values at any point —
including conditionally or inside loops.

## Filtering

Use `filter` on a generator for simple conditions:

```ocaml
open Hegel
open Hegel.Generators

let test_even_integers () =
  run_hegel_test ~test_cases:50
    (fun () ->
      let n = generate (filter (fun v -> v mod 2 = 0) (integers ())) in
      assert (n mod 2 = 0))
```

For conditions that depend on multiple generated values, use `assume` inside
the test body:

```ocaml
open Hegel
open Hegel.Generators

let test_division () =
  run_hegel_test ~test_cases:100
    (fun () ->
      let n1 = generate (integers ()) in
      let n2 = generate (integers ()) in
      assume (n2 <> 0);
      (* n2 is guaranteed non-zero here *)
      assert (n1 = (n1 / n2) * n2 + (n1 mod n2)))
```

Using bounds and `map` is more efficient than `filter` or `assume` because
they avoid generating values that will be rejected.

## Transforming generated values

Use `map` to transform values after generation:

```ocaml
open Hegel
open Hegel.Generators

let test_string_integers () =
  run_hegel_test ~test_cases:50
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
open Hegel
open Hegel.Generators

let test_list_with_valid_index () =
  run_hegel_test ~test_cases:50
    (fun () ->
      let n = generate (integers ~min_value:1 ~max_value:10 ()) in
      let lst = generate (lists (integers ()) ~min_size:n ~max_size:n ()) in
      let index = generate (integers ~min_value:0 ~max_value:(n - 1) ()) in
      assert (index >= 0 && index < List.length lst))
```

You can also use `flat_map` for dependent generation within a single
generator expression:

```ocaml
open Hegel
open Hegel.Generators

let test_list_with_valid_index_flatmap () =
  run_hegel_test ~test_cases:50
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

### Primitive types

```ocaml
booleans ()
(* bool — true or false *)

integers ~min_value:(-100) ~max_value:100 ()
(* int — optionally bounded *)

floats ~min_value:0.0 ~max_value:1.0 ~allow_nan:false ~allow_infinity:false ()
(* float — floating-point numbers *)

text ~min_size:0 ~max_size:100 ()
(* string — Unicode strings *)

binary ~min_size:0 ~max_size:64 ()
(* string — byte strings *)
```

### Constants and choices

```ocaml
just 42
(* int generator — always produces 42 *)

sampled_from [1; 2; 3]
(* int generator — picks uniformly from the given list *)
```

### Collections

```ocaml
lists (integers ~min_value:0 ~max_value:100 ()) ~min_size:1 ~max_size:10 ()
(* int list *)

hashmaps
  (integers ~min_value:0 ~max_value:999 ())
  (text ~max_size:20 ())
  ~min_size:0 ~max_size:5 ()
(* (int * string) list — key-value pairs *)

tuples2 (integers ()) (text ())
(* (int * string) *)

tuples3 (booleans ()) (integers ()) (text ())
(* (bool * int * string) *)
```

### Combinators

```ocaml
one_of [integers (); map float_of_int (integers ())]
(* picks from one of multiple generators *)

optional (text ())
(* string option — None or Some value *)

map f gen          (* transform: apply f to each generated value *)
flat_map f gen     (* chain: f receives a value and returns a new generator *)
filter pred gen    (* keep only values satisfying pred *)
```

### Formats and patterns

```ocaml
emails ()          (* email address strings *)
urls ()            (* URL strings *)
domains ()         (* domain name strings *)
dates ()           (* ISO 8601 date strings: YYYY-MM-DD *)
times ()           (* time strings *)
datetimes ()       (* ISO 8601 datetime strings *)
ip_addresses ()    (* IPv4 or IPv6 address strings *)
from_regex "^[a-z]{3,8}$" ()  (* strings matching the regex *)
```

## Debugging with note()

Use `note` to print debug information. Messages appear only when Hegel replays
the minimal failing example:

```ocaml
open Hegel
open Hegel.Generators

let test_something () =
  run_hegel_test ~test_cases:100
    (fun () ->
      let x = generate (integers ()) in
      let y = generate (integers ()) in
      note (Printf.sprintf "trying x=%d, y=%d" x y);
      assert (x + y = y + x))  (* commutativity — always true *)
```

## Guiding generation with target()

Use `target` to guide Hegel toward interesting values, making it more likely to
find boundary failures:

```ocaml
open Hegel
open Hegel.Generators

let test_optimization () =
  run_hegel_test ~test_cases:1000
    (fun () ->
      let x =
        generate
          (floats ~min_value:0.0 ~max_value:10000.0
             ~allow_nan:false ~allow_infinity:false ())
      in
      target x "maximize_x";
      assert (x <= 9999.0))
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
open Hegel

type point = { x : int; y : int } [@@deriving generator]
type color = Red | Green | Blue [@@deriving generator]

let () =
  run_hegel_test ~test_cases:100 (fun () ->
    let p = point_generator () in
    let c = color_generator () in
    assert (p.x = p.x);
    ignore c)
```

The PPX synthesises `<type>_generator : unit -> <type>` functions for records,
variants, type aliases, and nested types. Supported field types: `int`, `bool`,
`float`, `string`, `t list`, `t option`, tuples, and named types.

## Next steps

- Run `just docs` to build the full odoc API documentation.
- Browse the [`examples/`](../examples/) directory for runnable programs.
