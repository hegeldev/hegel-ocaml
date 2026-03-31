# Getting Started with Hegel for OCaml

This guide walks you through the basics of installing Hegel and writing your first tests.

## Prerequisites

You will need [`uv`](https://docs.astral.sh/uv/) installed and on your PATH.

## Install Hegel

Add `hegel` to your opam environment:

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

## Write your first test

You're now ready to write your first test. Add `hegel` to your dune library dependencies:

```
(executable
 (name my_tests)
 (libraries hegel))
```

Then create a test file:

```ocaml
open Hegel
open Hegel.Generators

let () =
  run_hegel_test (fun tc ->
    let n = draw tc (integers ()) in
    assert (n = n))  (* integers are always equal to themselves *)
```

Run it with `dune exec ./my_tests.exe`. You should see that this test passes.

Let's look at what's happening in more detail. `run_hegel_test` runs your test
many times (100, by default). The test function takes a `test_case` parameter,
which is passed to `draw` to draw different values. This test draws a random
integer and checks that it should be equal to itself.

Next, try a test that fails:

```ocaml
let () =
  run_hegel_test (fun tc ->
    let n = draw tc (integers ()) in
    assert (n < 50))  (* this will fail! *)
```

This test asserts that any integer is less than 50, which is obviously incorrect.
Hegel will find a test case that makes this assertion fail, and then shrink it
to find the smallest counterexample — in this case, `n = 50`.

To fix this test, you can constrain the integers you generate with `min_value`
and `max_value`:

```ocaml
let () =
  run_hegel_test (fun tc ->
    let n = draw tc (integers ~min_value:0 ~max_value:49 ()) in
    assert (n < 50))
```

Run the test again. It should now pass.

## Use generators

Hegel provides a rich library of generators that you can use out of the box.
There are primitive generators, such as `integers`, `floats`, and `text`, and
combinators that allow you to make generators out of other generators, such as
`lists` and `tuples`.

For example, you can use `lists` to generate a list of integers:

```ocaml
let () =
  run_hegel_test (fun tc ->
    let lst = draw tc (lists (integers ()) ()) in
    let initial_length = List.length lst in
    let extended = draw tc (integers ()) :: lst in
    assert (List.length extended > initial_length))
```

This test checks that prepending an element to a random list of integers should
always increase its length.

You can also define custom generators as regular functions. For example, say you
have a `person` record that you want to generate:

```ocaml
type person = { age : int; name : string }

let generate_person tc =
  let age = draw tc (integers ()) in
  let name = draw tc (text ()) in
  { age; name }
```

Note that you can feed the results of a `draw` to subsequent calls. For example,
say that you extend the `person` record to include a `driving_license` field:

```ocaml
type person = { age : int; name : string; driving_license : bool }

let generate_person tc =
  let age = draw tc (integers ()) in
  let name = draw tc (text ()) in
  let driving_license =
    if age >= 18 then draw tc (booleans ()) else false
  in
  { age; name; driving_license }
```

## Debug your failing test cases

Use `note` to attach debug information:

```ocaml
let () =
  run_hegel_test (fun tc ->
    let x = draw tc (integers ()) in
    let y = draw tc (integers ()) in
    note tc (Printf.sprintf "x + y = %d, y + x = %d" (x + y) (y + x));
    assert (x + y = y + x))
```

Notes only appear when Hegel replays the minimal failing example.

## Change the number of test cases

By default Hegel runs 100 test cases. To override this, pass `~settings`:

```ocaml
let () =
  run_hegel_test ~settings:(Client.settings ~test_cases:500 ()) (fun tc ->
    let n = draw tc (integers ()) in
    assert (n = n))
```

## Learning more

- Run `just docs` to build the full odoc API documentation.
- Browse the [`examples/`](../examples/) directory for runnable programs.
- See `Client.settings` for more configuration options to customise how your test runs.
