# Getting Started with Hegel for OCaml

This guide walks you through the basics of installing Hegel and writing your first tests.

## Install Hegel

Add `hegel` to your opam environment:

```bash
opam pin add hegel "git+ssh://git@github.com/hegeldev/hegel-ocaml.git"
```

Hegel calls the native `libhegel` shared library and locates (or downloads and
caches) it automatically at runtime, so there is no separate install step. See
the [README](../README.md#install-hegel) for how the library is resolved and the
supported platforms.

## Write your first test

You're now ready to write your first test. Add `hegel` to your dune library dependencies:

```
(library
 (name my_tests)
 (libraries hegel)
 (inline_tests (backend ppx_hegel_test))
 (preprocess (pps ppx_hegel_test)))
```

Then create a test file:

```ocaml
open Hegel
open Hegel.Generators

let%hegel_test integer_self_equality tc =
  let n = draw tc (integers ()) in
  assert (n = n)  (* integers are always equal to themselves *)
;;
```

Run it with `dune runtest`. You should see a `PASS` line for the test.

Let's look at what's happening in more detail. The `let%hegel_test name tc =
body` extension defines `name` as a `unit -> unit` function that runs `body`
many times (100, by default) and *also* registers it with Hegel's runtime so
`dune runtest` finds it. The `tc` parameter is a test case, which is passed
to `draw` to draw different values each iteration. The example above draws a
random integer and checks that it is equal to itself.

Next, try a test that fails:

```ocaml
let%hegel_test integer_under_fifty tc =
  let n = draw tc (integers ()) in
  assert (n < 50)  (* this will fail! *)
;;
```

This test asserts that any integer is less than 50, which is obviously incorrect.
Hegel will find a test case that makes this assertion fail, and then shrink it
to find the smallest counterexample — in this case, `n = 50`. `dune runtest`
will print a `FAIL` line, report the drawn value as `n = 50` (named after the
`let` binding), and exit non-zero.

To fix this test, you can constrain the integers you generate with `min_value`
and `max_value`:

```ocaml
let%hegel_test integer_under_fifty tc =
  let n = draw tc (integers ~min_value:0 ~max_value:49 ()) in
  assert (n < 50)
;;
```

Run `dune runtest` again. It should now pass.

## Use generators

Hegel provides a rich library of generators that you can use out of the box.
There are primitive generators, such as `integers`, `floats`, and `text`, and
combinators that allow you to make generators out of other generators, such as
`lists` and `tuples`.

For example, you can use `lists` to generate a list of integers:

```ocaml
let%hegel_test prepend_increases_length tc =
  let lst = draw tc (lists (integers ()) ()) in
  let initial_length = List.length lst in
  let extended = draw tc (integers ()) :: lst in
  assert (List.length extended > initial_length)
;;
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

When a test fails, Hegel replays the minimal failing example and prints each
value you drew as an s-expression, named after the `let` binding it was bound
to:

```ocaml
let%hegel_test addition_commutes tc =
  let x = draw tc (integers ()) in
  let y = draw tc (integers ()) in
  assert (x + y = y + x)
;;

(* On failure, prints:
     x = …
     y = …  *)
```

A value that is shadowed or drawn inside a loop is numbered (`x_1`, `x_2`, …),
and you can override the name with `~label`: `draw ~label:"seed" tc (integers ())`.

Some combinators hand the result type to your own code and so carry no printer —
`map`, `flat_map`, `sampled_from`, `just`, and generators from `[@@deriving
hegel]`. Either draw it with `draw_silent` (which prints nothing):

```ocaml
let parity = draw_silent tc (map (fun n -> n mod 2) (integers ())) 
```
or attach a printer with `with_printer`. The printer is any `'a -> Sexp.t`:
```ocaml
let parity =
  draw tc (with_printer (fun n -> Sexp.Atom (Int.to_string n))
             (map (fun n -> n mod 2) (integers ())))
```
If you have [`ppx_sexp_conv`](https://github.com/janestreet/ppx_sexp_conv) in your
`(preprocess (pps ...))`, `[%sexp_of: int]` is a shorthand for that printer:
```ocaml
let parity = draw tc (with_printer [%sexp_of: int] (map (fun n -> n mod 2) (integers ())))
```

You can also attach your own debug information with `note`:

```ocaml
let%hegel_test addition_commutes tc =
  let x = draw tc (integers ()) in
  let y = draw tc (integers ()) in
  note tc (Printf.sprintf "x + y = %d, y + x = %d" (x + y) (y + x));
  assert (x + y = y + x)
;;
```

Notes, like drawn values, only appear when Hegel replays the minimal failing
example.

## Change the number of test cases

By default Hegel runs 100 test cases. To override this, attach a
`[@@settings ...]` attribute to the test:

```ocaml
let%hegel_test integer_self_equality tc =
  let n = draw tc (integers ()) in
  assert (n = n)
[@@settings Hegel.settings ~test_cases:500 ()]
;;
```

## Learning more

- Run `just docs` to build the full odoc API documentation.
- Browse the [`examples/`](../examples/) directory for runnable programs.
- See `Client.settings` for more configuration options to customise how your test runs.
