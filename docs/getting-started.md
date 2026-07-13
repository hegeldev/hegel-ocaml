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
  assert (n = n)
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
generators for collections, such as `lists` and `tuples`, and generator combinators,
such as `map` and `flat_map`.

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

You can also build up compound values by writing a plain function that takes the
test case and draws its parts. For example, say you have a `person` record that
you want to generate:

```ocaml
type person = { age : int; name : string }

let generate_person tc =
  let age = draw tc (integers ~min_value:0 ~max_value:120 ()) in
  let name = draw tc (text ()) in
  { age; name }
```

`generate_person` has type `test_case -> person`. It is *not* a `generator`
value, so you do not pass it to `draw` — you call it directly with the same `tc`,
and it draws its fields for you:

```ocaml
let%hegel_test person_has_nonnegative_age tc =
  let p = generate_person tc in
  assert (p.age >= 0)
;;
```

If you instead want a first-class `generator` value — one you can draw with
`draw` rather than call by hand — wrap the function with `composite`:

```ocaml
let person_generator = composite generate_person
(* person_generator : (person, unprintable) generator *)

let%hegel_test people_are_generatable tc =
  let p = draw_silent tc person_generator in
  assert (p.age >= 0)
;;
```

`composite` carries no printer (the value type is yours), so draw it with
`draw_silent`. To print it on a failing replay — or to feed it into combinators
like `lists` that expect a *printable* element generator — attach a printer with
`with_printer` (see [Debugging failures](#debug-your-failing-test-cases)).

## Debug your failing test cases

When a test fails, Hegel replays the minimal failing example and prints a
report: the test's name and source location, how many cases ran, the values you
drew (as s-expressions, named after their `let` binding), the exception, and a
copy-pasteable line that replays the exact case.

```ocaml
let%hegel_test reverse_is_identity tc =
  let xs = draw tc (lists (integers ()) ()) in
  assert (xs = List.rev xs)
;;
```

On failure this prints:

```
--- Failure: reverse_is_identity (test/my_tests.ml:1) ------------------
Falsified after 8 test cases (0 discarded):

  xs = (0 1)

Exception: File "test/my_tests.ml", line 3, characters 2-8: Assertion failed
rerun with: [@@failure_blobs "AXic..."]
```

`Falsified after N test cases (M discarded)` counts the cases that ran before
the failure (`M` of them were rejected, for example by `assume`). The final line
is a `[@@failure_blobs "..."]` attribute you can paste onto the test to replay
this exact case deterministically. On a terminal the header prints in red. Set
`HEGEL_COLOR=0` to disable color (or `1` to force it on). Add
`with_print_blob false` to your `[@@settings ...]` to omit the blob line.

A value that is shadowed or drawn inside a loop is numbered (`x_1`, `x_2`, …):

```ocaml
let%hegel_test all_draws_below_ten tc =
  for _ = 1 to 3 do
    let x = draw tc (integers ()) in
    assert (x < 10)
  done
;;

(* On failure, prints:
     x_1 = …
     x_2 = …
     x_3 = 10  *)
```

The same `let x` runs on each iteration, so Hegel disambiguates the draws as
`x_1`, `x_2`, `x_3` in draw order. You can override the name with `~label`:
`draw ~label:"y" tc (integers ())`.

Some combinators hand the result type to your own code and so carry no printer —
`map`, `flat_map`, `sampled_from`, `just`, and generators from `[@@deriving
hegel_generator]`. Either draw it with `draw_silent` (which prints nothing):

```ocaml
let parity = draw_silent tc (map (fun n -> n mod 2) (integers ()))
```
or attach a printer with `with_printer`. The printer is any `'a -> Sexp.t`. 
Note that `Sexp` requires `open Core`.
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

## Assert with `require` and `require_equal`

`assert` works, `require_equal` provides more information. It compares two values 
and prints a structural s-expression diff of them in the report. `-` lines appear only in the first value and `+` lines only in the second. It takes a printer (`'a -> Core.Sexp.t`)
for the values; build one from `Core`'s `sexp_of_t` functions, or with a
`[%sexp_of: ...]` from `ppx_sexp_conv`:

```ocaml
let%hegel_test reverse_is_identity tc =
  let xs = draw tc (lists (integers ()) ()) in
  require_equal tc (Core.List.sexp_of_t Core.Int.sexp_of_t) xs (List.rev xs)
;;
```

On failure the report body shows exactly which parts differ:

```
  xs = (0 1)
  require_equal: values differ (- lhs / + rhs):
  -(0 1)  +(1 0)
```

For a plain boolean check with a custom message, use `require`, which raises
`Failure msg` when the condition is false:

```ocaml
require tc ~msg:"list must stay sorted" (is_sorted xs)
```

## Stateful testing

`Hegel.Stateful` applies a random sequence of *rules* to a model and checks 
invariants after every step. Pass `?sexp_of_state` to `Stateful.run` to trace 
the model state through a failing sequence:

```ocaml
let push =
  Stateful.Rule.create ~name:"push" ~step:(fun tc stack ->
    draw tc (integers ~min_value:0 ~max_value:9 ()) :: stack)

let%hegel_test stack_stays_small tc =
  Stateful.run
    ~init:[]
    ~rules:[ push ]
    ~invariants:[ (fun stack -> assert (List.length stack <= 2)) ]
    ~sexp_of_state:(Core.List.sexp_of_t Core.Int.sexp_of_t)
    tc
;;
```

When a sequence fails, the report shows each step, the draws it made, the state 
after it, and which step broke the invariant:

```
  state = ()
  Step 1: push
    draw_1 = 0
  state = (0)
  Step 2: push
    draw_2 = 0
  state = (0 0)
  Step 3: push
    draw_3 = 0
  state = (0 0 0)
  Invariant 0 violated after step 3.
```

See the `Hegel.Stateful` API docs for invariants across multiple
rules and for value pools that let one rule act on data an earlier rule produced.

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
- Browse the [`examples/`](../examples/) directory for runnable tests.
