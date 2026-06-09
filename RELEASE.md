RELEASE_TYPE: minor

This release makes Hegel print the values it drew as s-expressions when a test fails. 
On the final replay of a failing property, each drawn value is reported as
`name = value`. Inside a `let%hegel_test`, the PPX names each value after the variable it
was bound to:

```ocaml
let%hegel_test sorted_after_insert tc =
  let xs = Hegel.draw tc (lists (integers ()) ()) in
  let x = Hegel.draw tc (integers ()) in
  assert (is_sorted (insert x (List.sort compare xs)))
;;

(* On failure, prints:
     xs = (3 1)
     x = 0  *)
```

A named draw that is shadowed or inside a loop is numbered
(`x_1`, `x_2`, …). An unnamed `draw` or a draw outside of a `let%hegel_test` 
binding is named `draw_1`, `draw_2`, …, and you can always pass `~label` 
explicitly.

**Generators are now `('a, 'p) generator`.** The phantom `'p` is `printable`
when the generator carries a printer and `unprintable` otherwise. 

**`map`, `flat_map`, `sampled_from`, and `just` are unprintable.** These hand the
result type to your own code, so Hegel cannot print it without help. Draw them
with the new `draw_silent` (no printing), or attach a printer with the new
`with_printer` and draw with `draw`:

```ocaml
(* before *)
let v = Hegel.draw tc (sampled_from [ 10; 20; 30 ])

(* after — not printed *)
let v = Hegel.draw_silent tc (sampled_from [ 10; 20; 30 ])

(* after — printed on failure *)
let v =
  Hegel.draw tc (with_printer [%sexp_of: int] (sampled_from [ 10; 20; 30 ]))
```

**`[@@deriving generator]` is now `[@@deriving hegel]`, and emits a generator
value.** The deriver previously produced a `test_case -> t` function; it now
produces a `(t, unprintable) generator` value named `<t>_generator`.

```ocaml
type point = { x : int; y : int } [@@deriving hegel]

(* before: let p = point_generator tc *)
let p = Hegel.draw_silent tc point_generator
```

To print a value from a derived generator:

```ocaml
type point = { x : int; y : int } [@@deriving hegel, sexp_of]

let p = Hegel.draw tc (Hegel.with_printer sexp_of_point point_generator)
```

Stateful tests gain a `?sexp_of` argument on `Variables.create`; when given, each
variable that is drawn or consumed prints as `v<id> = <sexp>` on a failing
replay.
