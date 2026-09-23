RELEASE_TYPE: minor

**OxCaml only:**
This release limits OxCaml mode requirements to concurrent state machines.

A generator that a concurrent rule body captures must be portable. Each
combinator has a portable version, which you select with `[@mode portable]`:

```ocaml
let keys = (map [@mode portable]) Key.of_int (integers ~min_value:0 ~max_value:99 ())
```

The primitive generators, such as `integers` and `text`, are already portable.
`[@mode portable]` requires `ppx_template`, which `ppx_hegel_test`,
`ppx_hegel_generator`, and `ppx_jane` also include. On upstream OCaml, the 
attribute has no effect.

`[@@deriving hegel_generator ~portable]` makes a derived generator portable.
It is a compile error on upstream OCaml.

Generators no longer cross portability. A generator that a portable closure
captures must now be annotated with `[@mode portable]`, and a derived one with 
`[@@deriving hegel_generator ~portable]`.

`just`, `sampled_from`, and `Stateful.Pool` now accept any element type.

**Both OxCaml and OCaml:**
A concurrent state machine must use the new `Stateful.Concurrent_pool`
instead of `Stateful.Pool`.
