RELEASE_TYPE: patch

This patch adds `from_lark`, a generator for strings of a grammar written in the syntax of the [Lark](https://lark-parser.readthedocs.io/en/stable/grammar.html) parsing library. Generated strings are ones Lark's Earley parser accepts, and a failing string shrinks to a smaller string in the same grammar. Terminals can be drawn from generators with `~explicit`, which is required for a terminal the grammar `%declare`s. `%import`ed grammar files are found through `~import_paths`.

```ocaml
let arithmetic =
  {|
    ?start: sum
    ?sum: product | sum "+" product
    ?product: atom | product "*" atom
    ?atom: NUMBER | "(" sum ")"
    %import common.NUMBER
    %import common.WS
    %ignore WS
  |}

let%hegel_test parses tc =
  let expression = draw tc (from_lark arithmetic ~max_size:100 ()) in
  assert (Result.is_ok (My_parser.parse expression))
```

A test that never generates a valid input now always fails as unsatisfiable instead of passing.
