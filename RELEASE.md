RELEASE_TYPE: patch

This patch adds `functions`, `functions2`, and `functions3` in `Hegel.Generators` for 
generating functions. `functions2` and `functions3` generate curried two and three 
argument functions. Function generators are unprintable.

When a property over a generated function fails, Hegel prints the function application(s)
and its result(s). For example, a property that wrongly assumes applying a function twice 
returns the original value:

```ocaml
let%hegel_test involution tc =
  let f = draw_silent tc (functions ~sexp_of_arg:Int.sexp_of_t ~returns:(integers ()) ()) in
  let x = draw tc (integers ()) in
  assert (f (f x) = x)
```

fails with a concrete counterexample:

```
x = 0
f 0 = 1
f 1 = 1
```
