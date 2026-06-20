RELEASE_TYPE: minor

This release reworks the stateful-testing value-pool API. The `Stateful.Variables`
module is renamed to `Stateful.Pool`. The `draw` and `consume` functions have been
replaced by two unprintable generators `values_consumed` and `values_reusable` 
(which can be made printable using `with_printer`). This lets pool picks compose
with the rest of the generator combinators (`map`, `filter`, `with_printer`, …) 
and be drawn through the normal `draw` / `draw_silent` path.

`Pool.create` no longer takes a `?sexp_of` argument.

```ocaml
(* before *)
let vars = Stateful.Variables.create ~sexp_of:Int.sexp_of_t tc in
Stateful.Variables.add vars id;
let reused = Stateful.Variables.draw vars in
let taken = Stateful.Variables.consume vars in

(* after *)
let vars = Stateful.Pool.create tc in
Stateful.Pool.add vars id;
let reused = Hegel.draw_silent tc (Stateful.Pool.values_reusable vars) in
let taken = Hegel.draw_silent tc (Stateful.Pool.values_consumed vars) in
(* to print a pick on a failing replay: *)
let taken = Hegel.draw tc (with_printer Int.sexp_of_t (Stateful.Pool.values_consumed vars)) in
```

Updating your stateful tests requires renaming `Variables` to `Pool`,
replacing `draw`/`consume` calls with a `draw`/`draw_silent` of the corresponding
generator, and dropping the `~sexp_of` argument to `create`.
