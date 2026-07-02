# Changelog

## 0.8.0 - 2026-07-02

This release reexports many functions from the `Client` module to the `Hegel` module.
The `Client` module has been renamed to `Internal`. Hegel tests should only use the 
`Hegel`, `Hegel.Generators`, and `Hegel.Stateful` modules.

## 0.7.6 - 2026-07-01

Adds database key as an optional argument to `Hegel.run_hegel_test`. If `ppx_hegel_test` 
is used, the key "file_name:test_name" is automatically passed in by let%hegel_test.

## 0.7.5 - 2026-06-30

Add support for using ppx_hegel_test with OCaml 5.3+.

## 0.7.4 - 2026-06-30

This release publishes Hegel for OCaml to the opam repository, with the native 
Hegel engine (`libhegel`) bundled into the package. Installing through opam also
installs the prebuilt `libhegel` matching your platform.

## 0.7.3 - 2026-06-26

This patch bumps our pinned libhegel ([hegel-rust](hegeldev/hegel-rust)) from [0.23.0](https://github.com/hegeldev/hegel-rust/releases/tag/v0.23.0) to [0.23.1](https://github.com/hegeldev/hegel-rust/releases/tag/v0.23.1).

## 0.7.2 - 2026-06-24

Fixes formatting when reporting multiple failures.

## 0.7.1 - 2026-06-24

This patch bumps our pinned libhegel ([hegel-rust](hegeldev/hegel-rust)) from [0.21.2](https://github.com/hegeldev/hegel-rust/releases/tag/v0.21.2) to [0.23.0](https://github.com/hegeldev/hegel-rust/releases/tag/v0.23.0).

## 0.7.0 - 2026-06-22

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

## 0.6.3 - 2026-06-19

This patch bumps our pinned libhegel ([hegel-rust](hegeldev/hegel-rust)) from [0.19.2](https://github.com/hegeldev/hegel-rust/releases/tag/v0.19.2) to [0.21.2](https://github.com/hegeldev/hegel-rust/releases/tag/v0.21.2).

## 0.6.2 - 2026-06-18

This patch fixes a bug that caused stateful test generation to terminate early.

## 0.6.1 - 2026-06-17

This patch bumps our pinned libhegel ([hegel-rust](hegeldev/hegel-rust)) from [0.19.1](https://github.com/hegeldev/hegel-rust/releases/tag/v0.19.1) to [0.19.2](https://github.com/hegeldev/hegel-rust/releases/tag/v0.19.2).

## 0.6.0 - 2026-06-17

This release makes Hegel print the values it drew as s-expressions when a test fails. 
On the final replay of a failing property, each draw is reported as`name = value`. 
Inside a `let%hegel_test`, each draw is labeled with the variable it was bound to:

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
binding is named `draw_1`, `draw_2`, …. The label can also be passed explicitly.

**Generators are now `('a, 'p) generator`.** The phantom `'p` is `printable`
when the generator carries a printer and `unprintable` otherwise. 

**`map`, `flat_map`, `sampled_from`, and `just` are unprintable by default.** 
Draw from them with `draw_silent` (no printing).
```ocaml
(* before *)
let v = Hegel.draw tc (sampled_from [ 10; 20; 30 ])

(* after — not printed *)
let v = Hegel.draw_silent tc (sampled_from [ 10; 20; 30 ])
```

Attach a printer with the new `with_printer` to make a printable generator.
Draw from them with `draw`. `with_printer` takes any `'a -> Sexp.t`.

```ocaml
(* after — printed on failure *)
let sexp_of_int n = ...

let v =
  Hegel.draw tc (with_printer sexp_of_int (sampled_from [ 10; 20; 30 ]))
```

**`[@@deriving generator]` is now `[@@deriving hegel_generator]`, and emits a generator
value.** The deriver previously produced a `test_case -> t` function; it now
produces a `(t, unprintable) generator` value named `<t>_generator`.

```ocaml
type point = { x : int; y : int } [@@deriving hegel_generator]

(* before: let p = point_generator tc *)
let p = Hegel.draw_silent tc point_generator
```

To print a value from a derived generator:

```ocaml
type point = { x : int; y : int } [@@deriving hegel_generator]

let sexp_of_point p = ...

let p = Hegel.draw tc (Hegel.with_printer sexp_of_point point_generator)
```
                                                      
For a given type `t`, [`ppx_sexp_conv`](https://github.com/janestreet/ppx_sexp_conv) can be used to automatically 
derive `sexp_of_<t>`.

Stateful tests gain a `?sexp_of` argument on `Variables.create`; when given, each
variable that is drawn or consumed prints as `v<id> = <sexp>` on a failing
replay.

## 0.5.5 - 2026-06-16

This patch fixes a bug where `note` ignored the `verbosity` setting.

## 0.5.4 - 2026-06-15

Adds `with_stateful_step_count` to allow controlling the number of steps in a stateful test
and [swarm testing](https://users.cs.utah.edu/~regehr/papers/swarm12.pdf) to stateful tests. Internal changes to stateful test generation.

## 0.5.3 - 2026-06-15

This patch bumps our pinned libhegel ([hegel-rust](hegeldev/hegel-rust)) from [0.18.0](https://github.com/hegeldev/hegel-rust/releases/tag/v0.18.0) to [0.19.1](https://github.com/hegeldev/hegel-rust/releases/tag/v0.19.1).

## 0.5.2 - 2026-06-12

This release updates the bindings to libhegel 0.18.0 and has internal control 
flow refactors.

## 0.5.1 - 2026-06-12

Make reporting multiple failures false by default.

## 0.5.0 - 2026-06-09

This release replaces the hegel-core Python engine with the `libhegel` Rust engine, 
called in-process via ctypes. There is no longer a subprocess, socket,
or wire protocol. `libhegel` is located (or downloaded) at runtime and `dlopen`ed.

The breaking change is the removal of the `Hegel.Protocol`, `Hegel.Connection`, and
`Hegel.Session` modules.

## 0.4.2 - 2026-05-29

Implements reproducing a test case with failure blobs.
When a test fails, the following is outputted:

```
<some exception here>
...
[hegel] To replay the failure, add to your test: [@@failure_blobs [ "<blob string>"; ... ]]
```

To replay:

```ocaml
let%hegel_test my_test tc = body
[@@failure_blobs [ "<blob string>"; ... ]]
```

## 0.4.1 - 2026-05-27

Fixes uniqueness of hashmap keys and lists when unique=true not being preserved after transformation.

## 0.4.0 - 2026-05-27

This release changes the public API for running Hegel tests to a new `let%hegel_test` PPX extension.

```ocaml
(* before *)
let my_invariant =
  Hegel.run_hegel_test (fun tc -> ...)

(* after *)
let%hegel_test my_invariant tc = ...
```

The top-level `Hegel.run_hegel_test` has been removed from the public API.
Existing callers should migrate to `let%hegel_test`. The generated function
remains directly callable as an ordinary `unit -> unit`, so it works with any
test runner.

Per-test settings can be supplied with the `[@@settings ...]` attribute:

```ocaml
let%hegel_test my_invariant tc = ...
[@@settings Hegel.settings ~test_cases:1000 ()]
```

This release also adds integration with Antithesis. Each `let%hegel_test`
emits an always assertion recording whether the property test passed or
failed. Outside Antithesis, the integration is a no-op.

### Discovering tests with `dune runtest`

If you use dune, add the PPX as an `inline_tests` backend so `dune runtest`
discovers every `let%hegel_test` automatically:

```
(library
 (name my_tests)
 (libraries hegel)
 (inline_tests (backend ppx_hegel_test))
 (preprocess (pps ppx_hegel_test)))
```

After that, `dune runtest` finds and runs every `let%hegel_test` in the
library.

## 0.3.19 - 2026-05-19

Add `Single_test_case` mode setting for running exactly one test case with no shrinking or replay. 
This mode is mostly intended for long-running workloads. Stateful testing will keep running rules indefinitely when in this mode.

## 0.3.18 - 2026-05-18

This patch bumps our pinned hegel-core from [0.8.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.8.0) to [0.9.1](https://github.com/hegeldev/hegel-core/releases/tag/v0.9.1).

## 0.3.17 - 2026-05-14

Internal checksum refactor.

## 0.3.16 - 2026-05-14

Add `Hegel.Stateful` module for rule based stateful testing.

## 0.3.15 - 2026-05-13

Reexports `settings` in the top level `Hegel` module.

## 0.3.14 - 2026-05-13

This release adds the `phase` enum and using `with_phases` to control which test lifecycle phases run.

## 0.3.13 - 2026-05-07

Bump our pinned hegel-core to [0.8.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.8.0).

## 0.3.12 - 2026-05-06

Raise conformance to hegel-core 0.7.0.

## 0.3.11 - 2026-05-06

Restore compatibility with OxCaml.

## 0.3.10 - 2026-05-05

Bump our pinned hegel-core from 0.5.0 to [0.7.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.7.0)

## 0.3.9 - 2026-05-05

`ip_addresses` now emits the protocol schema `{"type": "ip_address", "version": 4|6}` instead of different types per version (`{"type": "ipv4"}` / `{"type": "ipv6"}`)

## 0.3.8 - 2026-05-05

`one_of` no longer wraps each child schema in a tagged tuple. It now sends the raw children and relies on the new protocol contract in which the server emits `[index, value]` for `one_of` schemas.

This release also bumps our pinned hegel-core to [0.5.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.5.0), which ships the matching server-side protocol change:

> This release changes the `one_of` protocol request to return a tuple of `[index, value]`, rather than just `value`.
>
> — [v0.5.0](https://github.com/hegeldev/hegel-core/releases/tag/v0.5.0)

## 0.3.7 - 2026-04-29

Bump our pinned hegel-core version from `0.4.1` to [`0.4.14`](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.14).

## 0.3.6 - 2026-04-17

Properly handle flaky generation failures.

## 0.3.5 - 2026-04-16

Add support for building and running on the OxCaml compiler (`ocaml-variants.5.2.0+ox`). The `ppx_hegel_generator` PPX deriver now works on both standard OCaml and OxCaml by abstracting over ppxlib AST differences (labeled tuples, constructor modalities) via a platform-selected compatibility module.

## 0.3.4 - 2026-04-14

Handle `StopTest` in `collection_reject` so that unique non-basic lists terminate cleanly when the server's rejection limit is reached, instead of raising an unhandled protocol error.

## 0.3.3 - 2026-04-14

Restore timeouts to stream receive operations, which were dropped when switching from polling to `Condition.wait`.

## 0.3.2 - 2026-04-14

Add `characters()` generator, and add more options to `text()`

## 0.3.1 - 2026-04-08

Add support for new hegel-core protocol versions.

## 0.3.0 - 2026-03-30

This release moves the library over to use the Jane Street standard library instead of the Inria one.

## 0.2.0 - 2026-03-27

This release brings the OCaml library in line with hegel-rust, including a **breaking API change**: test functions now receive an explicit `test_case` parameter.

* **Breaking**: `draw`, `assume`, `note`, and `target` now take a `test_case` first argument. Test functions change from `unit -> unit` to `test_case -> unit`. PPX-derived generators change from `unit -> 'a` to `test_case -> 'a`.
* Switch from Unix socket to `--stdio` pipe-based transport, eliminating temp directories and socket polling.
* Add a background reader thread for packet dispatch, replacing the demand-driven reader model.
* Add `Settings` API with `health_check`, `verbosity`, `database`, `derandomize`, and `suppress_health_check` support.
* Auto-install `hegel-core` via `uv` when not found on PATH.
* Add server crash detection via a monitor thread.
* Bump supported protocol versions to 0.1–0.7.

## 0.1.3 - 2026-03-11

Add validation to generator arguments, and drop `name` from `run_hegel_test`.

## 0.1.2 - 2026-03-04

Split out generators.ml into multiple files.

## 0.1.1 - 2026-03-03

Remove some unnecessary code only present for testing.

