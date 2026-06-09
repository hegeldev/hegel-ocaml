# Changelog

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

