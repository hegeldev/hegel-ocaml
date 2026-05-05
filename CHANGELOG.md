# Changelog

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

