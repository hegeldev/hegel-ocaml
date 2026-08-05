# Hegel for OCaml

## Build Commands

```bash
# No setup step: libhegel is located (or downloaded + cached) at runtime.
just test        # Run tests with 100% coverage enforcement
just format      # Auto-format code with ocamlformat
just lint        # Check formatting (fails if unformatted)
just docs        # Build API documentation with odoc
just check       # Run lint + docs + test (the full CI check)
```

## Tooling

- **OCaml**: 5.2.1
- **Build system**: Dune 3.21.1
- **Test framework**: Alcotest 1.9.1
- **Code coverage**: bisect_ppx 2.8.3 (enforced at 100% via scripts/check-coverage.py)
- **Formatter**: OCamlFormat 0.28.1 (version pinned in .ocamlformat)
- **Documentation**: odoc 3.1.0
- **Package manager**: opam 2.1.5
- **PPX derivation**: ppxlib 0.35.0 (for `[@@deriving hegel_generator]`)

## Project Structure

```
lib/                         # Library source
  dune                       # Library build config (bisect_ppx instrumented)
  hegel.ml / hegel.mli       # Main module — re-exports the public API
  ffi/                       # ctypes bindings to native libhegel (NOT instrumented)
    ffi.ml                   # dlopen + 1:1 C-ABI wrappers; settings/run/test_case
                             #   handles; typed draws + string-generator handles
    loader.ml                # locate/download libhegel at runtime (env > sibling > release)
  internal.ml                # Test runner + run lifecycle + typed-draw wrappers on
                             #   top of Hegel_ffi.Ffi (the module CLAUDE calls "client")
  generators.ml              # Re-export shim: include the four generators_* modules
  generators_core.ml         # generator type; draw/draw_silent, map/flat_map/filter,
                             #   composite, span labels — the discriminated union
  generators_primitives.ml   # integers, booleans, floats, text, binary, just, formats
  generators_collections.ml  # lists, assoc_lists, hash_tables (+ the table-agnostic
                             #   hash_tables_core), and the collection protocol
  generators_combinators.ml  # sampled_from, one_of, tuples2/3/4
  generators_functions.ml    # functions/functions2/functions3: memoized
                             #   function generators (Claessen's show/shrink,
                             #   but no trie — the engine shrinks results)
  derive.ml                  # Runtime support for [@@deriving hegel_generator]
  stateful.ml                # Stateful testing: Rule.create + run over action sequences
  antithesis.ml              # Antithesis integration (emits an always-typed assertion)
  core/                      # Optional hegel.core sublibrary ((optional) in dune; NOT
    hegel_core.ml/.mli       #   instrumented). Core.Hashtbl hash_tables + pool helpers
                             #   and the sexp_diff require_equal renderer (set_sexp_diff)

ppx/                         # PPX rewriters and derivers
  dune                       # PPX library build configs; a rule generates
                             #   ppx_compat.ml from one variant below
  ppx_hegel_generator.ml     # Deriver: reads type decls, emits generator functions
  ppx_hegel_test.ml          # Expander: rewrites [let%hegel_test name tc = body]
                             # into a plain callable function (no registration,
                             # no runtime library — see Inline Test Integration below)
  ppx_compat_pre-53.ml       # AST compat shim for ppxlib < 0.36 (OCaml < 5.3)
  ppx_compat_post-53.ml      # AST compat shim for ppxlib >= 0.36 (OCaml >= 5.3)
  ppx_compat_oxcaml.ml       # AST compat shim for the OxCaml compiler
  test/                      # PPX E2E tests, package-attributed so opam-repo-ci runs them
    test_ppx_derive.ml       # PPX deriver E2E tests (package ppx_hegel_generator)
    test_ppx_hegel_test.ml   # ppx_hegel_test expander E2E tests (package ppx_hegel_test)
    expect_tests/            # ppx_expect tests (dev-only, disabled in release profile)
                             # (one of the three is copied to ppx_compat.ml = ppx_hegel_compat lib)

test/                        # hegel's own test suite (one executable: test_hegel,
  dune                       #   Alcotest, package hegel — runs under `-p hegel`;
                             #   no PPX preprocessing beyond the ppx_js_style linter)
  test_hegel.ml              # Top-level Alcotest runner
  test_helpers.ml            # Shared test utilities
  test_client.ml             # Internal config + run lifecycle tests (real engine)
  test_generators_*.ml       # Generator core / primitives / collections / combinators
  test_derive.ml             # Derive module runtime helper tests
  test_stateful.ml           # Stateful testing tests
  test_antithesis.ml         # Antithesis integration tests
  test_single_test_case.ml   # Single-case / failure-blob replay tests

docs/                        # Tutorial and guide documents
  getting-started.md         # Getting Started tutorial (OCaml translation)

examples/                    # Example programs demonstrating the library
  dune                       # Example executables build config
  basic_properties.ml        # Primitive generators: integers, booleans, floats
  collections.ml             # Collections and combinators: lists, filter, map
  real_world.ml              # Real-world scenario: sorted-merge property test
  derived_types.ml           # Derived generators via [@@deriving hegel_generator]
  higher_order.ml            # Function generators: functions/functions2/functions3

scripts/
  check-coverage.py          # Parses bisect-ppx-report, enforces 100%

README.md                    # Project overview, install, quick-start
```

## Architecture Overview

### Native backend (lib/ffi/ffi.ml)

There is no subprocess, socket, or wire protocol. The engine is the native
`libhegel` C library (from hegel-rust, header `hegel-c/include/hegel.h`), called
in-process via ctypes. `Hegel_ffi.Loader` resolves the shared library at runtime
(mirroring hegel-go): `$HEGEL_LIBHEGEL_PATH`, then a sibling
`../hegel-rust/target/{release,debug}/` checkout, then a SHA-256-verified
download from the hegel-rust GitHub release cached under
`~/.cache/hegel-ocaml/libhegel/<version>/` (opt out with
`HEGEL_LIBHEGEL_NO_DOWNLOAD=1`). `Hegel_ffi.Ffi` `dlopen`s that path and exposes
thin 1:1 wrappers: settings handles, the run lifecycle (`run_start`,
`next_test_case`, `run_result`, `run_free`), and per-test-case primitives — the
typed draws (`generate_integer`, `generate_boolean`, `generate_float`,
`generate_bytes`, `generate_string` + the `string_generator_*` handle
constructors, `generate_date`/`time`/`datetime`, `generate_ipv4`/`ipv6`), spans,
collections, pools, `target`, `mark_complete`. There is no CBOR: each value is
drawn by a dedicated typed call rather than a schema round-trip (this replaced the
removed `hegel_generate`/CBOR-schema path in libhegel 0.26.0). There is no
engine thread (removed in libhegel 0.30.1): `hegel_next_test_case` runs all
engine work between test cases on the calling thread, so its binding releases
the OCaml runtime lock for the call's duration. The `ffi` library is
deliberately NOT bisect_ppx-instrumented, keeping its
mechanical marshalling out of the 100%-coverage gate (no `[@coverage off]`).

`lib/protocol.ml`, `lib/connection.ml`, `lib/cbor/`, `lib/cbor_helpers.ml`, and
the old Python-subprocess install flow were removed in the native-backend and
typed-draw migrations.

### Dependencies: core-free main library + optional hegel.core

The `hegel` library depends on the stdlib plus `sexplib0` (printer type
`'a -> Sexplib0.Sexp.t`, the same type as `Core.Sexp.t`), `unix` (isatty for
color detection), `threads.posix`, ctypes/ipaddr/yojson/dune-site. `core`,
`core_unix`, and `sexp_diff` are NOT dependencies of the library: `core` and
`sexp_diff` are opam depopts that gate the `(optional)` sublibrary
`hegel.core` (`lib/core/`, module `Hegel_core`). Anywhere the library needs a
container or renderer a Jane Street type used to provide, the dependency is
refunctionalized — the code takes the operations as closures/parameters, and
each side instantiates them:
- pools: `values_core`/`resolve_draw_core` (find/remove/is_empty closures) ← `Make_pool`+`Int_table` (stdlib) / `Hegel_core` (Core.Hashtbl)
- hash tables: `hash_tables_core ~of_pairs ~sexp_of_t` ← `hash_tables` (Stdlib.Hashtbl) / `Hegel_core.hash_tables` (Hashtbl.Poly)
- dates/times: `dates_core`/`times_core`/`datetimes_core ~of_parts ~sexp_of` ← `dates`/`times`/`datetimes` (ISO 8601 strings)
- require_equal diff: `Internal.set_diff_renderer` hook ← default prints both values (`-`/`+`, red/green); `Hegel_core.set_sexp_diff ()` installs the `sexp_diff` two-column renderer

The test suite still links `core`/`core_unix` (test-only dependencies; users
never install them). `lib/core/` is not bisect_ppx-instrumented (same
exclusion as ffi and the PPXes).

### Generator System (generators_core.ml + generators_{primitives,collections,combinators}.ml)

The generator type and combinators (`draw`, `map`, `flat_map`, `composite`, …)
live in `generators_core.ml`; the primitives, collections, and combinators are
split across the sibling `generators_*.ml` files. `generators.ml` is a thin shim
that `include`s all four so they surface as one `Hegel.Generators` module.

Generators are a discriminated union:
- **Leaf** — holds a `draw : test_case -> 'a` closure that performs a single typed engine draw (via one of the `Internal.generate_*` primitives). Calling `map` on a Leaf composes the closure in place (no extra span), since the engine already wraps every primitive draw in its own span.
- **Mapped** — wraps source + transform function (adds a `mapped` span).
- **FlatMapped** — wraps source + a function returning a generator. Evaluated recursively inside a `flat_map` span.
- **Filtered** — wraps source + predicate. Up to `max_filter_attempts` retries before `assume false`.
- **CompositeList** — lists of any element core. Uses the collection protocol (new_collection / collection_more) to generate elements one at a time.
- **Composite** — a `generate_fn` thunk run inside a labeled span; used by tuples, one_of, `lists ~unique`, and hash tables (all of which now always drive the collection protocol / draw sub-values directly — there is no schema fast path).
- **Values** — the engine-pool core behind `Stateful.Pool`. Refunctionalized: it stores the table's `find`/`remove`/`is_empty` closures, not a concrete hashtable. `Make_pool (Tbl : Stdlib.Hashtbl.S with type key = int)` (doc-hidden, with the ready-made `Int_table`) closes `values_core`/`resolve_draw_core` over a stdlib table; the optional `hegel.core` library closes the same primitives (via `Ppx_internal`) over `Core.Hashtbl`. `hash_tables` follows the same strategy at the API level: `hash_tables_core ~of_pairs ~sexp_of_t` is table-agnostic, `hash_tables` closes it over `Stdlib.Hashtbl`, `Hegel_core.hash_tables` over `Core.Hashtbl.Poly`.
- **Function** — a generated function (`functions`/`functions2`/`functions3`). `build ~name` returns a fresh per-test-case memoized function that draws each result from `returns` on first application (memoized on the argument via structural hash/equality — a polymorphic `Stdlib.Hashtbl` — so `sexp_of_arg` is display-only and an omitted one shows `<opaque>` without collapsing the key) and shows applied pairs as `name arg = result` via `note` on the final replay. Only *top-level* applications print — a pair applied at draw depth > 0 (inside a span) is suppressed, like a nested draw. A distinct core so `draw_silent_named` / `draw_named` can thread the draw-site binding name into the function (see the PPX note below); the name threads even when the function is drawn nested. Result draws are wrapped in a `Labels.function_result` span.

### Inline Test Integration (ppx/ppx_hegel_test.ml)

The `ppx_hegel_test` PPX rewrites `let%hegel_test name tc = body` into a
single top-level item: `let name = fun () -> Hegel.run_hegel_test ... (fun tc
-> body)`. That's it — `name` is an ordinary `unit -> unit` value with no
registration, no runtime library, and no side effect at module init. Hegel
has no test runner of its own and no `(inline_tests (backend ...))` stanza:
the project's own tests wire each `let%hegel_test`-produced function into
whatever test framework the project already uses (see `examples/*.ml`, which
each end with a plain `let () = test_foo (); test_bar (); ...`, and
`ppx/test/test_ppx_derive.ml` / `test_ppx_hegel_test.ml`, which build an
`Alcotest.test_case` list by hand). `dune runtest` then works exactly like it
does for any other test executable in that framework — there is nothing
hegel-specific to integrate.

Within the body the PPX also injects binding names into draws: `let x = draw tc g`
becomes `draw_named ~label:"x" ~repeatable:.. tc g`, and `let x = draw_silent tc g`
becomes `draw_silent_named ~name:"x" tc g`. Both target hidden entry points, keeping
`~repeatable`/`~name` off the public `draw`/`draw_silent` (the `draw`→`draw_named`
precedent). The `~name` is only meaningful for a function generator (`Function` core),
where it labels the shown `x arg = result` pairs; it is ignored for every other
generator, and attaches at the draw site (so it works through an intermediate
`let g = functions ..; let f = draw_silent tc g` binding). Precedence: an explicit
`?name` on `functions` always wins, else the draw-site binding name, else `"function"`.
A function made printable (via `with_printer`) is drawn with
`draw`; `draw_named` threads the label the same way (even when nested) and prints
the usual `x = value` line — the function renders as `<fun>` through its printer —
only at the top level, suppressing it when nested like any other draw.

Because the PPX only produces a callable and never calls it, a
`let%hegel_test` composes with any test framework: drop the produced
function into `Alcotest.test_case "name" `Quick name`, an OUnit test, a
`let%expect_test` body (see `ppx/test/expect_tests/`), or just call it
directly from `let () = ...`. Nothing about `let%hegel_test` opts a library
into an inline-tests backend or auto-discovery — the user always writes the
`dune runtest`-facing entry point themselves, exactly as they would for a
handwritten property test built on `Hegel.run_hegel_test` directly.

### Type-Directed Derivation (ppx/ + lib/derive.ml)

The `ppx_hegel_generator` PPX deriver synthesizes a `(<t>, unprintable) generator`
value named `<t>_generator` from type declarations annotated with
`[@@deriving hegel_generator]`. The generated code:

1. For **records**: generates each field by calling the appropriate primitive
   generator, then constructs the record value.
2. For **variants**: picks a constructor index via `sampled_from` (the
   engine's bounded-integer draw is non-uniform, over-weighting boundary
   indices), then generates arguments for the chosen constructor. A
   data-carrying variant wraps the index-plus-arguments draw in an
   `enum_variant` span (so the constructor choice and its fields shrink as one
   unit, matching the engine's own derived-enum generator); an all-nullary enum
   has no fields to group and is emitted as a bare `sampled_from` over the
   constructor values (spanless, like the engine).
3. For **type aliases**: delegates to the generator for the aliased type.
4. For **nested types**: draws `<type>_generator` via `draw_silent` (user-defined
   generators must exist in scope).

Records and aliases emit a `test_case -> t` field-drawing thunk wrapped with
`Hegel.Generators.composite` (a `fixed_dict` span); variants return a complete
generator directly (see above). Either way the result is an
`(t, unprintable) generator` value with no printer attached, so a bare
`[@@deriving hegel_generator]` always compiles. The
`Hegel.Derive` module provides runtime helpers for option and list types; it is
re-exported doc-hidden (the standard public-but-invisible PPX-runtime pattern)
because generated code in user projects calls it.

**Usage example:**

```ocaml
(* In your dune file, add:
     (preprocess (pps ppx_hegel_generator ppx_hegel_test)) *)

type point = { x : int; y : int } [@@deriving hegel_generator]
type color = Red | Green | Blue [@@deriving hegel_generator]
type entity = { name : string; tag : int option; active : bool }
[@@deriving hegel_generator]

(* Use inside a let%hegel_test body. The derived generators are
   (t, unprintable) generator values, drawn with draw_silent: *)
let%hegel_test derived_types_smoke tc =
  let p = Hegel.draw_silent tc point_generator in
  let c = Hegel.draw_silent tc color_generator in
  let e = Hegel.draw_silent tc entity_generator in
  (* p.x, p.y are ints; c is Red|Green|Blue; e has typed fields *)
  ignore (p, c, e)
;;

(* let%hegel_test only produces a callable; wire it into your own test
   framework, e.g. Alcotest.test_case "derived_types_smoke" `Quick
   derived_types_smoke, or just call it directly. *)
```

To print a derived value on a failing replay, add `[@@deriving sexp_of]` and draw
through `with_printer`:
`Hegel.draw tc (Hegel.with_printer sexp_of_point point_generator)`.

**Supported field types:**
- `int` — integers over the full native `int` range (same default as `integers ()`)
- `bool` — booleans
- `float` — finite floats (NaN and infinity disabled)
- `string` — text strings
- `t list` — lists of derived elements (engine-driven length via the collection protocol, like `lists`)
- `t option` — `Some v` or `None`
- Named types `t` — draws `t_generator` via `draw_silent` (must be in scope)
- Tuples `(t1 * t2 * ...)` — generates each component

### Collection Protocol

Lists, sets, and hashmaps generate elements one at a time via an engine-side
collection handle (there is no whole-collection schema draw):
1. `new_collection` → engine assigns a collection name
2. `collection_more` → engine returns true/false (should we generate another element?)
3. `collection_reject` → undo the last element (used by filter)

### Entry point

The engine runs in-process, so there is no subprocess or session to manage.
The public entry point is `Hegel.run_hegel_test ?settings ?test_location
test_fn` — `Internal.run_hegel_test`, which is `Internal.run_test` with [settings]
defaulting to `default_settings ()`. The `let%hegel_test` PPX targets the
doc-hidden `Hegel.run_hegel_test_ppx` — a thin wrapper that sets `~from_ppx:true`
on `Internal.run_hegel_test` — so the PPX-vs-plain signal never appears on the
public `run_hegel_test`. The `[@@failure_blobs ...]` record/replay workflow is
supported: the PPX forwards the listed blobs as `~failure_blobs`, which replays
the first blob as a standalone deterministic case (pair it with
`with_print_blob false` to suppress the `rerun with:` line that failing runs
print by default). `from_ppx` selects that line's syntax: a
`[@@failure_blobs [...]]` attribute under the PPX, a `~failure_blobs:[...]`
argument for a plain `run_hegel_test` caller. For persisting and replaying
failing examples across runs, use `database` / `database_key`.

### Test Runner (client.ml)

`run_test` builds an `Ffi.settings` from the OCaml settings, calls
`Ffi.run_start`, then loops on `Ffi.next_test_case` until it returns `None`. Each
test case handle is wrapped in a `test_case` record and passed to the user's function. 
The client controls when a final run occurs. Exceptions map to
`Ffi.mark_complete` statuses: VALID, INVALID (`Assume_rejected`/`Flaky_strategy`),
OVERRUN (`Data_exhausted` from a `Stop_test` during a primitive), INTERESTING
(any other exception, with a location-derived origin from `extract_origin`).
Interesting exceptions are captured by origin so the final-replay exception is
re-raised; after the loop, `Ffi.run_result` failures are raised (single) or
aggregated into a "Multiple failures" report. `run`/`settings` handles are freed
in an `Exn.protect ~finally`.

## Key Patterns and Conventions

### Documentation

- All public types, functions, exceptions, and constants use `(** ... *)` doc comments for odoc
- `just docs` must build with zero warnings — this is enforced in CI
- Parameter descriptions live inline in the first sentence when names are self-explanatory

### Testing

- Every lib module has a corresponding `test/test_<module>.ml`
- Unit tests use socketpair-based fake engines to avoid depending on the real hegel binary
- End-to-end tests (tagged `_e2e`) require the real binary and live under the same test file
- `test/` must build under `-p hegel` (opam-repo-ci runs it): plain Alcotest
  functions calling `Hegel.run_hegel_test`, no `let%hegel_test`, no PPX beyond
  the `ppx_js_style` linter. White-box tests use the doc-hidden `(**/**)`
  re-exports `Hegel.{Internal,Antithesis}`; `Hegel.Derive` is
  doc-hidden too because PPX-generated code calls it. Only `Generators`,
  `Stateful`, and the values/types directly under `Hegel` are documented API
- PPX E2E tests live under `ppx/test/`, attributed via `(package ...)` to
  `ppx_hegel_generator` (`test_ppx_derive.ml`) and `ppx_hegel_test`
  (`test_ppx_hegel_test.ml`) so `dune runtest -p <pkg>` runs them; each builds
  its own `Alcotest.test_case` list from the PPX-produced functions, same as
  any other consumer would. `ppx/test/expect_tests/` stays
  package-less and dev-only (`enabled_if (<> %{profile} release)`)
- 100% branch and line coverage is mandatory — no exceptions, no `[@coverage off]`

### Error Handling

- `Internal.Assume_rejected` — raised by `assume false`; mapped to `mark_complete INVALID`
- `Internal.Data_exhausted` — raised when StopTest is received; skips `mark_complete`
- `Connection.Request_error` — raised on protocol-level errors from the engine

### Typed Draws (no schema)

There is no CBOR schema layer. Each generator draws its value through a dedicated
typed FFI call (`Hegel_ffi.Ffi` / `Internal.generate_*`):
- `integers` → `generate_integer ~min_value ~max_value` (i64 bounds; OCaml native int fits)
- `booleans` → `generate_boolean 0.5 None`
- `floats` → `generate_float ~min_value ~max_value ~allow_nan ~allow_infinity ~exclude_min ~exclude_max ~smallest_nonzero_magnitude` (width 64; unbounded ends are ±infinity)
- `binary` → `generate_bytes ~min_size ~max_size`
- `text` / `characters` → build a text `string_generator` handle (codec / codepoint bounds / categories / include-exclude chars) then `generate_string`; surrogates auto-excluded
- `from_regex` / `emails` / `urls` / `domains` → the matching `string_generator_*` handle + `generate_string`
- `dates` / `times` / `datetimes` → `generate_date`/`time`/`datetime` structs. The parts feed the refunctionalized builders `dates_core`/`times_core`/`datetimes_core` (`~of_parts` constructor + `~sexp_of` printer, labeled args); the public `dates`/`times`/`datetimes` close them over ISO 8601 strings (`YYYY-MM-DD`, `HH:MM:SS.ffffff` with the fraction always printed, joined by `T`). A typed date library plugs in its own `~of_parts` (no string parsing round-trip)
- `ip_addresses` → `generate_ipv4`/`generate_ipv6` raw bytes, rendered to strings by the `ipaddr` library (`Ipaddr.V4/V6.{of_octets_exn, to_string}`; RFC 5952 for v6)
- `sampled_from` → `generate_integer 0 (n-1)` then index into the values array
- `just` → a Leaf whose `draw` ignores the engine and returns the constant
- `one_of` / `optional` / tuples / `lists` / `hashmaps` → `Composite`/`CompositeList` cores that draw an index or drive the collection protocol, calling sub-generators' draws directly

String-generator handles are context-bound: built from `tc.context`, used for the
draw, and always freed (`Internal.with_string_generator`).

### Coverage Rules

- 100% line coverage is mandatory on library code
- `scripts/check-coverage.py` parses `bisect-ppx-report summary` output
- Unreachable engine-contract violations use `failwith "..."` (tested via unit tests on the transform)
- `[@coverage off]` annotations are never used
- Only the instrumented `hegel` library is measured; the `hegel_ffi` bindings, examples, and PPX code are not

## Lessons Learned

### PPX Deriver Implementation

1. **PPX generates `(t, unprintable) generator` values**: The deriver builds a
   `test_case -> t` field-drawing thunk and wraps it with
   `Hegel.Generators.composite`, yielding a `(t, unprintable) generator` value
   named `<t>_generator`. It carries no printer (the output type is the user's),
   so a bare `[@@deriving hegel_generator]` always compiles; draw it with `draw_silent`, or
   pair it with `[@@deriving sexp_of]` and `with_printer` to print on a failing
   replay. (Earlier revisions emitted a `test_case -> t` thunk drawn by calling
   it directly; the `composite` wrapper replaced that so derived generators
   compose with the other combinators.)

2. **PPX tests need a separate executable**: Because the PPX needs
   `(preprocess (pps ppx_hegel_generator))`, the test file using `[@@deriving hegel_generator]`
   must be in a separate `(test ...)` stanza from the main test suite. Both test
   executables are run by `dune runtest`.

3. **ppxlib.metaquot is essential**: The PPX uses `[%expr ...]` and `[%stri ...]`
   metaquot syntax for readable AST construction. This requires
   `(preprocess (pps ppxlib.metaquot))` in the PPX's own dune file.

4. **Runtime helpers in lib/derive.ml**: For option and list types, the PPX delegates
   to runtime helpers `Hegel.Derive.generate_option` and `Hegel.Derive.generate_list`
   (`Derive` is re-exported doc-hidden, exclusively for generated code). These live
   in the main library (not the PPX) so they're covered by bisect_ppx.

5. **Floats default to finite**: The PPX generates `floats ~allow_nan:false ~allow_infinity:false ()`
   to avoid NaN/infinity in derived types, which would cause issues in most user code.

### Documentation and Polish Stage

7. **Zero odoc warnings is enforced by fatal warnings in the dev profile**: the root
   `dune` file sets `(env (dev (odoc (warnings fatal))))`, so `dune build @doc` (and
   `just check-docs`) fails outright on any odoc warning (e.g. a bad reference), on cold
   and warm builds alike. (By default odoc warnings don't fail the build and dune's cache
   hides them on rebuilds; the recipe used to force a cold build by deleting
   `_build/default/_doc` and failing on any output, but that delete corrupted dune's
   incremental odoc state whenever sources had changed since the last doc build.) All lib
   modules must have
   `(** ... *)` doc comments on every public type, function, constant, and exception.
   References to non-public modules (e.g. `Internal`) must be code spans (`[Internal.note]`),
   not `{!...}` links — the target isn't in the doc tree, so the link can't resolve.

8. **odoc module-level comment must come first**: The module-level `(** ... *)` comment must appear
   before any `open` statements or definitions. odoc picks up only the first doc comment as the
   module doc. Comments placed after the first definition are treated as item-level docs.

9. **README under 200 lines**: The full API reference belongs in odoc comments, not the README.
   README should cover: what it is, installation, a quick-start example, a generator/combinator
   table, mention of the PPX deriver, project layout, and build commands. All API detail goes
   in `just docs` output.

10. **Getting Started tutorial in `docs/getting-started.md`**: Plain Markdown, not an odoc page.
    Reference it from README.md. Translate all Python library examples to idiomatic OCaml, adding
    short notes where the OCaml API differs (no decorator, no `.generate()` method, etc.).

11. **Five example programs cover the full surface area**: `basic_properties.ml` (primitives,
    assume, note), `collections.ml` (lists, map, flat_map, filter, sampled_from, hashmaps),
    `real_world.ml` (sorted-merge property test), `derived_types.ml` (PPX deriver),
    `higher_order.ml` (function generators). Each has a standalone `main`; derived_types needs
    a separate dune stanza with PPX preprocessing.

12. **opam not on PATH in shell spawned by `just`**: The `just` tool starts a fresh shell that
    does not source `.bashrc` or `.profile`. Fix: add
    `export PATH := env("HOME") + "/.opam/5.2.1/bin:" + env("PATH")` at the top of the
    justfile, and use `eval $(opam env)` inside recipes that need the full opam environment.
    The `export PATH` line in justfile is evaluated by `just` itself, not the shell.

### Good-Taste Audit

13. **OCamlFormat is the authority on doc comment placement**: OCaml has two valid placements for
    documentation comments — before an item (`(** doc *) type t = ...`) or after it
    (`type t = ... (** doc *)`). Both are accepted by odoc. However, **OCamlFormat enforces the
    trailing form** for `type` and `exception` declarations — it will revert any "before" placement
    on format. Trust the formatter; do not fight it. Trailing doc comments after type/exception
    declarations are the OCamlFormat-canonical style.

14. **`_foo` naming convention**: In OCaml, a leading `_` on a name signals "intentionally unused"
    and suppresses the unused-variable warning. Using `_foo` for a module-level binding that IS
    used (e.g. `let _session = ...` that is referenced throughout the module) is misleading and
    confusing. Only use `_foo` or `_` for genuinely unused bindings. The exception is ppxlib's
    `let _deriver = Deriving.add ...` pattern, where the value IS intentionally unused (the
    registration side-effect is what matters). Avoid `_my_foo` — `_deriver` or `_` are cleaner.

15. **`_foo` used immediately after binding is a genuine bad taste**: `let _msg = ...` then
    `let pairs = extract_dict _msg` — where the `_msg` binding is immediately used — is wrong.
    The `_` prefix should only appear on bindings that are structurally required but whose value
    is intentionally discarded. If you access the value, drop the underscore prefix.

16. **Trailing `(** ... *)` docs after `and` declarations in mutually recursive types**: For
    `type t = ... and u = ...`, OCamlFormat places the trailing doc after each `and` clause, not
    before. This is consistent with the single-type case. Do not attempt to restructure these.

### Code Review (Greybeard Pass)

17. **`Option.fold` is idiomatic OCaml for option-with-default-accumulator**: The pattern
    `Option.fold ~none:acc ~some:(fun x -> Some x) opt` is the standard OCaml way to say
    "if Some, replace the accumulator; if None, keep it." Don't replace it with a match — bisect_ppx
    treats `Option.fold` as a single coverage point, but a match creates two branches, one of which
    may be hard to cover in tests.

18. **Shared test helpers belong in `test/test_helpers.ml`**: Any utility function used across
    multiple test modules (e.g. `contains_substring`) should live in a shared helper module listed
    in the dune `(modules ...)` stanza. This avoids copy-paste and ensures consistent behavior.

19. **Or-patterns in match arms for deduplication**: When two match arms do the same thing with
    minor variation, use `(Some (Dead _) | None) as entry -> ...` and dispatch on the bound
    variable inside the arm body. This is cleaner than duplicating the entire block.

20. **`dune-project` license must match the actual LICENSE file**: The `(license ...)` field
    in `dune-project` is propagated to the generated `.opam` file. If these disagree with
    the actual `LICENSE` file, downstream tooling (opam, GitHub license detection) will show
    conflicting information. Always check that the declared license matches the file.
