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
    ffi.ml                   # dlopen + 1:1 C-ABI wrappers; settings/run/test_case handles
    loader.ml                # locate/download libhegel at runtime (env > sibling > release)
  cbor/cbor.ml               # Vendored CBOR encoder/decoder (RFC 7049; mirage/ocaml-cbor)
  cbor_helpers.ml            # Type-safe CBOR extractors on top of cbor
  client.ml                  # Test runner + run lifecycle on top of Hegel_ffi.Ffi
  generators.ml              # Re-export shim: include the four generators_* modules
  generators_core.ml         # generator type; draw/draw_silent, map/flat_map/filter,
                             #   composite, span labels — the discriminated union
  generators_primitives.ml   # integers, booleans, floats, text, binary, just, formats
  generators_collections.ml  # lists, hashmaps, sets, and the collection protocol
  generators_combinators.ml  # sampled_from, one_of, tuples2/3/4
  derive.ml                  # Runtime support for [@@deriving hegel_generator]
  stateful.ml                # Stateful testing: Rule.create + run over action sequences
  antithesis.ml              # Antithesis integration (emits an always-typed assertion)
  test_runtime/              # Inline-test registry + runner for [let%hegel_test]
    hegel_test_runtime.ml    # Registry, run_all, test_main (called by dune-generated runner)

ppx/                         # PPX rewriters and derivers
  dune                       # PPX library build configs; ppx_hegel_test declares
                             # (inline_tests.backend ...) + (ppx_runtime_libraries hegel_test_runtime);
                             # a rule generates ppx_compat.ml from one variant below
  ppx_hegel_generator.ml     # Deriver: reads type decls, emits generator functions
  ppx_hegel_test.ml          # Expander: rewrites [let%hegel_test name tc = body]
                             # into a callable function plus a Hegel_test_runtime.register call
  ppx_compat_pre-53.ml       # AST compat shim for ppxlib < 0.36 (OCaml < 5.3)
  ppx_compat_post-53.ml      # AST compat shim for ppxlib >= 0.36 (OCaml >= 5.3)
  ppx_compat_oxcaml.ml       # AST compat shim for the OxCaml compiler
  test/                      # PPX E2E tests, package-attributed so opam-repo-ci runs them
    test_ppx_derive.ml       # PPX deriver E2E tests (package ppx_hegel_generator)
    test_ppx_hegel_test.ml   # ppx_hegel_test expander E2E tests (package ppx_hegel_test)
    test_hegel_test_runtime.ml # Inline-test runner behavior (re-spawns itself;
                             #   package ppx_hegel_test)
    expect_tests/            # ppx_expect tests (dev-only, disabled in release profile)
                             # (one of the three is copied to ppx_compat.ml = ppx_hegel_compat lib)

test/                        # hegel's own test suite (one executable: test_hegel,
  dune                       #   Alcotest, package hegel — runs under `-p hegel`;
                             #   no PPX preprocessing beyond the ppx_js_style linter)
  test_hegel.ml              # Top-level Alcotest runner
  test_helpers.ml            # Shared test utilities
  test_cbor_helpers.ml       # CBOR helper tests
  test_cbor_vectors.ml       # CBOR round-trip vector tests
  test_client.ml             # Internal config + run lifecycle tests (real engine)
  test_generators_*.ml       # Generator core / primitives / collections / combinators / schema
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
`next_test_case`, `run_result`, `run_free`), and per-test-case primitives
(`generate`, spans, collections, pools, `target`, `mark_complete`). CBOR is used
only for schema bytes (in) and generated-value bytes (out). `hegel_next_test_case`
blocks on the engine's worker thread, so its binding releases the OCaml runtime
lock. The `ffi` library is deliberately NOT bisect_ppx-instrumented, keeping its
mechanical marshalling out of the 100%-coverage gate (no `[@coverage off]`).

`lib/protocol.ml`, `lib/connection.ml`, and the old Python-subprocess install
flow were removed in the native-backend migration.

### Generator System (generators_core.ml + generators_{primitives,collections,combinators}.ml)

The generator type and combinators (`draw`, `map`, `flat_map`, `composite`, …)
live in `generators_core.ml`; the primitives, collections, and combinators are
split across the sibling `generators_*.ml` files. `generators.ml` is a thin shim
that `include`s all four so they surface as one `Hegel.Generators` module.

Generators are a discriminated union:
- **Basic** — holds a raw CBOR schema + optional transform. Calling `map` on a Basic preserves the schema (composes transforms). The engine generates the value in one round-trip.
- **Mapped** — wraps source + transform function (non-schema-preserving map).
- **FlatMapped** — wraps source + a function returning a generator. Evaluated recursively inside a `flat_map` span.
- **Filtered** — wraps source + predicate. Up to `max_filter_attempts` retries before `assume false`.
- **CompositeList** — used when list elements are non-Basic. Uses the collection protocol (new_collection / collection_more) to generate elements one at a time.

### Inline Test Integration (ppx/ppx_hegel_test.ml + lib/test_runtime/)

The `ppx_hegel_test` PPX rewrites `let%hegel_test name tc = body` into two
top-level items: (1) `let name = fun () -> Hegel.run_hegel_test ...
(fun tc -> body)`, and (2) `let () = Hegel_test_runtime.register ~name ~file
~line name`. The function remains directly callable; the registration is a
side effect run at module init.

`ppx_hegel_test`'s dune stanza declares `(inline_tests.backend ...)` with a
generated runner that calls `Hegel_test_runtime.test_main ()`, and
`(ppx_runtime_libraries hegel_test_runtime)` so the runtime is auto-linked
into any library that uses the PPX. Users opt in per-library with
`(inline_tests (backend ppx_hegel_test))`, after which `dune runtest`
discovers and runs every `let%hegel_test`. The runtime's `test_main ()`
iterates the registry, wraps each test in try/with, prints PASS/FAIL per
test, and exits non-zero on any failure.

Libraries that don't opt in still get the registration side effect (the
PPX always emits it) — entries just sit unused. That's harmless and lets
tests built around alternative harnesses (Alcotest, raw `let () = name ()`)
keep working unchanged. The runner's behavior is verified in
`ppx/test/test_hegel_test_runtime.ml`, which re-spawns itself with a magic
`--__hegel_test_runtime_demo MODE` argv that the top of the same file
intercepts to register a single test and call
`Hegel_test_runtime.test_main`; the subprocess's exit code is then
asserted. Spawning itself (rather than a separate demo executable)
sidesteps the build-ordering trap that would otherwise hit any recipe
invoking the test binary directly.

### Type-Directed Derivation (ppx/ + lib/derive.ml)

The `ppx_hegel_generator` PPX deriver synthesizes a `(<t>, unprintable) generator`
value named `<t>_generator` from type declarations annotated with
`[@@deriving hegel_generator]`. The generated code:

1. For **records**: generates each field by calling the appropriate primitive
   generator, then constructs the record value.
2. For **variants**: picks a constructor index uniformly at random via
   `sampled_from`, then generates arguments for the chosen constructor.
3. For **type aliases**: delegates to the generator for the aliased type.
4. For **nested types**: draws `<type>_generator` via `draw_silent` (user-defined
   generators must exist in scope).

The PPX emits a `test_case -> t` field-drawing thunk and wraps it with
`Hegel.Generators.composite`, producing an `(t, unprintable) generator` value —
no printer is attached, so a bare `[@@deriving hegel_generator]` always compiles. The
`Hegel.Derive` module provides runtime helpers for option and list types; it is
re-exported doc-hidden (the standard public-but-invisible PPX-runtime pattern)
because generated code in user projects calls it.

**Usage example:**

```ocaml
(* In your dune file, add:
     (inline_tests (backend ppx_hegel_test))
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
```

To print a derived value on a failing replay, add `[@@deriving sexp_of]` and draw
through `with_printer`:
`Hegel.draw tc (Hegel.with_printer sexp_of_point point_generator)`.

**Supported field types:**
- `int` — bounded integers (±1073741823 to fit OCaml native int)
- `bool` — booleans
- `float` — finite floats (NaN and infinity disabled)
- `string` — text strings
- `t list` — lists of derived elements (max size 20)
- `t option` — `Some v` or `None`
- Named types `t` — draws `t_generator` via `draw_silent` (must be in scope)
- Tuples `(t1 * t2 * ...)` — generates each component

### Collection Protocol

Non-basic list elements use a engine-side collection handle:
1. `new_collection` → engine assigns a collection name
2. `collection_more` → engine returns true/false (should we generate another element?)
3. `collection_reject` → undo the last element (used by filter)

### Entry point

The engine runs in-process, so there is no subprocess or session to manage.
The public entry point is `Hegel.run_hegel_test ?settings ?test_location
test_fn` — `Internal.run_hegel_test`, which is `Internal.run_test` with [settings]
defaulting to `default_settings ()`. It is what the `let%hegel_test` PPX
targets. The `[@@failure_blobs ...]` record/replay workflow is supported: the
PPX forwards the listed blobs as `~failure_blobs` to `run_hegel_test`, which
replays the first blob as a standalone deterministic case (pair it with
`with_print_blob true` to print the reproducing blob on failure). For persisting
and replaying failing examples across runs, use `database` / `database_key`.

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
  re-exports `Hegel.{Internal,Cbor_helpers,Antithesis}`; `Hegel.Derive` is
  doc-hidden too because PPX-generated code calls it. Only `Generators`,
  `Stateful`, and the values/types directly under `Hegel` are documented API
- PPX E2E tests live under `ppx/test/`, attributed via `(package ...)` to
  `ppx_hegel_generator` (`test_ppx_derive.ml`) and `ppx_hegel_test`
  (`test_ppx_hegel_test.ml`, `test_hegel_test_runtime.ml`) so
  `dune runtest -p <pkg>` runs them; `ppx/test/expect_tests/` stays
  package-less and dev-only (`enabled_if (<> %{profile} release)`)
- 100% branch and line coverage is mandatory — no exceptions, no `[@coverage off]`

### Error Handling

- `Internal.Assume_rejected` — raised by `assume false`; mapped to `mark_complete INVALID`
- `Internal.Data_exhausted` — raised when StopTest is received; skips `mark_complete`
- `Connection.Request_error` — raised on protocol-level errors from the engine

### Schema Format

The Hegel engine speaks CBOR. Generator schemas are CBOR maps:
- `{"type": "boolean"}` — booleans
- `{"type": "integer", "min_value"?: N, "max_value"?: N}` — integers
- `{"type": "float", "allow_nan": bool, "allow_infinity": bool, "width": 64, "exclude_min": bool, "exclude_max": bool, "min_value"?: f, "max_value"?: f}` — floats
- `{"type": "string", "min_size": N, "max_size"?: N}` — text
- `{"type": "binary", "min_size": N, "max_size"?: N}` — binary
- sampled_from: uses `{"type": "integer", "min_value": 0, "max_value": N-1}` with a map transform to index into the values array
- `{"type": "list", "elements": schema, "min_size": N, "max_size"?: N}` — lists
- `{"type": "dict", "keys": schema, "values": schema, "min_size": N, "max_size"?: N}` — dicts (engine returns `[[k,v],...]`)
- `{"constant": null}` — just (constant value; transform ignores engine result)
- `{"type": "regex", "pattern": str, "fullmatch": bool}` — from_regex
- `{"type": "email"}`, `{"type": "url"}`, `{"type": "domain", "max_length"?: N}` — format generators
- `{"type": "date"}`, `{"type": "time"}`, `{"type": "datetime"}` — date/time generators
- `{"type": "ip_addresses", "version": 4|6}` — IP address generators
- `{"one_of": [tagged_schema, ...]}` — one_of (tagged-tuple schemas for dispatch)
- `{"type": "tuple", "elements": [schema, ...]}` — tuples

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

7. **Zero odoc warnings is enforced by `just check-docs`, not by dune**: `dune build @doc`
   exits 0 even when odoc emits warnings (bad references, undocumented values), and dune's
   cache hides the warnings entirely on rebuilds — a warm `dune build @doc` prints nothing
   even if the doc comments are broken. The `check-docs` recipe therefore removes
   `_build/default/_doc` first and fails on any output. All lib modules must have
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

11. **Four example programs covers the full surface area**: `basic_properties.ml` (primitives,
    assume, note), `collections.ml` (lists, map, flat_map, filter, sampled_from, hashmaps),
    `real_world.ml` (sorted-merge property test), `derived_types.ml` (PPX deriver). Each has a
    standalone `main`; derived_types needs a separate dune stanza with PPX preprocessing.

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
