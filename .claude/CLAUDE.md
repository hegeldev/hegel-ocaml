# Hegel for OCaml

## Build Commands

```bash
just setup       # Install dependencies and hegel binary into .venv/
just test        # Run tests with 100% coverage enforcement
just format      # Auto-format code with ocamlformat
just lint        # Check formatting (fails if unformatted)
just docs        # Build API documentation with odoc
just conformance # Run conformance tests against the Python framework
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
- **PPX derivation**: ppxlib 0.35.0 (for `[@@deriving generator]`)

## Project Structure

```
lib/                         # Library source
  dune                       # Library build config (bisect_ppx instrumented)
  hegel.ml                   # Main module — re-exports all sub-modules
  protocol.ml                # Binary wire protocol: packet format, CRC32, constants
  cbor_helpers.ml            # CBOR encoding/decoding with type-safe extractors
  connection.ml              # Multiplexed connection and stream abstractions
  client.ml                  # Test runner and lifecycle management
  session.ml                 # Global server subprocess management
  generators.ml              # Generator combinators (booleans, integers, lists, …)
  conformance.ml             # Conformance test helpers (get_test_cases, write_metrics)
  derive.ml                  # Runtime support for [@@deriving generator]

ppx/                         # PPX deriver package (ppx_hegel_generator)
  dune                       # PPX library build config (depends on ppxlib)
  ppx_hegel_generator.ml     # Deriver: reads type decls, emits generator functions

test/                        # Alcotest test suite
  dune                       # Test build config (two executables: test_hegel, test_ppx_derive)
  test_hegel.ml              # Top-level Alcotest runner
  test_protocol.ml           # Wire protocol tests
  test_cbor_helpers.ml       # CBOR helper tests
  test_connection.ml         # Connection and stream tests
  test_client.ml             # Client lifecycle tests
  test_generators.ml         # Generator combinator tests
  test_showcase.ml           # End-to-end example property tests
  test_conformance_helpers.ml # Conformance helper and new-generator tests
  test_derive.ml             # Derive module runtime helper tests
  test_ppx_derive.ml         # PPX deriver E2E tests (uses ppx_hegel_generator)

conformance/                 # Conformance test binaries (compiled executables)
  dune                       # Builds json_params library + 8 executables
  json_params.ml             # Lightweight JSON parser for argv[1] params
  test_booleans.ml           # Boolean conformance binary
  test_integers.ml           # Integer conformance binary
  test_floats.ml             # Float conformance binary
  test_text.ml               # Text (string) conformance binary
  test_binary.ml             # Binary (bytes) conformance binary
  test_lists.ml              # List conformance binary (uses CompositeList path)
  test_sampled_from.ml       # SampledFrom conformance binary
  test_hashmaps.ml           # Hashmap (dict) conformance binary

test/conformance/            # Python conformance test harness
  test_conformance.py        # pytest file — imports from hegel.conformance

docs/                        # Tutorial and guide documents
  getting-started.md         # Getting Started tutorial (OCaml translation)

examples/                    # Example programs demonstrating the library
  dune                       # Example executables build config
  basic_properties.ml        # Primitive generators: integers, booleans, floats
  collections.ml             # Collections and combinators: lists, filter, map
  real_world.ml              # Real-world scenario: sorted-merge property test
  derived_types.ml           # Derived generators via [@@deriving generator]

scripts/
  check-coverage.py          # Parses bisect-ppx-report, enforces 100%

README.md                    # Project overview, install, quick-start
```

## Architecture Overview

### Transport Layer (protocol.ml)

Packets are fixed-format binary frames:
- 4-byte magic `HEGL` (0x4845474C)
- 4-byte CRC32 of the payload
- 4-byte stream ID (odd = client-allocated, even = server-allocated)
- 4-byte message ID (high bit set = reply)
- 4-byte payload length
- N-byte CBOR payload
- 1-byte terminator (0x0A)

### Multiplexing (connection.ml)

A single Unix socket carries many logical streams. The reader thread dispatches incoming packets to per-stream inboxes (guarded by a mutex + condition variable). Streams are demand-driven: the reader only runs when a stream is waiting for a packet. Client streams get odd IDs; server streams get even IDs.

### Generator System (generators.ml)

Generators are a discriminated union:
- **Basic** — holds a raw CBOR schema + optional transform. Calling `map` on a Basic preserves the schema (composes transforms). The server generates the value in one round-trip.
- **Mapped** — wraps source + transform function (non-schema-preserving map).
- **FlatMapped** — wraps source + a function returning a generator. Evaluated recursively inside a `flat_map` span.
- **Filtered** — wraps source + predicate. Up to `max_filter_attempts` retries before `assume false`.
- **CompositeList** — used when list elements are non-Basic. Uses the collection protocol (new_collection / collection_more) to generate elements one at a time.

### Type-Directed Derivation (ppx/ + lib/derive.ml)

The `ppx_hegel_generator` PPX deriver synthesizes `unit -> 'a` generator functions
from type declarations annotated with `[@@deriving generator]`. The generated code:

1. For **records**: generates each field by calling the appropriate primitive
   generator, then constructs the record value.
2. For **variants**: picks a constructor index uniformly at random via
   `sampled_from`, then generates arguments for the chosen constructor.
3. For **type aliases**: delegates to the generator for the aliased type.
4. For **nested types**: calls `<type>_generator ()` (user-defined generators
   must exist in scope).

The PPX emits code that calls `Hegel.Generators.generate` directly — no
intermediate `generator` value is produced. The `Hegel.Derive` module provides
runtime helpers for option and list types.

**Usage example:**

```ocaml
(* In your dune file, add: (preprocess (pps ppx_hegel_generator)) *)

type point = { x : int; y : int } [@@deriving generator]
type color = Red | Green | Blue [@@deriving generator]
type entity = { name : string; tag : int option; active : bool }
[@@deriving generator]

(* Use inside a Hegel test body: *)
let () =
  Hegel.Session.run_hegel_test ~name:"my_test" ~test_cases:100 (fun () ->
    let p = point_generator () in
    let c = color_generator () in
    let e = entity_generator () in
    (* p.x, p.y are ints; c is Red|Green|Blue; e has typed fields *)
    ignore (p, c, e))
```

**Supported field types:**
- `int` — bounded integers (±1073741823 to fit OCaml native int)
- `bool` — booleans
- `float` — finite floats (NaN and infinity disabled)
- `string` — text strings
- `t list` — lists of derived elements (max size 20)
- `t option` — `Some v` or `None`
- Named types `t` — calls `t_generator ()` (must be in scope)
- Tuples `(t1 * t2 * ...)` — generates each component

### Collection Protocol

Non-basic list elements use a server-side collection handle:
1. `new_collection` → server assigns a collection name
2. `collection_more` → server returns true/false (should we generate another element?)
3. `collection_reject` → undo the last element (used by filter)

### Session Management (session.ml)

A singleton `_session` lazily starts the `hegel` binary as a subprocess on first use. It creates a temporary directory, waits for the Unix socket to appear, connects, and registers an `at_exit` handler to clean up. `restart_session` is provided for test modes that need a fresh server (e.g., `HEGEL_PROTOCOL_TEST_MODE`).

### Test Runner (client.ml)

`run_test` sends a `run_test` command on the control stream, then processes `test_case` events — each carrying a new data stream ID. For each test case, `run_test_case` runs the user's test function with the data stream as the active stream (stored in thread-local refs). Exceptions are mapped to `mark_complete` statuses: VALID, INVALID (assume), INTERESTING (any other exception). If StopTest is received on any command, `_test_aborted` is set and `mark_complete` is skipped.

## Key Patterns and Conventions

### Documentation

- All public types, functions, exceptions, and constants use `(** ... *)` doc comments for odoc
- `just docs` must build with zero warnings — this is enforced in CI
- Parameter descriptions live inline in the first sentence when names are self-explanatory

### Testing

- Every lib module has a corresponding `test/test_<module>.ml`
- Unit tests use socketpair-based fake servers to avoid depending on the real hegel binary
- End-to-end tests (tagged `_e2e`) require the real binary and live under the same test file
- PPX-derived tests live in `test/test_ppx_derive.ml` (separate executable with PPX preprocessing)
- 100% branch and line coverage is mandatory — no exceptions, no `[@coverage off]`

### Error Handling

- `Client.Assume_rejected` — raised by `assume false`; mapped to `mark_complete INVALID`
- `Client.Data_exhausted` — raised when StopTest is received; skips `mark_complete`
- `Connection.Request_error` — raised on protocol-level errors from the server

### Schema Format

The Hegel server speaks CBOR. Generator schemas are CBOR maps:
- `{"type": "boolean"}` — booleans
- `{"type": "integer", "min_value"?: N, "max_value"?: N}` — integers
- `{"type": "float", "allow_nan": bool, "allow_infinity": bool, "width": 64, "exclude_min": bool, "exclude_max": bool, "min_value"?: f, "max_value"?: f}` — floats
- `{"type": "string", "min_size": N, "max_size"?: N}` — text
- `{"type": "binary", "min_size": N, "max_size"?: N}` — binary
- sampled_from: uses `{"type": "integer", "min_value": 0, "max_value": N-1}` with a map transform to index into the values array
- `{"type": "list", "elements": schema, "min_size": N, "max_size"?: N}` — lists
- `{"type": "dict", "keys": schema, "values": schema, "min_size": N, "max_size"?: N}` — dicts (server returns `[[k,v],...]`)
- `{"constant": null}` — just (constant value; transform ignores server result)
- `{"type": "regex", "pattern": str, "fullmatch": bool}` — from_regex
- `{"type": "email"}`, `{"type": "url"}`, `{"type": "domain", "max_length"?: N}` — format generators
- `{"type": "date"}`, `{"type": "time"}`, `{"type": "datetime"}` — date/time generators
- `{"type": "ip_addresses", "version": 4|6}` — IP address generators
- `{"one_of": [tagged_schema, ...]}` — one_of (tagged-tuple schemas for dispatch)
- `{"type": "tuple", "elements": [schema, ...]}` — tuples

### Coverage Rules

- 100% line coverage is mandatory on library code
- `scripts/check-coverage.py` parses `bisect-ppx-report summary` output
- Unreachable server-contract violations use `failwith "..."` (tested via unit tests on the transform)
- `[@coverage off]` annotations are never used
- Only `lib/` code is instrumented; conformance binaries, examples, and PPX code are not measured

## Lessons Learned

### PPX Deriver Implementation

1. **PPX generates `unit -> 'a` functions, not `generator` values**: The Hegel generator
   system operates at the CBOR level — `generate gen` returns `CBOR.Simple.t`. For
   type-directed derivation to be ergonomic, the PPX generates functions that call
   `generate` internally and extract typed OCaml values. This means derived generators
   are `unit -> my_type` thunks, not `Hegel.Generators.generator` values.

2. **Unbounded integers cause CBOR bigint issues**: `integers()` without bounds can
   generate numbers too large for OCaml's native int. The CBOR library encodes these
   as tagged bigints that `extract_int` can't decode. The PPX bounds ints to
   ±1073741823 (30-bit) to avoid this. Users needing different ranges should use
   the manual `Generators.integers ~min_value ~max_value ()` API.

3. **PPX tests need a separate executable**: Because the PPX needs
   `(preprocess (pps ppx_hegel_generator))`, the test file using `[@@deriving generator]`
   must be in a separate `(test ...)` stanza from the main test suite. Both test
   executables are run by `dune runtest`.

4. **ppxlib.metaquot is essential**: The PPX uses `[%expr ...]` and `[%stri ...]`
   metaquot syntax for readable AST construction. This requires
   `(preprocess (pps ppxlib.metaquot))` in the PPX's own dune file.

5. **Runtime helpers in lib/derive.ml**: For option and list types, the PPX delegates
   to runtime helpers `Hegel.Derive.generate_option` and `Hegel.Derive.generate_list`.
   These live in the main library (not the PPX) so they're covered by bisect_ppx.

6. **Floats default to finite**: The PPX generates `floats ~allow_nan:false ~allow_infinity:false ()`
   to avoid NaN/infinity in derived types, which would cause issues in most user code.

### Documentation and Polish Stage

7. **`just docs` already enforces zero warnings**: The `dune build @doc` target is strict — any
   undocumented public value produces a warning that fails the build. All lib modules must have
   `(** ... *)` doc comments on every public type, function, constant, and exception. This is
   enforced in CI via `just check: lint docs test`.

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

18. **`Checkseum.Crc32.digest_string` supports incremental computation**: The `init` parameter of
    `digest_string` can be the result of a previous digestion, enabling CRC32 over multiple
    string segments without concatenation. Use `compute_crc32_parts` for two-segment CRC.

19. **`Bytes.blit_string` for zero-copy assembly**: When building a buffer from header + payload +
    terminator, use `Bytes.create` + `Bytes.blit` / `Bytes.blit_string` instead of string
    concatenation followed by `Bytes.of_string`. This avoids intermediate string allocations.

20. **Shared test helpers belong in `test/test_helpers.ml`**: Any utility function used across
    multiple test modules (e.g. `contains_substring`) should live in a shared helper module listed
    in the dune `(modules ...)` stanza. This avoids copy-paste and ensures consistent behavior.

21. **Or-patterns in match arms for deduplication**: When two match arms do the same thing with
    minor variation, use `(Some (Dead _) | None) as entry -> ...` and dispatch on the bound
    variable inside the arm body. This is cleaner than duplicating the entire block.

22. **Test helper deduplication must include protocol helpers**: The `with_fake_server`,
    `accept_run_test`, `send_test_case`, `send_test_done`, and `recv_command` helpers are
    used across `test_client.ml`, `test_generators.ml`, and `test_derive.ml`. They all live
    in `test_helpers.ml` and are re-exported via `let foo = Test_helpers.foo` in each test
    file. This is idiomatic OCaml — module-level `let` aliases are cheap and keep the call
    sites clean while eliminating copy-paste.

23. **OCaml codebase is stable and well-structured after greybeard pass**: After three rounds
    of expert review, the lib/ code has no reinvented wheels, no non-idiomatic patterns,
    no needless complexity, no bad names, and no dead code. The `List.filter_map Fun.id`
    pattern for optional schema fields, `Option.fold` for accumulator updates, and
    `Fun.protect ~finally` for resource cleanup are all standard idiomatic OCaml.

24. **`dune-project` license must match the actual LICENSE file**: The `(license ...)` field
    in `dune-project` is propagated to the generated `.opam` file. If these disagree with
    the actual `LICENSE` file, downstream tooling (opam, GitHub license detection) will show
    conflicting information. Always check that the declared license matches the file.

25. **Shared conformance helpers belong in `json_params.ml`**: Utility functions used by
    multiple conformance binaries (like `int_opt_to_json`) should live in the shared
    `json_params` module rather than being copy-pasted into each binary. The conformance
    binaries already depend on `json_params` via the dune `(libraries ...)` stanza.
