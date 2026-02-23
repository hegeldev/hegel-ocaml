# Hegel SDK for OCaml

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

## Project Structure

```
lib/                         # Library source (the SDK itself)
  dune                       # Library build config (bisect_ppx instrumented)
  hegel.ml                   # Main module — re-exports all sub-modules
  protocol.ml                # Binary wire protocol: packet format, CRC32, constants
  cbor_helpers.ml            # CBOR encoding/decoding with type-safe extractors
  connection.ml              # Multiplexed connection and channel abstractions
  client.ml                  # Test runner and lifecycle management
  session.ml                 # Global hegeld subprocess management
  generators.ml              # Generator combinators (booleans, integers, lists, …)
  conformance.ml             # Conformance test helpers (get_test_cases, write_metrics)

test/                        # Alcotest test suite
  dune                       # Test build config
  test_hegel.ml              # Top-level Alcotest runner
  test_protocol.ml           # Wire protocol tests
  test_cbor_helpers.ml       # CBOR helper tests
  test_connection.ml         # Connection and channel tests
  test_client.ml             # Client lifecycle tests
  test_generators.ml         # Generator combinator tests
  test_showcase.ml           # End-to-end example property tests
  test_conformance_helpers.ml # Conformance helper and new-generator tests

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

tests/conformance/           # Python conformance test harness
  test_conformance.py        # pytest file — imports from hegel.conformance

docs/                        # Tutorial and guide documents
  getting-started.md         # Getting Started tutorial (OCaml translation)

examples/                    # Example programs demonstrating the SDK
  dune                       # Example executables build config
  basic_properties.ml        # Primitive generators: integers, booleans, floats
  collections.ml             # Collections and combinators: lists, filter, map
  real_world.ml              # Real-world scenario: sorted-merge property test

scripts/
  check-coverage.py          # Parses bisect-ppx-report, enforces 100%

README.md                    # Project overview, install, quick-start
```

## Architecture Overview

### Transport Layer (protocol.ml)

Packets are fixed-format binary frames:
- 4-byte magic `HEGL` (0x4845474C)
- 4-byte CRC32 of the payload
- 4-byte channel ID (odd = client-allocated, even = server-allocated)
- 4-byte message ID (high bit set = reply)
- 4-byte payload length
- N-byte CBOR payload
- 1-byte terminator (0x0A)

### Multiplexing (connection.ml)

A single Unix socket carries many logical channels. The reader thread dispatches incoming packets to per-channel inboxes (guarded by a mutex + condition variable). Channels are demand-driven: the reader only runs when a channel is waiting for a packet. Client channels get odd IDs; server channels get even IDs.

### Generator System (generators.ml)

Generators are a discriminated union:
- **Basic** — holds a raw CBOR schema + optional transform. Calling `map` on a Basic preserves the schema (composes transforms). The server generates the value in one round-trip.
- **Mapped** — wraps source + transform function (non-schema-preserving map).
- **FlatMapped** — wraps source + a function returning a generator. Evaluated recursively inside a `flat_map` span.
- **Filtered** — wraps source + predicate. Up to `max_filter_attempts` retries before `assume false`.
- **CompositeList** — used when list elements are non-Basic. Uses the collection protocol (new_collection / collection_more) to generate elements one at a time.

### Collection Protocol

Non-basic list elements use a server-side collection handle:
1. `new_collection` → server assigns a collection name
2. `collection_more` → server returns true/false (should we generate another element?)
3. `collection_reject` → undo the last element (used by filter)

### Session Management (session.ml)

A singleton `_session` lazily starts the `hegel` binary as a subprocess on first use. It creates a temporary directory, waits for the Unix socket to appear, connects, and registers an `at_exit` handler to clean up. `restart_session` is provided for test modes that need a fresh server (e.g., `HEGEL_PROTOCOL_TEST_MODE`).

### Test Runner (client.ml)

`run_test` sends a `run_test` command on the control channel, then processes `test_case` events — each carrying a new data channel ID. For each test case, `run_test_case` runs the user's test function with the data channel as the active channel (stored in thread-local refs). Exceptions are mapped to `mark_complete` statuses: VALID, INVALID (assume), INTERESTING (any other exception). If StopTest is received on any command, `_test_aborted` is set and `mark_complete` is skipped.

## Key Patterns and Conventions

### Documentation

- All public types, functions, exceptions, and constants use `(** ... *)` doc comments for odoc
- `just docs` must build with zero warnings — this is enforced in CI
- Parameter descriptions live inline in the first sentence when names are self-explanatory

### Testing

- Every lib module has a corresponding `test/test_<module>.ml`
- Unit tests use socketpair-based fake servers to avoid depending on the real hegel binary
- End-to-end tests (tagged `_e2e`) require the real binary and live under the same test file
- 100% branch and line coverage is mandatory — no exceptions, no `[@coverage off]`

### Error Handling

- `Client.Assume_rejected` — raised by `assume false`; mapped to `mark_complete INVALID`
- `Client.Data_exhausted` — raised when StopTest is received; skips `mark_complete`
- `Connection.Request_error` — raised on protocol-level errors from the server

### Schema Format

The Hegel server speaks CBOR. Generator schemas are CBOR maps:
- `{"type": "boolean"}` — booleans
- `{"type": "integer", "min_value"?: N, "max_value"?: N}` — integers
- `{"type": "number", "allow_nan": bool, "allow_infinity": bool, "width": 64, "exclude_min": bool, "exclude_max": bool, "min_value"?: f, "max_value"?: f}` — floats
- `{"type": "string", "min_size": N, "max_size"?: N}` — text
- `{"type": "binary", "min_size": N, "max_size"?: N}` — binary
- `{"sampled_from": [...]}` — sampled from list (no "type" key)
- `{"type": "list", "elements": schema, "min_size": N, "max_size"?: N}` — lists
- `{"type": "dict", "keys": schema, "values": schema, "min_size": N, "max_size"?: N}` — dicts (server returns `[[k,v],...]`)

### Coverage Rules

- 100% line coverage is mandatory on library code
- `scripts/check-coverage.py` parses `bisect-ppx-report summary` output
- Unreachable server-contract violations use `failwith "..."` (tested via unit tests on the transform)
- `[@coverage off]` annotations are never used
- Only `lib/` code is instrumented; conformance binaries and examples are not measured
