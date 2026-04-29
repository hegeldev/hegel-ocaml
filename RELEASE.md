RELEASE_TYPE: patch

Bump our pinned hegel-core version from `0.4.1` to [`0.4.14`](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.14), incorporating the following hegel-core changes:

- [v0.4.2](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.2): Add `crash_after_handshake` and `crash_after_handshake_with_stderr` test modes for client crash-detection testing.
- [v0.4.3](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.3): Add `OneOfConformance` test for the `one_of` generator, plus recommended integer bound constants (`INT32_MIN`/`MAX`, `INT64_MIN`/`MAX`, `BIGINT_MIN`/`MAX`) for use in conformance test setup.
- [v0.4.4](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.4): Initial Windows-support fixes (mostly affecting conformance testing).
- [v0.4.5](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.5): Add a new `OriginDeduplicationConformance` test.
- [v0.4.6](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.6): Fix several concurrency bugs and improve error handling robustness in the protocol layer. The server's reader loop no longer crashes on packets for unknown/closed streams or malformed close-stream packets — it sends an error reply back instead. Race conditions in `Connection.close()`, `Stream.close()`, `Stream.write_request`, `Connection.new_stream`, and `receive_handshake` have been fixed. Bare `assert` statements were replaced with explicit error raises. `StdioTransport.sendall` now converts `ValueError` to `OSError` so the existing handling catches it.
- [v0.4.7](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.7): Add a `single_test_case` top-level protocol command that hands a single test case to the client in final mode and returns the result, with no shrinking, replay, or other exploration.
- [v0.4.8](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.8): Remove the unused Unix-socket transport from the `hegel` server. The server now always communicates over stdin/stdout, and the previously-required `--stdio` flag has been removed; we drop it from our subprocess invocation.
- [v0.4.9](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.9): Add a `command_prefix` argument to `run_conformance_tests` to control how conformance tests are run.
- [v0.4.10](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.10): Add fraction and complex number schema types.
- [v0.4.11](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.11): Add a `skip_unique` parameter to `ListConformance`.
- [v0.4.12](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.12): Remove CBOR tagging from fraction and complex numbers.
- [v0.4.13](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.13): Tweak how conformance tests write metrics — the server now writes one server-side metrics line per attempted test case (including OVERRUN/INVALID), and the harness asserts that count matches the client metrics.
- [v0.4.14](https://github.com/hegeldev/hegel-core/releases/tag/v0.4.14): Pin our dependencies to below their next major version.

The protocol version remains `0.10`, so no client-side protocol changes are required.

Together with the version bump, this PR:

- Drops the now-unrecognised `--stdio` flag from the `hegel` subprocess invocation. (Hegel-core 0.4.8 removed the flag because stdin/stdout is the only transport.)
- Adds the `hegel-core-release` receiver workflow (`.github/workflows/bump-hegel-core.yml` and `.github/scripts/bump_hegel_core.py`) so future hegel-core releases will automatically open a bump PR here, mirroring the setup already in place for hegel-rust. The bump script also keeps the `nix/flake.nix` reference in sync with `lib/session.ml`.

The conformance-harness pin in `justfile` is intentionally left at `hegel-core==0.4.1` (older than the runtime pin). Hegel-core 0.4.13 introduced strict per-test-case alignment between client-side `write_metrics` calls and server-side metrics entries, including for OVERRUN test cases that the OCaml client never surfaces to user code. Bringing the harness forward will require some adaptation work in the conformance binaries; that's a follow-up.
