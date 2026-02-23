# Hegel SDK for OCaml

## Build Commands

```bash
just setup   # Install dependencies and hegel binary into .venv/
just test    # Run tests with 100% coverage enforcement
just format  # Auto-format code with ocamlformat
just lint    # Check formatting (fails if unformatted)
just docs    # Build API documentation with odoc
just check   # Run lint + docs + test (the full CI check)
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
lib/           # Library source code (the SDK)
  dune         # Library build config
  hegel.ml     # Main module
test/          # Test files
  dune         # Test build config
  test_hegel.ml
scripts/       # Build scripts
  check-coverage.py  # Parses bisect_ppx output, enforces 100%
```

## Project Conventions

- All library code lives in `lib/`; all tests in `test/`
- The opam package is named `hegel`; the OCaml library is `hegel`
- Use `(** ... *)` doc comments on all public functions/types for odoc
- Tests use Alcotest; each test module corresponds to a library module
- Coverage is measured only on `lib/` code, not test code
- Format all code before committing: `just format`
- The `hegel` binary is found via PATH=".venv/bin:$PATH" in test recipes

## Coverage Rules

- 100% line coverage is mandatory on library code
- `scripts/check-coverage.py` parses `bisect-ppx-report summary` output
- Unreachable code should use `assert false` or `failwith "unreachable"`
- Coverage annotations like `[@coverage off]` are never used
- Test files are excluded from coverage measurement via bisect_ppx conditional instrumentation (only `lib/` is instrumented)
