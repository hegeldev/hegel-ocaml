set ignore-comments := true

check-tests:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    find _build -name '*.coverage' -delete 2>/dev/null || true
    # Run test binaries directly (not via dune runtest) so output streams
    # in real-time — dune runtest buffers output until completion, hiding
    # diagnostic messages when a test hangs. The native libhegel engine is
    # located automatically at runtime (HEGEL_LIBHEGEL_PATH > sibling
    # ../hegel-rust/target build > verified download); see lib/ffi/loader.ml.
    dune build --instrument-with bisect_ppx \
      test/test_hegel.exe \
      ppx/test/test_ppx_derive.exe \
      ppx/test/test_ppx_hegel_test.exe
    export BISECT_FILE="$PWD/_build/default/test/bisect"
    ./_build/default/ppx/test/test_ppx_derive.exe
    ./_build/default/ppx/test/test_ppx_hegel_test.exe
    ./_build/default/test/test_hegel.exe
    # The ppx_expect tests are an inline-tests library (no standalone exe), so
    # run them through dune; coverage merges via BISECT_FILE.
    dune runtest ppx/test/expect_tests --instrument-with bisect_ppx --force
    dune build --instrument-with bisect_ppx lib/jane/test/test_hegel_jane.exe
    ./_build/default/lib/jane/test/test_hegel_jane.exe
    dune runtest lib/jane/test --instrument-with bisect_ppx --force
    dune build --instrument-with bisect_ppx ppx/test/test_ppx_derive_jane.exe
    ./_build/default/ppx/test/test_ppx_derive_jane.exe
    python3 scripts/check-coverage.py
    
format:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune fmt || true
    # also run format-nix if we have nix installed
    @which nix && just format-nix || true

format-nix:
    nix run nixpkgs#nixfmt -- nix/flake.nix

check-format-nix:
    nix run nixpkgs#nixfmt -- --check nix/flake.nix

check-format:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build @fmt

check-docs:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    # Scope to the `hegel` package so the internal ppx_hegel_* packages are
    # excluded from the generated docs. odoc warnings are fatal in the dev
    # profile (see the root `dune` file), so a cached success is genuinely
    # warning-free — no cold rebuild needed. (Deleting _build/default/_doc to
    # force one corrupted dune's incremental odoc state after source edits.)
    dune build @doc --only-packages hegel

docs: check-docs
    open _build/default/_doc/_html/index.html

check-tests-no-coverage:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build test/test_hegel.exe ppx/test/test_ppx_derive.exe \
      ppx/test/test_ppx_hegel_test.exe
    ./_build/default/ppx/test/test_ppx_derive.exe
    ./_build/default/ppx/test/test_ppx_hegel_test.exe
    ./_build/default/test/test_hegel.exe
    # ppx_expect tests are an inline-tests library (no standalone exe), so run
    # them through dune. --force ensures they execute even if dune considers
    # them cached.
    if [ "${HEGEL_SKIP_EXPECT_TESTS:-}" = "1" ]; then
      echo "Skipping ppx_expect suite (HEGEL_SKIP_EXPECT_TESTS=1)"
    else
      dune runtest ppx/test/expect_tests --force
    fi
    # The hegel.jane regression suite needs the core/sexp_diff opam depopts;
    # skip it on environments that don't install them.
    if [ "${HEGEL_SKIP_JANE_TESTS:-}" = "1" ]; then
      echo "Skipping hegel.jane suite (HEGEL_SKIP_JANE_TESTS=1)"
    else
      dune build lib/jane/test/test_hegel_jane.exe
      ./_build/default/lib/jane/test/test_hegel_jane.exe
      dune runtest lib/jane/test --force
      dune build ppx/test/test_ppx_derive_jane.exe
      ./_build/default/ppx/test/test_ppx_derive_jane.exe
    fi

# these aliases are provided as ux improvements for local developers. CI should use the longer
# forms.
test: check-tests
check: check-format check-docs check-tests
