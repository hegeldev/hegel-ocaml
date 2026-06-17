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
      test/test_ppx_derive.exe \
      test/test_ppx_hegel_test.exe
    export BISECT_FILE="$PWD/_build/default/test/bisect"
    ./_build/default/test/test_ppx_derive.exe
    ./_build/default/test/test_ppx_hegel_test.exe
    ./_build/default/test/test_hegel.exe
    # The ppx_expect tests are an inline-tests library (no standalone exe), so
    # run them through dune; coverage merges via BISECT_FILE.
    dune runtest test/expect_tests --instrument-with bisect_ppx --force
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
    dune build @doc 2>&1

docs:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build @doc 2>&1
    open _build/default/_doc/_html/index.html

check-tests-no-coverage:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build test/test_hegel.exe test/test_ppx_derive.exe test/test_ppx_hegel_test.exe
    ./_build/default/test/test_ppx_derive.exe
    ./_build/default/test/test_ppx_hegel_test.exe
    ./_build/default/test/test_hegel.exe
    # ppx_expect tests are an inline-tests library (no standalone exe), so run
    # them through dune. --force ensures they execute even if dune considers
    # them cached.
    dune runtest test/expect_tests --force

# these aliases are provided as ux improvements for local developers. CI should use the longer
# forms.
test: check-tests
check: check-format check-docs check-tests
