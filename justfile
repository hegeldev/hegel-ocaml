set ignore-comments := true

check-tests:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    find _build -name '*.coverage' -delete 2>/dev/null || true
    # Run test binaries directly (not via dune runtest) so output streams
    # in real-time — dune runtest buffers output until completion, hiding
    # diagnostic messages when a test hangs.
    dune build --instrument-with bisect_ppx test/test_hegel.exe test/test_ppx_derive.exe
    export BISECT_FILE=_build/default/test/bisect
    ./_build/default/test/test_ppx_derive.exe
    ./_build/default/test/test_hegel.exe
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
    dune build test/test_hegel.exe test/test_ppx_derive.exe
    ./_build/default/test/test_ppx_derive.exe
    ./_build/default/test/test_hegel.exe

check-conformance:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env 2>/dev/null || true)
    dune build conformance/
    uv run --with 'hegel-core==0.7.0' --with pytest pytest test/conformance/ -v

# these aliases are provided as ux improvements for local developers. CI should use the longer
# forms.
test: check-tests
conformance: check-conformance
check: check-format check-docs check-tests
