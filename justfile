set ignore-comments := true

check-tests:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    find _build -name '*.coverage' -delete 2>/dev/null || true
    dune runtest --instrument-with bisect_ppx --force
    python3 scripts/check-coverage.py

format:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune fmt || true

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

check-conformance:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env 2>/dev/null || true)
    dune build conformance/
    uv run --with 'hegel-core==0.4.0' --with pytest pytest test/conformance/ -v

# these aliases are provided as ux improvements for local developers. CI should use the longer
# forms.
test: check-tests
conformance: check-conformance
check: check-format check-docs check-tests
