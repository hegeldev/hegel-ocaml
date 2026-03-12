# Hegel SDK for OCaml
# This justfile provides the standard build recipes.

# Install dependencies.
setup:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    opam install --yes odoc

# Run tests with 100% code coverage enforcement.
test:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    # Clean previous coverage data
    find _build -name '*.coverage' -delete 2>/dev/null || true
    # Run tests with bisect_ppx instrumentation
    dune runtest --instrument-with bisect_ppx --force
    # Enforce 100% coverage
    python3 scripts/check-coverage.py

# Auto-format code.
format:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune fmt 2>&1 || true

# Check formatting + linting.
lint:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build @fmt

# Build API documentation from source. Must succeed with zero warnings.
docs:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env)
    dune build @doc 2>&1

# Run conformance tests against the Hegel conformance framework.
conformance:
    #!/usr/bin/env bash
    set -euo pipefail
    eval $(opam env 2>/dev/null || true)
    # Ensure .hegel/venv exists with hegel installed (same venv the SDK uses)
    if [ ! -f .hegel/venv/bin/hegel ]; then
        mkdir -p .hegel
        uv venv --clear .hegel/venv
        uv pip install --python .hegel/venv/bin/python \
            "hegel @ git+ssh://git@github.com/antithesishq/hegel-core.git"
    fi
    # Install pytest if not already available
    if ! .hegel/venv/bin/python -c "import pytest" 2>/dev/null; then
        uv pip install --python .hegel/venv/bin/python pytest
    fi
    export PATH="$(pwd)/.hegel/venv/bin:$PATH"
    # Build conformance binaries
    dune build conformance/
    # Run Python conformance tests
    .hegel/venv/bin/python -m pytest test/conformance/ -v

# Update the pinned hegel-core version to the latest release.
update-hegel-core-version:
    #!/usr/bin/env bash
    set -euo pipefail
    tag=$(gh api repos/antithesishq/hegel-core/releases/latest --jq '.tag_name')
    sed -i '' "s/^let hegel_version = \".*\"/let hegel_version = \"${tag}\"/" lib/session.ml
    echo "Updated hegel_version to ${tag}"
    # Clear cached install so the next test run picks up the new version
    rm -rf .hegel/venv

# Run lint + docs + test (the full CI check).
check: lint docs test
