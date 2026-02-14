setup:
    uv venv .venv
    uv pip install --python .venv/bin/python 'hegel @ git+ssh://git@github.com/antithesishq/hegel.git'

test:
    dune exec --instrument-with bisect_ppx test_unit/test_unit.exe
    dune exec --instrument-with bisect_ppx test/test_hegel.exe
    dune exec --instrument-with bisect_ppx test/test_generators.exe
    dune exec --instrument-with bisect_ppx test/test_docs.exe
    dune exec --instrument-with bisect_ppx test/test_failing.exe
    dune exec --instrument-with bisect_ppx test/test_coverage.exe

coverage: test
    bisect-ppx-report summary --per-file
    bisect-ppx-report html

clean-coverage:
    rm -f bisect*.coverage
    rm -rf _coverage
