setup:
    uv venv .venv
    uv pip install --python .venv/bin/python 'hegel @ git+ssh://git@github.com/antithesishq/hegel.git'

test:
    dune runtest --instrument-with bisect_ppx

coverage: test
    bisect-ppx-report summary --per-file
    bisect-ppx-report html

clean-coverage:
    rm -f bisect*.coverage
    rm -rf _coverage
