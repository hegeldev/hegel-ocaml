format:
    # `dune fmt` exits with nonzero exit code if it makes any changes. That's
    # fine for a `check` command, but misleading for local development.
    dune fmt || true

test:
    dune runtest --instrument-with bisect_ppx

coverage: test
    bisect-ppx-report summary --per-file
    bisect-ppx-report html

clean-coverage:
    rm -f bisect*.coverage
    rm -rf _coverage
