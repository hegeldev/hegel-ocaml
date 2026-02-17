format:
    dune fmt

test:
    dune runtest --instrument-with bisect_ppx

coverage: test
    bisect-ppx-report summary --per-file
    bisect-ppx-report html

clean-coverage:
    rm -f bisect*.coverage
    rm -rf _coverage
