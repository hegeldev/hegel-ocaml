(lang dune 3.16)

(name hegel)

(generate_opam_files true)

(source
 (github antithesishq/hegel-ocaml))

(authors "Antithesis <engineering@antithesis.com>")

(maintainers "Antithesis <engineering@antithesis.com>")

(license ISC)

(package
 (name hegel)
 (synopsis "Hegel property-based testing SDK for OCaml")
 (description
  "A property-based testing SDK that communicates with the Hegel server (hegeld) to enable universal property-based testing from OCaml.")
 (depends
  (ocaml
   (>= 5.2))
  (dune
   (>= 3.16))
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (bisect_ppx
   (and
    :with-test
    (>= 2.8)))
  (odoc :with-doc)))
