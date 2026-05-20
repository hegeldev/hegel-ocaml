(lang dune 3.16)

(name hegel)

(version 0.3.19)

(generate_opam_files true)

(source
 (github hegeldev/hegel-ocaml))

(license MIT)

(package
 (name hegel)
 (synopsis "Hegel property-based testing library for OCaml")
 (description
  "A property-based testing library that communicates with the Hegel server to enable universal property-based testing from OCaml.")
 (depends
  (ocaml
   (>= 5.2))
  (core
   (>= 0.17))
  (core_unix
   (>= 0.17))
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (ocplib-endian
   (>= 1.2))
  (ppx_js_style
   (>= v0.17))
  (bisect_ppx
   (and
    :with-test
    (>= 2.8)))
  (ocamlformat
   (and
    :with-test
    (= 0.29.0)))
  (yojson
   (>= 1.7))
  (odoc :with-doc)))

(package
 (name ppx_hegel_generator)
 (synopsis "PPX deriver for Hegel generator derivation")
 (description
  "Automatically derives Hegel generators for OCaml types annotated with [@@deriving generator].")
 (depends
  (ocaml
   (>= 5.2))
  (ppxlib
   (>= 0.28))
  (ppx_js_style
   (>= v0.17))
  (hegel
   (= :version))))

(package
 (name ppx_hegel_test)
 (synopsis "PPX expander for Hegel test definitions with Antithesis support")
 (description
  "Provides the [let%hegel_test] extension.")
 (depends
  (ocaml
   (>= 5.2))
  (ppxlib
   (>= 0.28))
  (ppx_js_style
   (>= v0.17))
  (hegel
   (= :version))))
