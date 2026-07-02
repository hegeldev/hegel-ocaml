(lang dune 3.16)

(name hegel)

(version 0.8.0)

(generate_opam_files true)

(using dune_site 0.1)

(source
 (github hegeldev/hegel-ocaml))

(license MIT)

(maintainers "Ethan Chou <ethan.chou@antithesis.com>")

(authors "Hegel Developers <https://github.com/hegeldev>")

(package
 (name hegel)
 (sites
  (lib libhegel))
 (synopsis "Hegel property-based testing library for OCaml")
 (description
  "A property-based testing library that communicates with the Hegel engine to enable universal property-based testing from OCaml.")
 (depends
  (ocaml
   (>= 5.1))
  (core
   (>= 0.17))
  (core_unix
   (>= 0.17))
  (ctypes
   (>= 0.20))
  (ctypes-foreign
   (>= 0.20))
  (dune-site
   (>= 3.16))
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (ppx_expect
   (and
    :with-test
    (>= v0.17)))
  (ocplib-endian
   (>= 1.2))
  (ppx_js_style
   (>= v0.17))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8)))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (yojson
   (>= 1.7))
  (odoc :with-doc)))

(package
 (name ppx_hegel_compat)
 (synopsis "Internal ppxlib AST shim shared by Hegel's PPX packages")
 (description
  "Abstracts over the differences between standard OCaml's ppxlib AST and OxCaml's ppxlib AST (labeled tuples, constructor modalities) so the Hegel PPX deriver and test rewriter can share a single implementation. Not intended for direct use.")
 (depends
  (ocaml
   (>= 5.1))
  (ppxlib
   (>= 0.28))))

(package
 (name ppx_hegel_generator)
 (synopsis "PPX deriver for Hegel generator derivation")
 (description
  "Automatically derives Hegel generators for OCaml types annotated with [@@deriving hegel_generator].")
 (depends
  (ocaml
   (>= 5.1))
  (ppxlib
   (>= 0.28))
  (ppx_js_style
   (>= v0.17))
  (ppx_hegel_compat
   (= :version))
  (hegel
   (= :version))
  (ppx_hegel_test
   (and
    :with-test
    (= :version)))
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (core
   (and
    :with-test
    (>= 0.17)))
  (core_unix
   (and
    :with-test
    (>= 0.17)))))

(package
 (name ppx_hegel_test)
 (synopsis "PPX expander for Hegel test definitions with Antithesis support")
 (description
  "Provides the [let%hegel_test] extension.")
 (depends
  (ocaml
   (>= 5.1))
  (ppxlib
   (>= 0.28))
  (ppx_js_style
   (>= v0.17))
  (ppx_hegel_compat
   (= :version))
  (hegel
   (= :version))
  (alcotest
   (and
    :with-test
    (>= 1.7)))
  (core
   (and
    :with-test
    (>= 0.17)))
  (core_unix
   (and
    :with-test
    (>= 0.17)))
  (yojson
   (and
    :with-test
    (>= 1.7)))))
