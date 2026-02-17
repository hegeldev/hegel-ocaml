(lang dune 3.0)
(name hegel)
(version 0.1.0)
(generate_opam_files true)

(package
 (name hegel)
 (synopsis "OCaml SDK for Hegel property-based testing")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.0))
  (alcotest (and :with-test (>= 1.5.0)))))

(package
 (name ppx_hegel)
 (synopsis "PPX deriver for Hegel generators")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.0))
  (ppxlib (>= 0.28.0))
  (hegel (= :version))))
