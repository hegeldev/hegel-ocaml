(** Smoke test for the opt-in [ppx_hegel_test.expect] inline-tests backend.
    Existence in this dune file confirms that:
    - [hegel.test_runtime_expect] links against [ppx_inline_test]'s
      runtime library cleanly
    - The expect-style backend stanza in [ppx/dune] parses
    - A trivial [let%hegel_test] under the expect backend builds and runs
      via [Ppx_inline_test_lib.exit]
    The actual [dune promote] integration ([(diff?)] rules for
    [.ml.corrected] siblings) is dune's responsibility and not exercised
    here — users get it transitively from [(extends ppx_inline_test)]. *)

let%hegel_test smoke_passing (tc : Hegel.Client.test_case) =
  let _ = Hegel.draw tc (Hegel.Generators.booleans ()) in
  ()
[@@settings Hegel.settings ~test_cases:3 ()]
;;
