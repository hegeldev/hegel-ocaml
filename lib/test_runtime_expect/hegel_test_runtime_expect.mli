(** Glue library that hooks {!Hegel_test_runtime} into [ppx_inline_test]'s
    runner. Linking this library has the side effect of registering an
    evaluator with [Ppx_inline_test_lib.add_evaluator] so that
    [Ppx_inline_test_lib.exit ()] runs every [let%hegel_test] in the
    library and reports the combined status.

    Users opt into the [dune promote] workflow by selecting this library's
    backend stanza in their dune file:

    {v
    (inline_tests (backend ppx_hegel_test.expect))
    v}

    With the default backend ([ppx_hegel_test]) this library is not linked
    and [ppx_inline_test] is not a transitive dependency. *)
