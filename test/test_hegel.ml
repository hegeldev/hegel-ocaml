let () =
  Alcotest.run
    "hegel"
    [ "client", Test_client.tests
    ; "generators_core", Test_generators_core.tests
    ; "generators_primitives", Test_generators_primitives.tests
    ; "generators_collections", Test_generators_collections.tests
    ; "generators_combinators", Test_generators_combinators.tests
    ; "derive", Test_derive.tests
    ; "stateful", Test_stateful.tests
    ; "printing", Test_printing.tests
    ; "single_test_case", Test_single_test_case.tests
    ; "antithesis", Test_antithesis.tests
    ]
;;
