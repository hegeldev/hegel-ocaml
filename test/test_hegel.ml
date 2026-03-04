let () =
  Alcotest.run "hegel"
    [
      ("protocol", Test_protocol.tests);
      ("cbor_helpers", Test_cbor_helpers.tests);
      ("connection", Test_connection.tests);
      ("client", Test_client.tests);
      ("generators_core", Test_generators_core.tests);
      ("generators_primitives", Test_generators_primitives.tests);
      ("generators_collections", Test_generators_collections.tests);
      ("generators_combinators", Test_generators_combinators.tests);
      ("showcase", Test_showcase.tests);
      ("conformance_helpers", Test_conformance_helpers.tests);
      ("derive", Test_derive.tests);
    ]
