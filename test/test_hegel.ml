let () =
  (* Register first so it runs LAST (LIFO) — after all session at_exit
     handlers. If this message never appears, process exit is hanging before
     at_exit handlers run (H2). *)
  Stdlib.at_exit (fun () ->
      Printf.eprintf
        "[hegel-debug] final at_exit handler (registered first)\n%!");
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
      ("conformance_helpers", Test_conformance_helpers.tests);
      ("derive", Test_derive.tests);
    ]
