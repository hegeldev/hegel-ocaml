(* When invoked with the [--__hegel_test_runtime_demo MODE] argv, this binary
   reuses itself as the subprocess for [Test_hegel_test_runtime]'s exit-code
   assertions. *)
let () =
  match Sys.argv with
  | [| _; "--__hegel_test_runtime_demo"; mode |] ->
    let run =
      match mode with
      | "fail" -> fun () -> failwith "deliberate"
      | _ -> fun () -> ()
    in
    Hegel_test_runtime.register ~name:"demo" ~file:__FILE__ ~line:__LINE__ run;
    Hegel_test_runtime.test_main ()
  | _ -> ()
;;

let () =
  (* Register first so it runs LAST (LIFO) — after all session at_exit
     handlers. If this message never appears, process exit is hanging before
     at_exit handlers run (H2). *)
  Stdlib.at_exit (fun () ->
    Printf.eprintf "[hegel-debug] final at_exit handler (registered first)\n%!");
  Alcotest.run
    "hegel"
    [ "protocol", Test_protocol.tests
    ; "crc32", Test_crc32.tests
    ; "cbor_helpers", Test_cbor_helpers.tests
    ; "cbor_vectors", Test_cbor_vectors.tests
    ; "connection", Test_connection.tests
    ; "client", Test_client.tests
    ; "generators_core", Test_generators_core.tests
    ; "generators_primitives", Test_generators_primitives.tests
    ; "generators_collections", Test_generators_collections.tests
    ; "generators_combinators", Test_generators_combinators.tests
    ; "conformance_helpers", Test_conformance_helpers.tests
    ; "derive", Test_derive.tests
    ; "stateful", Test_stateful.tests
    ; "single_test_case", Test_single_test_case.tests
    ; "antithesis", Test_antithesis.tests
    ; "hegel_test_runtime", Test_hegel_test_runtime.tests
    ]
;;
