(* [Test_client.test_hegel_toml_config] re-executes this binary with a
   [hegel.toml] named by [HEGEL_CONFIG]; the child checks the settings it
   resolves and exits. *)
let () =
  match Stdlib.Sys.getenv_opt "HEGEL_TEST_CONFIG_CHILD" with
  | Some _ ->
    Test_client.config_child ();
    Stdlib.exit 0
  | None -> ()
;;

let () =
  Alcotest.run
    "hegel"
    [ "client", Test_client.tests
    ; "clone", Test_clone.tests
    ; "concurrency", Test_concurrency.tests
    ; "generators_core", Test_generators_core.tests
    ; "generators_primitives", Test_generators_primitives.tests
    ; "generators_collections", Test_generators_collections.tests
    ; "generators_combinators", Test_generators_combinators.tests
    ; "generators_functions", Test_generators_functions.tests
      (* loader forks child processes. Unix.fork fails for the rest of the
         process once any domain has been spawned, and the stateful suite's
         test-only parallel capability spawns domains, so loader must run first. *)
    ; "loader", Test_loader.tests
    ; "stateful", Test_stateful.tests
    ]
;;
