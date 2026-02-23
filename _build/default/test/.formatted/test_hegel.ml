let test_version () =
  Alcotest.(check string) "version string" "0.1.0" (Hegel.version ())

let () =
  Alcotest.run "hegel"
    [
      ("version", [ Alcotest.test_case "returns version" `Quick test_version ]);
    ]
