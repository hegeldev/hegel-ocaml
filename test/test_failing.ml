let test_failing_property_shrunk () =
  match
    Hegel.run (fun () ->
        let xs =
          (Hegel.Gen.list ~min_size:1 ~max_size:20
             (Hegel.Gen.int ~min:0 ~max:100 ()))
            .generate ()
        in
        let sum = List.fold_left ( + ) 0 xs in
        assert (sum < 100))
  with
  | () -> Alcotest.fail "expected Failure exception"
  | exception Failure msg ->
      Alcotest.(check string) "failure message" "Property test failed" msg

let () =
  Alcotest.run "Hegel Failing"
    [
      ( "shrinking",
        [
          Alcotest.test_case "failing property is shrunk" `Quick
            test_failing_property_shrunk;
        ] );
    ]
