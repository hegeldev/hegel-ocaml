(* Test that a failing property test gets properly shrunk *)
let () =
  try
    Hegel.run (fun () ->
        let xs =
          (Hegel.Gen.list ~min_size:1 ~max_size:20
             (Hegel.Gen.int ~min:0 ~max:100 ()))
            .generate ()
        in
        let sum = List.fold_left ( + ) 0 xs in
        assert (sum < 100));
    Printf.eprintf "ERROR: should have failed\n";
    exit 1
  with Failure msg when msg = "Property test failed" -> ()
