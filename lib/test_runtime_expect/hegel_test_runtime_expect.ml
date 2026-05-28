let () =
  Ppx_inline_test_lib.add_evaluator ~f:(fun () ->
    let failures = Hegel_test_runtime.run_all () in
    if Hegel.Blobs.any_corrected_written ()
    then Ppx_inline_test_lib.Test_result.Success
    else if failures > 0
    then Ppx_inline_test_lib.Test_result.Failure
    else Ppx_inline_test_lib.Test_result.Success)
;;
