(** Register Hegel's test registry as an evaluator for [ppx_inline_test].
    See the accompanying [.mli] for the high-level description. *)

let () =
  Ppx_inline_test_lib.add_evaluator ~f:(fun () ->
    let failures = Hegel_test_runtime.run_all () in
    if failures > 0
    then Ppx_inline_test_lib.Test_result.Failure
    else Ppx_inline_test_lib.Test_result.Success)
;;
