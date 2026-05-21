(** Conformance binary: generates boolean values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let dummy_test_location = Json_params.dummy_test_location

let () =
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test
    ~settings:(Hegel.settings ~test_cases ())
    dummy_test_location
    (fun tc ->
       let b = Hegel.draw tc (booleans ()) in
       write_metrics [ ("value", if b then "true" else "false") ])
;;
