(** Conformance binary: generates integer values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let dummy_test_location = Json_params.dummy_test_location

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_value = Json_params.get_int_opt params "min_value" in
  let max_value = Json_params.get_int_opt params "max_value" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test
    ~settings:(Hegel.settings ~test_cases ())
    dummy_test_location
    (fun tc ->
       let n = Hegel.draw tc (integers ?min_value ?max_value ()) in
       write_metrics [ "value", string_of_int n ])
;;
