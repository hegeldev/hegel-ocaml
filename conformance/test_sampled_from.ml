(** Conformance binary: generates sampled_from values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let options = Json_params.get_int_array params "options" in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let n = Hegel.draw tc (sampled_from options) in
      write_metrics [ ("value", string_of_int n) ])
