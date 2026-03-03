(** Conformance binary: generates sampled_from values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let options = Json_params.get_int_array params "options" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_sampled_from" ~test_cases (fun () ->
      let n = Hegel.draw (sampled_from options) in
      write_metrics [ ("value", string_of_int n) ])
