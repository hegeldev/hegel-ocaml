(** Conformance binary: generates binary byte strings and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let mode = Json_params.get_mode params in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let gen = binary ~min_size ?max_size () in
      let gen =
        if mode = "non_basic" then Json_params.make_non_basic gen else gen
      in
      let b = Hegel.draw tc gen in
      let length = String.length b in
      write_metrics [ ("length", string_of_int length) ])
