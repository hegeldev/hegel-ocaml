(** Conformance binary: generates boolean values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let mode = Json_params.get_mode params in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let gen = booleans () in
      let gen =
        if mode = "non_basic" then Json_params.make_non_basic gen else gen
      in
      let b = Hegel.draw tc gen in
      write_metrics [ ("value", if b then "true" else "false") ])
