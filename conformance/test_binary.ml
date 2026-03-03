(** Conformance binary: generates binary byte strings and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_binary" ~test_cases (fun () ->
      let b = Hegel.draw (binary ~min_size ?max_size ()) in
      let length = String.length b in
      write_metrics [ ("length", string_of_int length) ])
