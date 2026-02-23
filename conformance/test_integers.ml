(** Conformance binary: generates integer values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators
open Hegel.Cbor_helpers

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_value = Json_params.get_int_opt params "min_value" in
  let max_value = Json_params.get_int_opt params "max_value" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_integers" ~test_cases (fun () ->
      let v = generate (integers ?min_value ?max_value ()) in
      let n = extract_int v in
      write_metrics [ ("value", string_of_int n) ])
