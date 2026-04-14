(** Conformance binary: generates lists of integers and writes metrics.

    Forces the CompositeList path (non-basic elements) when [mode] is
    ["non_basic"] or when a collection-related protocol test mode is active,
    otherwise uses the basic schema path. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let min_value = Json_params.get_int_opt params "min_value" in
  let max_value = Json_params.get_int_opt params "max_value" in
  let mode = Json_params.get_mode params in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let test_mode =
        try Sys.getenv "HEGEL_PROTOCOL_TEST_MODE" with Not_found -> ""
      in
      let needs_non_basic =
        mode = "non_basic"
        || test_mode = "stop_test_on_collection_more"
        || test_mode = "stop_test_on_new_collection"
      in
      let base_elem = integers ?min_value ?max_value () in
      let elem_gen =
        if needs_non_basic then Json_params.make_non_basic base_elem
        else base_elem
      in
      let list_gen = lists elem_gen ~min_size ?max_size () in
      let items = Hegel.draw tc list_gen in
      let elements_json =
        "[" ^ String.concat "," (List.map string_of_int items) ^ "]"
      in
      write_metrics [ ("elements", elements_json) ])
