(** Conformance binary: generates lists of integers and writes metrics.

    Uses filtered elements (CompositeList path) so that new_collection and
    collection_more commands are sent — required for error-mode conformance
    tests (stop_test_on_collection_more, stop_test_on_new_collection). *)

open Hegel.Conformance
open Hegel.Generators
open Hegel.Cbor_helpers

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let min_value = Json_params.get_int_opt params "min_value" in
  let max_value = Json_params.get_int_opt params "max_value" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_lists" ~test_cases (fun () ->
      (* Use filter(always_true) to force the CompositeList path, which sends
         new_collection and collection_more commands. This is required so that
         the error-mode conformance tests (stop_test_on_collection_more,
         stop_test_on_new_collection) work correctly. *)
      let elem_gen =
        filter (fun _ -> true) (integers ?min_value ?max_value ())
      in
      let list_gen = lists elem_gen ~min_size ?max_size () in
      let v = generate list_gen in
      let items = extract_list v in
      let size = List.length items in
      let ints = List.map extract_int items in
      let min_element =
        if size = 0 then None
        else Some (List.fold_left min (List.hd ints) (List.tl ints))
      in
      let max_element =
        if size = 0 then None
        else Some (List.fold_left max (List.hd ints) (List.tl ints))
      in
      write_metrics
        [
          ("size", string_of_int size);
          ("min_element", Json_params.int_opt_to_json min_element);
          ("max_element", Json_params.int_opt_to_json max_element);
        ])
