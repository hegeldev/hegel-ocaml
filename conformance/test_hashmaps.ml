(** Conformance binary: generates hash maps (dicts) and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators
open Hegel.Cbor_helpers

(** [int_opt_to_json v] serializes an optional int to JSON. *)
let int_opt_to_json = function None -> "null" | Some n -> string_of_int n

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size_opt = Json_params.get_int_opt params "max_size" in
  let max_size = match max_size_opt with Some ms -> ms | None -> 10 in
  let key_type = Json_params.get_string params "key_type" "integer" in
  let min_key = Json_params.get_int params "min_key" (-1000) in
  let max_key = Json_params.get_int params "max_key" 1000 in
  let min_value_p = Json_params.get_int params "min_value" (-1000) in
  let max_value_p = Json_params.get_int params "max_value" 1000 in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_hashmaps" ~test_cases (fun () ->
      let key_gen =
        if key_type = "string" then
          (* For string keys, use text with bounded length to ensure uniqueness
             within the key range. We use a text generator and map it. *)
          text ~min_size:1 ~max_size:10 ()
        else integers ~min_value:min_key ~max_value:max_key ()
      in
      let val_gen = integers ~min_value:min_value_p ~max_value:max_value_p () in
      let map_gen = hashmaps key_gen val_gen ~min_size ~max_size () in
      let v = generate map_gen in
      let pairs = extract_dict v in
      let size = List.length pairs in
      let values = List.map (fun (_, vv) -> extract_int vv) pairs in
      let min_val_result =
        if size = 0 then None
        else Some (List.fold_left min (List.hd values) (List.tl values))
      in
      let max_val_result =
        if size = 0 then None
        else Some (List.fold_left max (List.hd values) (List.tl values))
      in
      (* For key metrics: extract int keys if key_type is "integer" *)
      let min_key_result, max_key_result =
        if key_type = "integer" && size > 0 then
          let keys = List.map (fun (k, _) -> extract_int k) pairs in
          ( Some (List.fold_left min (List.hd keys) (List.tl keys)),
            Some (List.fold_left max (List.hd keys) (List.tl keys)) )
        else (None, None)
      in
      write_metrics
        [
          ("size", string_of_int size);
          ("min_key", int_opt_to_json min_key_result);
          ("max_key", int_opt_to_json max_key_result);
          ("min_value", int_opt_to_json min_val_result);
          ("max_value", int_opt_to_json max_val_result);
        ])
