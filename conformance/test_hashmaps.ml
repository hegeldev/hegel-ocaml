(** Conformance binary: generates hash maps (dicts) and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

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
  if key_type = "string" then
    Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
      (fun tc ->
        let key_gen = text ~min_size:1 ~max_size:10 () in
        let val_gen =
          integers ~min_value:min_value_p ~max_value:max_value_p ()
        in
        let map_gen = hashmaps key_gen val_gen ~min_size ~max_size () in
        let pairs = Hegel.draw tc map_gen in
        let size = List.length pairs in
        let values = List.map snd pairs in
        let min_val_result =
          if size = 0 then None
          else Some (List.fold_left min (List.hd values) (List.tl values))
        in
        let max_val_result =
          if size = 0 then None
          else Some (List.fold_left max (List.hd values) (List.tl values))
        in
        write_metrics
          [
            ("size", string_of_int size);
            ("min_key", "null");
            ("max_key", "null");
            ("min_value", Json_params.int_opt_to_json min_val_result);
            ("max_value", Json_params.int_opt_to_json max_val_result);
          ])
  else
    Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
      (fun tc ->
        let key_gen = integers ~min_value:min_key ~max_value:max_key () in
        let val_gen =
          integers ~min_value:min_value_p ~max_value:max_value_p ()
        in
        let map_gen = hashmaps key_gen val_gen ~min_size ~max_size () in
        let pairs = Hegel.draw tc map_gen in
        let size = List.length pairs in
        let keys = List.map fst pairs in
        let values = List.map snd pairs in
        let min_key_result =
          if size = 0 then None
          else Some (List.fold_left min (List.hd keys) (List.tl keys))
        in
        let max_key_result =
          if size = 0 then None
          else Some (List.fold_left max (List.hd keys) (List.tl keys))
        in
        let min_val_result =
          if size = 0 then None
          else Some (List.fold_left min (List.hd values) (List.tl values))
        in
        let max_val_result =
          if size = 0 then None
          else Some (List.fold_left max (List.hd values) (List.tl values))
        in
        write_metrics
          [
            ("size", string_of_int size);
            ("min_key", Json_params.int_opt_to_json min_key_result);
            ("max_key", Json_params.int_opt_to_json max_key_result);
            ("min_value", Json_params.int_opt_to_json min_val_result);
            ("max_value", Json_params.int_opt_to_json max_val_result);
          ])
