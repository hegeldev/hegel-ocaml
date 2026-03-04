(** Conformance binary: generates float values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

(** [json_float f] serializes a float to JSON, handling NaN and Infinity. *)
let json_float f =
  if Float.is_nan f then "null"
  else if Float.is_infinite f then if f > 0.0 then "1e308" else "-1e308"
  else Printf.sprintf "%.17g" f

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_value = Json_params.get_float_opt params "min_value" in
  let max_value = Json_params.get_float_opt params "max_value" in
  let exclude_min = Json_params.get_bool params "exclude_min" false in
  let exclude_max = Json_params.get_bool params "exclude_max" false in
  let allow_nan = Json_params.get_bool_opt params "allow_nan" in
  let allow_infinity = Json_params.get_bool_opt params "allow_infinity" in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~name:"test_floats" ~test_cases (fun () ->
      let f =
        Hegel.draw
          (floats ?min_value ?max_value ~exclude_min ~exclude_max ?allow_nan
             ?allow_infinity ())
      in
      let is_nan = Float.is_nan f in
      let is_infinite = Float.is_infinite f in
      write_metrics
        [
          ("value", json_float f);
          ("is_nan", if is_nan then "true" else "false");
          ("is_infinite", if is_infinite then "true" else "false");
        ])
