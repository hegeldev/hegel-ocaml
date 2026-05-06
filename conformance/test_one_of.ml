(** Conformance binary: generates one_of values and writes metrics.

    Three modes exercise the two one_of code paths:
    - [basic]: all branches are basic integer generators (single combined schema
      path)
    - [map_negate]: branches mapped through negation; still all-basic
    - [filter_even]: branches filtered to even values only, which is non-basic,
      forcing the compositional span path *)

open Hegel.Conformance
open Hegel.Generators

let parse_ranges params =
  match List.assoc_opt "ranges" params with
  | Some (`List items) ->
      List.map
        (fun item ->
          match item with
          | `Assoc r ->
              let lo = Json_params.get_int r "min_value" 0 in
              let hi = Json_params.get_int r "max_value" 0 in
              (lo, hi)
          | _ -> failwith "ranges: expected object")
        items
  | _ -> failwith "ranges: expected list"

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let mode = Json_params.get_mode params in
  let ranges = parse_ranges params in
  let make_branch (lo, hi) =
    let base = integers ~min_value:lo ~max_value:hi () in
    match mode with
    | "basic" -> base
    | "map_negate" -> map (fun n -> -n) base
    | "filter_even" -> filter (fun n -> n mod 2 = 0) base
    | _ -> failwith ("unknown mode: " ^ mode)
  in
  let branches = List.map make_branch ranges in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      with_metrics (fun () ->
          let n = Hegel.draw tc (one_of branches) in
          [ ("value", string_of_int n) ]))
