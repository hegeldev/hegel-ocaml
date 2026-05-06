(** Conformance binary: triggers failures whose origin should deduplicate to
    exactly one distinct interesting test case.

    Two modes:
    - [value_in_error_message]: assertion fails with the generated value
      embedded in the message. A correct origin (exc_type + innermost file:line)
      deduplicates all failures to one.
    - [multiple_call_sites]: the same buggy function is reached from two
      different call paths. A correct origin (innermost frame only) deduplicates
      to one. *)

open Hegel.Conformance
open Hegel.Generators

exception Bug of int

let buggy_function x = if x > 10 then raise (Bug x) [@@inline never]
let call_path_a x = buggy_function x [@@inline never]
let call_path_b x = buggy_function x [@@inline never]

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let mode = Json_params.get_mode params in
  let test_cases = get_test_cases () in
  try
    Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
      (fun tc ->
        with_metrics (fun () ->
            let x = Hegel.draw tc (integers ~min_value:0 ~max_value:100 ()) in
            (match mode with
            | "value_in_error_message" ->
                if x > 10 then
                  failwith
                    (Printf.sprintf "Generated value %d exceeded threshold 10" x)
            | "multiple_call_sites" ->
                if x mod 2 = 0 then call_path_a x else call_path_b x
            | _ -> failwith ("unknown mode: " ^ mode));
            []))
  with Failure _ | Bug _ -> ()
