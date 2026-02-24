(** Conformance binary: generates boolean values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators
open Hegel.Cbor_helpers

let () =
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_booleans" ~test_cases (fun () ->
      let v = generate (booleans ()) in
      let b = extract_bool v in
      write_metrics [ ("value", if b then "true" else "false") ])
