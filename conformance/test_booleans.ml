(** Conformance binary: generates boolean values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~name:"test_booleans" ~test_cases (fun () ->
      let b = Hegel.draw (booleans ()) in
      write_metrics [ ("value", if b then "true" else "false") ])
