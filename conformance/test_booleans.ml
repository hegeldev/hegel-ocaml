(** Conformance binary: generates boolean values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

let () =
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let b = Hegel.draw tc (booleans ()) in
      write_metrics [ ("value", if b then "true" else "false") ])
