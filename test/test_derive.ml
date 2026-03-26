(** Tests for the {!Hegel.Derive} runtime support module.

    These tests verify the runtime helpers used by the [@@deriving generator]
    PPX:
    - {!Hegel.Derive.generate_option}: generates [Some v] or [None]
    - {!Hegel.Derive.generate_list}: generates a list of values *)

open Hegel

(** Test: generate_option E2E — generates both Some and None. *)
let test_generate_option_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Session.run_hegel_test ~test_cases:50 (fun tc ->
      let gen_fn tc =
        Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:10 ())
      in
      let result = Hegel.Derive.generate_option tc gen_fn in
      match result with
      | Some n ->
          assert (n >= 0 && n <= 10);
          saw_some := true
      | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none

(** Test: generate_list E2E — generates lists with correct elements. *)
let test_generate_list_e2e () =
  Session.run_hegel_test ~test_cases:20 (fun tc ->
      let gen_fn tc =
        Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
      in
      let result = Hegel.Derive.generate_list tc gen_fn in
      assert (List.length result >= 0 && List.length result <= 20);
      List.iter (fun n -> assert (n >= 0 && n <= 100)) result)

let tests =
  [
    Alcotest.test_case "generate_option e2e" `Quick test_generate_option_e2e;
    Alcotest.test_case "generate_list e2e" `Quick test_generate_list_e2e;
  ]
