(** Tests for the [Hegel.Derive] runtime support module (doc-hidden; called by
    [@@deriving hegel_generator]-generated code).

    These tests verify the runtime helpers used by the [@@deriving hegel_generator]
    PPX:
    - [Derive.generate_option]: generates [Some v] or [None]
    - [Derive.generate_list]: generates a list of values *)

module Derive = Hegel.Derive

(** Test: generate_option E2E — generates both Some and None. *)
let test_generate_option_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:50 ()) (fun tc ->
    let gen_fn tc =
      Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:10 ())
    in
    match Derive.generate_option tc gen_fn with
    | Some n ->
      assert (n >= 0 && n <= 10);
      saw_some := true
    | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none
;;

(** Test: generate_list E2E — generates lists with correct elements. *)
let test_generate_list_e2e () =
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:20 ()) (fun tc ->
    let gen_fn tc =
      Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
    in
    let result = Derive.generate_list tc gen_fn in
    List.iter (fun n -> assert (n >= 0 && n <= 100)) result)
;;

(** Regression test: generate_list lengths are engine-driven via the collection
    protocol, with no hidden client-side cap. The old implementation drew a
    length in [0, 20]; engine-sized collections exceed 20 elements in roughly
    4% of cases, so over 1000 cases the maximum observed length exceeds 20 with
    near-certainty. *)
let test_generate_list_exceeds_old_cap () =
  let max_len = ref 0 in
  Hegel.run_hegel_test ~settings:(Hegel.settings ~test_cases:1000 ()) (fun tc ->
    let gen_fn tc =
      Hegel.draw tc (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
    in
    let len = List.length (Derive.generate_list tc gen_fn) in
    if len > !max_len then max_len := len);
  Alcotest.(check bool) "some list longer than 20" true (!max_len > 20)
;;

let tests =
  [ Alcotest.test_case "generate_option e2e" `Quick test_generate_option_e2e
  ; Alcotest.test_case "generate_list e2e" `Quick test_generate_list_e2e
  ; Alcotest.test_case
      "generate_list exceeds old cap"
      `Quick
      test_generate_list_exceeds_old_cap
  ]
;;
