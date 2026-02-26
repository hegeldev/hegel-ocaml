(** Tests for the {!Hegel.Derive} runtime support module.

    These tests verify the runtime helpers used by the [@@deriving generator]
    PPX:
    - {!Hegel.Derive.generate_option}: generates [Some v] or [None]
    - {!Hegel.Derive.generate_list}: generates a list of values *)

open Hegel.Connection

let with_fake_server = Test_helpers.with_fake_server
let accept_run_test = Test_helpers.accept_run_test
let send_test_case = Test_helpers.send_test_case
let send_test_done = Test_helpers.send_test_done
let recv_command = Test_helpers.recv_command

(* ==== generate_option tests via socketpair ==== *)

(** Test: generate_option returns Some when bool=true, None when bool=false. *)
let test_generate_option_some_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* generate_option calls generate(booleans()) first.
         If true → calls gen_fn() which calls generate(integers()).
         Sequence: generate(bool=true), generate(int=42), mark_complete *)
      let rec handle_cmds gen_count =
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            let gc = gen_count + 1 in
            if gc = 1 then send_response_value data_ch msg_id (`Bool true)
            else send_response_value data_ch msg_id (`Int 42);
            handle_cmds gc
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      in
      handle_cmds 0;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"opt_some" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Generators.generate (Hegel.Generators.integers ())
          in
          let result = Hegel.Derive.generate_option gen_fn in
          match result with
          | Some n -> Alcotest.(check int) "value" 42 n
          | None -> Alcotest.fail "expected Some"))

(** Test: generate_option returns None when bool=false. *)
let test_generate_option_none_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* generate_option calls generate(booleans()) = false → None *)
      let rec handle_cmds () =
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            send_response_value data_ch msg_id (`Bool false);
            handle_cmds ()
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      in
      handle_cmds ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"opt_none" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Generators.generate (Hegel.Generators.integers ())
          in
          let result = Hegel.Derive.generate_option gen_fn in
          Alcotest.(check bool) "is None" true (result = None)))

(* ==== generate_list tests via socketpair ==== *)

(** Test: generate_list generates correct-length list. *)
let test_generate_list_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      (* generate_list calls generate(integers(0,20)) for length first.
         Then gen_fn() N times, each calling generate(integers()). *)
      let rec handle_cmds gen_count =
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            let gc = gen_count + 1 in
            if gc = 1 then
              (* Length = 3 *)
              send_response_value data_ch msg_id (`Int 3)
            else
              (* Element values *)
              send_response_value data_ch msg_id (`Int (10 * gc));
            handle_cmds gc
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      in
      handle_cmds 0;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"list_gen" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Generators.generate (Hegel.Generators.integers ())
          in
          let result = Hegel.Derive.generate_list gen_fn in
          Alcotest.(check int) "length" 3 (List.length result)))

(** Test: generate_list with length 0. *)
let test_generate_list_empty_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      let rec handle_cmds () =
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            (* Length = 0 *)
            send_response_value data_ch msg_id (`Int 0);
            handle_cmds ()
        | "mark_complete" -> send_response_value data_ch msg_id `Null
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      in
      handle_cmds ();
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"list_empty" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Generators.generate (Hegel.Generators.integers ())
          in
          let result = Hegel.Derive.generate_list gen_fn in
          Alcotest.(check int) "length" 0 (List.length result)))

(* ==== E2E tests ==== *)

(** Test: generate_option E2E — generates both Some and None. *)
let test_generate_option_e2e () =
  let saw_some = ref false in
  let saw_none = ref false in
  Hegel.Session.run_hegel_test ~name:"derive_opt_e2e" ~test_cases:50 (fun () ->
      let gen_fn () =
        Hegel.Generators.generate
          (Hegel.Generators.integers ~min_value:0 ~max_value:10 ())
      in
      let result = Hegel.Derive.generate_option gen_fn in
      match result with
      | Some n ->
          assert (n >= 0 && n <= 10);
          saw_some := true
      | None -> saw_none := true);
  assert !saw_some;
  assert !saw_none

(** Test: generate_list E2E — generates lists with correct elements. *)
let test_generate_list_e2e () =
  Hegel.Session.run_hegel_test ~name:"derive_list_e2e" ~test_cases:20 (fun () ->
      let gen_fn () =
        Hegel.Generators.generate
          (Hegel.Generators.integers ~min_value:0 ~max_value:100 ())
      in
      let result = Hegel.Derive.generate_list gen_fn in
      assert (List.length result >= 0 && List.length result <= 20);
      List.iter (fun n -> assert (n >= 0 && n <= 100)) result)

let tests =
  [
    (* Socketpair tests *)
    Alcotest.test_case "generate_option Some socketpair" `Quick
      test_generate_option_some_socketpair;
    Alcotest.test_case "generate_option None socketpair" `Quick
      test_generate_option_none_socketpair;
    Alcotest.test_case "generate_list socketpair" `Quick
      test_generate_list_socketpair;
    Alcotest.test_case "generate_list empty socketpair" `Quick
      test_generate_list_empty_socketpair;
    (* E2E tests *)
    Alcotest.test_case "generate_option e2e" `Quick test_generate_option_e2e;
    Alcotest.test_case "generate_list e2e" `Quick test_generate_list_e2e;
  ]
