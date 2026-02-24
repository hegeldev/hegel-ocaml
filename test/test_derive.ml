(** Tests for the {!Hegel.Derive} runtime support module.

    These tests verify the runtime helpers used by the [@@deriving generator]
    PPX:
    - {!Hegel.Derive.generate_option}: generates [Some v] or [None]
    - {!Hegel.Derive.generate_list}: generates a list of values *)

open Hegel.Connection

(* ==== Socketpair helpers ==== *)

let with_fake_server server_fn client_fn =
  let server_socket, client_socket =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server_conn = create_connection server_socket ~name:"Server" () in
  let client_conn = create_connection client_socket ~name:"Client" () in
  let t = Thread.create server_fn server_conn in
  client_fn client_conn;
  close client_conn;
  close server_conn;
  Thread.join t

let accept_run_test server_conn =
  receive_handshake server_conn;
  let control = control_channel server_conn in
  let msg_id, message = receive_request control () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let test_ch_id =
    Int32.of_int
      (Hegel.Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
  in
  let test_channel = connect_channel server_conn test_ch_id ~role:"Test" () in
  send_response_value control msg_id (`Bool true);
  test_channel

let send_test_case server_conn test_channel =
  let data_ch = new_channel server_conn ~role:"Data" () in
  let req_id =
    send_request test_channel
      (`Map
         [
           (`Text "event", `Text "test_case");
           (`Text "channel", `Int (Int32.to_int data_ch.channel_id));
         ])
  in
  ignore (receive_response_raw test_channel req_id ());
  data_ch

let send_test_done test_channel ~interesting =
  ignore
    (pending_get
       (request test_channel
          (`Map
             [
               (`Text "event", `Text "test_done");
               ( `Text "results",
                 `Map
                   [
                     (`Text "passed", `Bool (interesting = 0));
                     (`Text "test_cases", `Int 1);
                     ( `Text "valid_test_cases",
                       `Int (if interesting = 0 then 1 else 0) );
                     (`Text "invalid_test_cases", `Int 0);
                     (`Text "interesting_test_cases", `Int interesting);
                   ] );
             ])))

let recv_command data_ch =
  let msg_id, message = receive_request data_ch () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let cmd =
    Hegel.Cbor_helpers.extract_string (List.assoc (`Text "command") pairs)
  in
  (msg_id, cmd, pairs)

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
      let gen_count = ref 0 in
      let done_ = ref false in
      while not !done_ do
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            incr gen_count;
            if !gen_count = 1 then
              send_response_value data_ch msg_id (`Bool true)
            else send_response_value data_ch msg_id (`Int 42)
        | "mark_complete" ->
            send_response_value data_ch msg_id `Null;
            done_ := true
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"opt_some" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Cbor_helpers.extract_int
              (Hegel.Generators.generate (Hegel.Generators.integers ()))
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
      let done_ = ref false in
      while not !done_ do
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" -> send_response_value data_ch msg_id (`Bool false)
        | "mark_complete" ->
            send_response_value data_ch msg_id `Null;
            done_ := true
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"opt_none" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Cbor_helpers.extract_int
              (Hegel.Generators.generate (Hegel.Generators.integers ()))
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
      let gen_count = ref 0 in
      let done_ = ref false in
      while not !done_ do
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            incr gen_count;
            if !gen_count = 1 then
              (* Length = 3 *)
              send_response_value data_ch msg_id (`Int 3)
            else
              (* Element values *)
              send_response_value data_ch msg_id (`Int (10 * !gen_count))
        | "mark_complete" ->
            send_response_value data_ch msg_id `Null;
            done_ := true
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"list_gen" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Cbor_helpers.extract_int
              (Hegel.Generators.generate (Hegel.Generators.integers ()))
          in
          let result = Hegel.Derive.generate_list gen_fn in
          Alcotest.(check int) "length" 3 (List.length result)))

(** Test: generate_list with length 0. *)
let test_generate_list_empty_socketpair () =
  with_fake_server
    (fun server_conn ->
      let test_channel = accept_run_test server_conn in
      let data_ch = send_test_case server_conn test_channel in
      let done_ = ref false in
      while not !done_ do
        let msg_id, cmd, _pairs = recv_command data_ch in
        match cmd with
        | "generate" ->
            (* Length = 0 *)
            send_response_value data_ch msg_id (`Int 0)
        | "mark_complete" ->
            send_response_value data_ch msg_id `Null;
            done_ := true
        | other -> failwith (Printf.sprintf "Unexpected: %s" other)
      done;
      send_test_done test_channel ~interesting:0)
    (fun client_conn ->
      let c = Hegel.Client.create_client client_conn in
      Hegel.Client.run_test c ~name:"list_empty" ~test_cases:1 (fun () ->
          let gen_fn () =
            Hegel.Cbor_helpers.extract_int
              (Hegel.Generators.generate (Hegel.Generators.integers ()))
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
        Hegel.Cbor_helpers.extract_int
          (Hegel.Generators.generate
             (Hegel.Generators.integers ~min_value:0 ~max_value:10 ()))
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
        Hegel.Cbor_helpers.extract_int
          (Hegel.Generators.generate
             (Hegel.Generators.integers ~min_value:0 ~max_value:100 ()))
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
