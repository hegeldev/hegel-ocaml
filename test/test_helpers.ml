(** Shared test utilities used across test modules. *)

open Hegel.Connection

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
let contains_substring s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

(** Helper: create a socketpair fake server and client, run both, then join. *)
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

(** Helper: accept a run_test command on the control channel and return the test
    channel. *)
let accept_run_test server_conn =
  receive_handshake server_conn;
  let control = control_channel server_conn in
  let msg_id, message = receive_request control () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let test_ch_id =
    Int32.of_int
      (Hegel.Cbor_helpers.extract_int (List.assoc (`Text "channel_id") pairs))
  in
  let test_channel = connect_channel server_conn test_ch_id ~role:"Test" () in
  send_response_value control msg_id (`Bool true);
  test_channel

(** Helper: send a test_case event and return the data channel. *)
let send_test_case server_conn test_channel =
  let data_ch = new_channel server_conn ~role:"Data" () in
  let req_id =
    send_request test_channel
      (`Map
         [
           (`Text "event", `Text "test_case");
           (`Text "channel_id", `Int (Int32.to_int (channel_id data_ch)));
         ])
  in
  ignore (receive_response_raw test_channel req_id ());
  data_ch

(** Helper: send test_done with given interesting count. *)
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

(** Helper: receive a request, extract command name and pairs. *)
let recv_command data_ch =
  let msg_id, message = receive_request data_ch () in
  let pairs = Hegel.Cbor_helpers.extract_dict message in
  let cmd =
    Hegel.Cbor_helpers.extract_string (List.assoc (`Text "command") pairs)
  in
  (msg_id, cmd, pairs)
