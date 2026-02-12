let reject_marker = "HEGEL_REJECT"

let find_hegel_path () =
  let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
  let dirs = String.split_on_char ':' path in
  let rec find = function
    | [] -> None
    | dir :: rest ->
      let p = Filename.concat dir "hegel" in
      if Sys.file_exists p then Some p else find rest
  in
  find dirs

let extract_channel_id event =
  match Cbor.map_get event "channel" with
  | Some v -> (
    match Cbor.as_int v with
    | Some n -> n
    | None -> failwith "Missing channel id")
  | None -> failwith "Missing channel id"

let rec run ?(test_cases = 100) ?hegel_path test_fn =
  let hegel_path =
    match hegel_path with
    | Some p -> p
    | None -> (
      match find_hegel_path () with
      | Some p -> p
      | None -> failwith "hegel binary not found on PATH")
  in
  let temp_dir = Filename.temp_dir "hegel" "" in
  let socket_path = Filename.concat temp_dir "hegel.sock" in
  let cleanup_temp () =
    (try Sys.remove socket_path with Sys_error _ -> ());
    (try Sys.rmdir temp_dir with Sys_error _ -> ())
  in
  let pid =
    Unix.create_process hegel_path
      [|
        hegel_path;
        socket_path;
        "--verbosity";
        "normal";
        "--test-cases";
        string_of_int test_cases;
      |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let cleanup_process () =
    (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
  in
  (* Wait for socket *)
  let fd = ref None in
  (try
     for attempt = 0 to 49 do
       if !fd = None && Sys.file_exists socket_path then begin
         try
           let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
           Unix.connect sock (Unix.ADDR_UNIX socket_path);
           fd := Some sock
         with Unix.Unix_error _ when attempt < 49 -> Unix.sleepf 0.1
       end
       else if !fd = None then Unix.sleepf 0.1
     done
   with _ -> ());
  let sock =
    match !fd with
    | Some s -> s
    | None ->
      cleanup_process ();
      cleanup_temp ();
      failwith "Timeout waiting for hegel socket"
  in
  let conn = Protocol.Connection.create sock in
  let cleanup () =
    (try Protocol.Connection.close conn with _ -> ());
    cleanup_process ();
    cleanup_temp ()
  in
  (try
     (* Version negotiation *)
     let control = Protocol.Connection.control_channel conn in
     let req_id =
       Protocol.Channel.send_request control Protocol.version_negotiation_message
     in
     let response = Protocol.Channel.receive_response control req_id in
     if response <> Protocol.version_negotiation_ok then
       failwith (Printf.sprintf "Version negotiation failed: %s" response);
     (* Create test channel *)
     let test_channel = Protocol.Connection.new_channel conn in
     let test_channel_id = Protocol.Channel.channel_id test_channel in
     (* Send run_test *)
     let run_test_msg =
       Cbor.Map
         [
           (Cbor.Text "command", Cbor.Text "run_test");
           (Cbor.Text "name", Cbor.Text "test");
           (Cbor.Text "test_cases", Cbor.Unsigned test_cases);
           (Cbor.Text "channel", Cbor.Unsigned test_channel_id);
         ]
     in
     let run_test_id =
       Protocol.Channel.send_request control (Cbor.encode_to_string run_test_msg)
     in
     let _ = Protocol.Channel.receive_response control run_test_id in
     (* Handle test_case events *)
     let got_interesting = ref false in
     let ack_null =
       Cbor.encode_to_string (Cbor.Map [ (Cbor.Text "result", Cbor.Null) ])
     in
     let result_data = ref Cbor.Null in
     let running = ref true in
     while !running do
       let event_id, event_payload =
         Protocol.Channel.receive_request test_channel
       in
       let event = Cbor.decode_string event_payload in
       let event_type =
         match Cbor.map_get event "event" with
         | Some (Cbor.Text s) -> s
         | _ -> ""
       in
       match event_type with
       | "test_case" ->
         let channel_id = extract_channel_id event in
         let tc_channel = Protocol.Connection.connect_channel conn channel_id in
         Protocol.Channel.send_response test_channel event_id ack_null;
         run_test_case tc_channel test_fn false got_interesting
       | "test_done" ->
         let ack_true =
           Cbor.encode_to_string
             (Cbor.Map [ (Cbor.Text "result", Cbor.Bool true) ])
         in
         Protocol.Channel.send_response test_channel event_id ack_true;
         result_data :=
           (match Cbor.map_get event "results" with
           | Some v -> v
           | None -> Cbor.Null);
         running := false
       | _ -> Protocol.Channel.send_response test_channel event_id ack_null
     done;
     (* Process final replay test cases *)
     let n_interesting =
       match Cbor.map_get !result_data "interesting_test_cases" with
       | Some v -> (match Cbor.as_int v with Some n -> n | None -> 0)
       | None -> 0
     in
     for _ = 1 to n_interesting do
       let event_id, event_payload =
         Protocol.Channel.receive_request test_channel
       in
       let event = Cbor.decode_string event_payload in
       let channel_id = extract_channel_id event in
       let tc_channel = Protocol.Connection.connect_channel conn channel_id in
       Protocol.Channel.send_response test_channel event_id ack_null;
       run_test_case tc_channel test_fn true got_interesting
     done;
     let passed =
       match Cbor.map_get !result_data "passed" with
       | Some (Cbor.Bool b) -> b
       | _ -> true
     in
     (try Protocol.Channel.close test_channel with _ -> ());
     (try Protocol.Channel.close control with _ -> ());
     cleanup ();
     if (not passed) || !got_interesting then failwith "Property test failed"
   with exn ->
     cleanup ();
     raise exn)

and run_test_case tc_channel test_fn is_final got_interesting =
  State.set_last_run is_final;
  State.set_connection tc_channel;
  State.set_test_aborted false;
  let result =
    try
      test_fn ();
      `Valid
    with
    | Gen.Assume_rejected -> `Invalid
    | Failure msg when msg = reject_marker -> `Invalid
    | exn -> `Interesting exn
  in
  let status, origin =
    match result with
    | `Valid -> ("VALID", Cbor.Null)
    | `Invalid -> ("INVALID", Cbor.Null)
    | `Interesting exn ->
      got_interesting := true;
      if is_final then begin
        Printf.eprintf "Exception: %s\n%!" (Printexc.to_string exn);
        List.iter
          (fun v -> Printf.eprintf "%s\n%!" v)
          (State.take_generated_values ())
      end;
      let origin_str =
        Printf.sprintf "Exception: %s" (Printexc.to_string exn)
      in
      ("INTERESTING", Cbor.Text origin_str)
  in
  let was_aborted = State.get_test_aborted () in
  if not was_aborted then begin
    (match State.get_connection () with
    | Some _state ->
      let mark_complete =
        Cbor.Map
          [
            (Cbor.Text "command", Cbor.Text "mark_complete");
            (Cbor.Text "status", Cbor.Text status);
            (Cbor.Text "origin", origin);
          ]
      in
      let ch = State.get_channel () in
      (try ignore (Protocol.Channel.request_cbor ch mark_complete) with _ -> ());
      (try Protocol.Channel.close ch with _ -> ())
    | None -> ())
  end;
  State.clear_connection ()
