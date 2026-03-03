(** Shared test utilities used across test modules. *)

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

(** [raw_handshake_responder fd] reads one raw handshake packet from [fd] and
    responds with ["Hegel/0.3"]. Used by tests that need the client's
    {!Hegel.Connection.send_handshake} to succeed without the removed
    [receive_handshake]. *)
let raw_handshake_responder fd =
  let pkt = Hegel.Protocol.read_packet fd in
  Hegel.Protocol.write_packet fd
    {
      channel_id = pkt.channel_id;
      message_id = pkt.message_id;
      is_reply = true;
      payload = "Hegel/0.3";
    }

(** Helper: check where a given command is *)
let find_cmd cmd =
  let inp_channel = Unix.open_process_in ("which " ^ cmd) in
  let output = String.trim (In_channel.input_all inp_channel) in
  let exit_code = Unix.close_process_in inp_channel in
  match exit_code with
  | WEXITED 0 -> output
  | exit_code ->
      raise
        (Failure
           (Printf.sprintf "Command failed with status: %s"
              (match exit_code with
              | WEXITED code -> string_of_int code
              | WSIGNALED sign -> "signaled: " ^ string_of_int sign
              | WSTOPPED sign -> "stopped: " ^ string_of_int sign)))
