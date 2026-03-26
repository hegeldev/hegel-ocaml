(** Shared test utilities used across test modules. *)

external unsetenv : string -> unit = "caml_unsetenv"
(** [unsetenv name] removes the environment variable [name] from the process
    environment. Uses the POSIX [unsetenv(3)] function via a C stub. *)

open Hegel

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

(** [make_socket_pair ()] creates a connected socketpair. Returns [(fd1, fd2)].
*)
let make_socket_pair () = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0

(** [make_connection fd ?name ?debug ()] creates a connection using the same fd
    for both reading and writing (for use with socketpairs). *)
let make_connection fd ?name ?debug () =
  Connection.create_connection ~read_fd:fd ~write_fd:fd ?name ?debug ()

(** [raw_handshake_responder fd] reads one raw handshake packet from [fd] and
    responds with ["Hegel/0.3"]. Used by tests that need the client's
    {!Connection.send_handshake} to succeed. This function reads and writes on a
    raw fd without a connection object, so it must be used BEFORE any connection
    is created on the same fd, or on a separate fd that has no background
    reader. *)
let raw_handshake_responder fd =
  let pkt = Protocol.read_packet fd in
  Protocol.write_packet fd
    {
      channel_id = pkt.channel_id;
      message_id = pkt.message_id;
      is_reply = true;
      payload = "Hegel/0.3";
    }

(** [handshake_via_channel peer_conn] handles a handshake on the peer side using
    the connection's channel API (compatible with the background reader). *)
let handshake_via_channel peer_conn =
  let ch = Connection.control_channel peer_conn in
  let msg_id, _payload = Connection.receive_request_raw ch () in
  Connection.send_response_raw ch msg_id "Hegel/0.3";
  peer_conn.Connection.connection_state <- Connection.Client

(** [handshake_pair peer_conn client_conn] performs a handshake between peer and
    client connections. Uses the channel API for the peer side (compatible with
    the background reader thread). *)
let handshake_pair peer_conn client_conn =
  let t = Thread.create handshake_via_channel peer_conn in
  let _version = Connection.send_handshake client_conn in
  Thread.join t

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
