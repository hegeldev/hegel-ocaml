(** Shared test utilities used across test modules. *)

external unsetenv : string -> unit = "caml_unsetenv"
(** [unsetenv name] removes the environment variable [name] from the process
    environment. Uses the POSIX [unsetenv(3)] function via a C stub. *)

open! Core
module Unix = Core_unix
module Mutex = Caml_threads.Mutex
module Thread = Caml_threads.Thread
open Hegel

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
let contains_substring s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.equal (String.sub s ~pos:i ~len:sublen) sub then true
      else check (i + 1)
    in
    check 0

(** [make_socket_pair ()] creates a connected socketpair. Returns [(fd1, fd2)].
*)
let make_socket_pair () =
  Core_unix.socketpair ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()

(** [make_connection fd ?name ?debug ()] creates a connection using the same fd
    for both reading and writing (for use with socketpairs). *)
let make_connection fd ?name ?debug () =
  Connection.create_connection ~read_fd:fd ~write_fd:fd ?name ?debug ()

(** [raw_handshake_responder fd] reads one raw handshake packet from [fd] and
    responds with ["Hegel/0.10"]. Used by tests that need the client's
    {!Connection.send_handshake} to succeed. This function reads and writes on a
    raw fd without a connection object, so it must be used BEFORE any connection
    is created on the same fd, or on a separate fd that has no background
    reader. *)
let raw_handshake_responder fd =
  let pkt = Protocol.read_packet fd in
  Protocol.write_packet fd
    {
      stream_id = pkt.stream_id;
      message_id = pkt.message_id;
      is_reply = true;
      payload = "Hegel/0.10";
    }

(** [handshake_via_stream peer_conn] handles a handshake on the peer side using
    the connection's stream API (compatible with the background reader). *)
let handshake_via_stream peer_conn =
  let ch = Connection.control_stream peer_conn in
  let msg_id, _payload = Connection.receive_request_raw ch in
  Connection.send_response_raw ch msg_id "Hegel/0.10";
  peer_conn.Connection.connection_state <- Connection.Client

(** [handshake_pair peer_conn client_conn] performs a handshake between peer and
    client connections. Uses the stream API for the peer side (compatible with
    the background reader thread). *)
let handshake_pair peer_conn client_conn =
  Printf.eprintf "[hegel-debug] handshake_pair: creating peer thread\n%!";
  let t = Thread.create handshake_via_stream peer_conn in
  Printf.eprintf "[hegel-debug] handshake_pair: send_handshake\n%!";
  let _version = Connection.send_handshake client_conn in
  Printf.eprintf "[hegel-debug] handshake_pair: joining peer thread\n%!";
  Thread.join t;
  Printf.eprintf "[hegel-debug] handshake_pair: done\n%!"

(** Helper: check where a given command is *)
let find_cmd cmd =
  let inp_stream = Core_unix.open_process_in ("which " ^ cmd) in
  let output = String.strip (In_channel.input_all inp_stream) in
  let exit_code = Core_unix.close_process_in inp_stream in
  match exit_code with
  | Ok () -> output
  | Error err ->
      raise
        (Failure
           (sprintf "Command failed with status: %s"
              (match err with
              | `Exit_non_zero code -> Int.to_string code
              | `Signal signal -> "signaled: " ^ Signal.to_string signal)))
