type connection_state = {
  channel : Protocol.Channel.t;
  mutable span_depth : int;
}

let is_last_run = ref false
let generated_values : string list ref = ref []
let test_aborted = ref false
let connection : connection_state option ref = ref None

let set_last_run v = is_last_run := v
let get_last_run () = !is_last_run

let buffer_generated_value s =
  generated_values := s :: !generated_values

let take_generated_values () =
  let vs = List.rev !generated_values in
  generated_values := [];
  vs

let set_test_aborted v = test_aborted := v
let get_test_aborted () = !test_aborted

let set_connection ch =
  assert (!connection = None);
  connection := Some { channel = ch; span_depth = 0 }

let get_connection () = !connection

let clear_connection () = connection := None

let get_channel () =
  match !connection with
  | Some s -> s.channel
  | None -> failwith "No active connection"

let increment_span_depth () =
  match !connection with
  | Some s -> s.span_depth <- s.span_depth + 1
  | None -> failwith "start_span called with no active connection"

let decrement_span_depth () =
  match !connection with
  | Some s ->
    assert (s.span_depth > 0);
    s.span_depth <- s.span_depth - 1
  | None -> failwith "stop_span called with no active connection"
