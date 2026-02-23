(** Test runner and lifecycle management for the Hegel SDK.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. It manages:
    - Test lifecycle (run_test, test_case events, mark_complete)
    - Thread-local state for the current data channel
    - Helper functions (assume, note, target, generate_from_schema)
    - Origin extraction for error reporting *)

open Connection

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

(** Thread-local state for the current test case. *)
let current_channel : channel option ref = ref None

(** Thread-local flag indicating the final (replay) run. *)
let is_final_run : bool ref = ref false

(** Thread-local flag indicating the test was aborted (StopTest). *)
let test_aborted : bool ref = ref false

(** Thread-local flag indicating we are inside a test case. *)
let in_test : bool ref = ref false

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)
let extract_origin exn =
  let bt = Printexc.get_raw_backtrace () in
  let slots = Printexc.backtrace_slots bt in
  match slots with
  | None -> Printf.sprintf "%s at :0" (Printexc.exn_slot_name exn)
  | Some slots ->
      (* Find the last slot with location info *)
      let filename = ref "" in
      let lineno = ref 0 in
      Array.iter
        (fun slot ->
          Option.iter
            (fun (loc : Printexc.location) ->
              filename := loc.filename;
              lineno := loc.line_number)
            (Printexc.Slot.location slot))
        slots;
      Printf.sprintf "%s at %s:%d"
        (Printexc.exn_slot_name exn)
        !filename !lineno

(** [get_channel ()] returns the current test data channel, raising [Failure] if
    not in a test context. *)
let get_channel () =
  match !current_channel with
  | None ->
      failwith
        "Not in a test context - must be called from within a test function"
  | Some ch -> ch

(** [generate_from_schema schema] generates a value from a schema by sending a
    generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest. *)
let generate_from_schema schema =
  let channel = get_channel () in
  try
    pending_get
      (request channel
         (`Map [ (`Text "command", `Text "generate"); (`Text "schema", schema) ]))
  with Request_error e when e.error_type = "StopTest" ->
    test_aborted := true;
    raise Data_exhausted

(** [assume condition] rejects the current test case if [condition] is [false].
*)
let assume condition = if not condition then raise Assume_rejected

(** [note message] records a message that will be printed on the final (failing)
    run. *)
let note message = if !is_final_run then Printf.eprintf "%s\n%!" message

(** [target value label] sends a target command to guide the search engine
    toward higher values. *)
let target value label =
  let channel = get_channel () in
  ignore
    (pending_get
       (request channel
          (`Map
             [
               (`Text "command", `Text "target");
               (`Text "value", `Float value);
               (`Text "label", `Text label);
             ])))

(** [start_span ?label ()] starts a generation span for better shrinking. *)
let start_span ?(label = 0) () =
  if !test_aborted then ()
  else begin
    let channel = get_channel () in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "start_span");
                 (`Text "label", `Int label);
               ])))
  end

(** [stop_span ?discard ()] ends the current generation span. *)
let stop_span ?(discard = false) () =
  if !test_aborted then ()
  else begin
    let channel = get_channel () in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "stop_span");
                 (`Text "discard", `Bool discard);
               ])))
  end

type client = { connection : connection; control : channel; lock : Mutex.t }
(** Hegel client for running property-based tests. *)

(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)
let create_client connection =
  let server_version = float_of_string (send_handshake connection) in
  if server_version < 0.1 || server_version > 0.1 then
    failwith
      (Printf.sprintf
         "hegel-ocaml supports protocol versions 0.1 through 0.1, but got \
          server version %g. Upgrading hegel-ocaml or downgrading your hegel \
          cli might help."
         server_version);
  { connection; control = connection.control_channel; lock = Mutex.create () }

(** [run_test_case client channel test_fn ~is_final] runs a single test case.
    Sets up thread-local state and calls [test_fn]. Reports status via
    mark_complete. *)
let run_test_case _client channel test_fn ~is_final =
  if !current_channel <> None then
    failwith "Cannot nest test cases - already inside a test case";
  current_channel := Some channel;
  is_final_run := is_final;
  test_aborted := false;
  in_test := true;
  let already_complete = ref false in
  let status = ref "VALID" in
  let origin = ref `Null in
  let final_exn = ref None in
  (try test_fn () with
  | Assume_rejected -> status := "INVALID"
  | Data_exhausted -> already_complete := true
  | exn ->
      status := "INTERESTING";
      origin := `Text (extract_origin exn);
      if is_final then final_exn := Some exn);
  current_channel := None;
  is_final_run := false;
  test_aborted := false;
  in_test := false;
  (if not !already_complete then
     try
       ignore
         (pending_get
            (request channel
               (`Map
                  [
                    (`Text "command", `Text "mark_complete");
                    (`Text "status", `Text !status);
                    (`Text "origin", !origin);
                  ])))
     with Request_error e when e.error_type = "StopTest" -> ());
  close_channel channel;
  match !final_exn with Some e -> raise e | None -> ()

(** [run_test client ~name ~test_cases test_fn] runs a property test. *)
let run_test client ~name ~test_cases test_fn =
  if !in_test then
    failwith "Cannot nest test cases - already inside a test case";
  let test_channel = new_channel client.connection ~role:"Test" () in
  Mutex.lock client.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock client.lock)
    (fun () ->
      ignore
        (pending_get
           (request client.control
              (`Map
                 [
                   (`Text "command", `Text "run_test");
                   (`Text "name", `Text name);
                   (`Text "test_cases", `Int test_cases);
                   (`Text "channel", `Int (Int32.to_int test_channel.channel_id));
                 ]))));
  let result_data = ref None in
  let continue = ref true in
  while !continue do
    let message_id, message = receive_request test_channel () in
    let pairs = Cbor_helpers.extract_dict message in
    let event =
      match List.assoc_opt (`Text "event") pairs with
      | Some v -> Cbor_helpers.extract_string v
      | None -> ""
    in
    if event = "test_case" then begin
      let channel_id =
        Int32.of_int
          (Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
      in
      send_response_value test_channel message_id `Null;
      let test_case_channel =
        connect_channel client.connection channel_id ~role:"Test Case" ()
      in
      run_test_case client test_case_channel test_fn ~is_final:false
    end
    else if event = "test_done" then begin
      send_response_value test_channel message_id (`Bool true);
      result_data :=
        Some (Cbor_helpers.extract_dict (List.assoc (`Text "results") pairs));
      continue := false
    end
    else
      send_response_raw test_channel message_id
        (CBOR.Simple.encode
           (`Map
              [
                ( `Text "error",
                  `Text (Printf.sprintf "Unrecognised event %s" event) );
                (`Text "type", `Text "InvalidMessage");
              ]))
  done;
  let results = match !result_data with Some r -> r | None -> assert false in
  let n_interesting =
    Cbor_helpers.extract_int
      (List.assoc (`Text "interesting_test_cases") results)
  in
  if n_interesting = 0 then ()
  else if n_interesting = 1 then begin
    let message_id, message = receive_request test_channel () in
    let pairs = Cbor_helpers.extract_dict message in
    let channel_id =
      Int32.of_int
        (Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
    in
    send_response_value test_channel message_id `Null;
    let test_case_channel =
      connect_channel client.connection channel_id ~role:"Test Case" ()
    in
    run_test_case client test_case_channel test_fn ~is_final:true
  end
  else
    let exceptions = ref [] in
    for _i = 0 to n_interesting - 1 do
      try
        let message_id, message = receive_request test_channel () in
        let pairs = Cbor_helpers.extract_dict message in
        let channel_id =
          Int32.of_int
            (Cbor_helpers.extract_int (List.assoc (`Text "channel") pairs))
        in
        send_response_value test_channel message_id `Null;
        let test_case_channel =
          connect_channel client.connection channel_id ~role:"Test Case" ()
        in
        run_test_case client test_case_channel test_fn ~is_final:true;
        exceptions :=
          Failure
            (Printf.sprintf "Expected test case %d to fail but it didn't"
               (List.length !exceptions))
          :: !exceptions
      with e -> exceptions := e :: !exceptions
    done;
    let exns = List.rev !exceptions in
    raise
      (Failure
         (Printf.sprintf "Multiple failures (%d):\n%s" (List.length exns)
            (String.concat "\n"
               (List.mapi
                  (fun i e ->
                    Printf.sprintf "  %d: %s" i (Printexc.to_string e))
                  exns))))
