(** Test runner and lifecycle management for the Hegel SDK.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. It manages:
    - Test lifecycle (run_test, test_case events, mark_complete)
    - Module-level state for the current data channel (single-threaded)
    - Helper functions (assume, note, target, generate_from_schema)
    - Origin extraction for error reporting *)

open Connection

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

type test_case_data = {
  channel : channel;
  is_final : bool;
  mutable test_aborted : bool;
}
(** Per-test-case data, consolidating channel, final-run flag, and abort state.
    Domain-local so each domain has its own independent test state. *)

let current_data : test_case_data option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

(** [get_data ()] returns the current test case data, raising [Failure] if not
    in a test context. *)
let get_data () =
  match Domain.DLS.get current_data with
  | None -> failwith "Not in a test context"
  | Some d -> d

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)
let extract_origin exn =
  let bt = Printexc.get_raw_backtrace () in
  let file_line =
    match Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
        Array.fold_left
          (fun acc slot ->
            Option.fold ~none:acc
              ~some:(fun loc -> Some loc)
              (Printexc.Slot.location slot))
          None slots
        |> Option.map (fun (loc : Printexc.location) ->
            (loc.filename, loc.line_number))
  in
  match file_line with
  | None -> Printf.sprintf "%s at :0" (Printexc.exn_slot_name exn)
  | Some (file, line) ->
      Printf.sprintf "%s at %s:%d" (Printexc.exn_slot_name exn) file line

(** [generate_from_schema schema data] generates a value from a schema by
    sending a generate command to the server. Raises {!Data_exhausted} if the
    server signals StopTest. *)
let generate_from_schema schema data =
  let channel = data.channel in
  try
    pending_get
      (request channel
         (`Map [ (`Text "command", `Text "generate"); (`Text "schema", schema) ]))
  with Request_error e when e.error_type = "StopTest" ->
    data.test_aborted <- true;
    raise Data_exhausted

(** [assume condition] rejects the current test case if [condition] is [false].
*)
let assume condition =
  if Domain.DLS.get current_data = None then
    failwith "assume() cannot be called outside of a Hegel test";
  if not condition then raise Assume_rejected

(** [note message] records a message that will be printed on the final (failing)
    run. *)
let note message =
  match Domain.DLS.get current_data with
  | None -> failwith "note() cannot be called outside of a Hegel test"
  | Some data -> if data.is_final then Printf.eprintf "%s\n%!" message

(** [target value label] sends a target command to guide the search engine
    toward higher values. *)
let target value label =
  let data = get_data () in
  let channel = data.channel in
  ignore
    (pending_get
       (request channel
          (`Map
             [
               (`Text "command", `Text "target");
               (`Text "value", `Float value);
               (`Text "label", `Text label);
             ])))

(** [start_span ?label data] starts a generation span for better shrinking. *)
let start_span ?(label = 0) data =
  if data.test_aborted then ()
  else begin
    let channel = data.channel in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "start_span");
                 (`Text "label", `Int label);
               ])))
  end

(** [stop_span ?discard data] ends the current generation span. *)
let stop_span ?(discard = false) data =
  if data.test_aborted then ()
  else begin
    let channel = data.channel in
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
  if server_version < 0.1 || server_version > 0.3 then
    failwith
      (Printf.sprintf
         "hegel-ocaml supports protocol versions 0.1 through 0.3, but got \
          server version %g. Upgrading hegel-ocaml or downgrading your hegel \
          cli might help."
         server_version);
  { connection; control = control_channel connection; lock = Mutex.create () }

(** [run_test_case client channel test_fn ~is_final] runs a single test case.
    Sets up thread-local state and calls [test_fn]. Reports status via
    mark_complete. *)
type test_outcome =
  | Valid
  | Invalid
  | Data_was_exhausted
  | Interesting of { origin_text : string; exn : exn option }

let run_test_case _client channel test_fn ~is_final =
  if Domain.DLS.get current_data <> None then
    failwith "Cannot nest test cases - already inside a test case";
  let data = { channel; is_final; test_aborted = false } in
  Domain.DLS.set current_data (Some data);
  let outcome =
    try
      test_fn ();
      Valid
    with
    | Assume_rejected -> Invalid
    | Data_exhausted -> Data_was_exhausted
    | exn ->
        Interesting
          {
            origin_text = extract_origin exn;
            exn = (if is_final then Some exn else None);
          }
  in
  Domain.DLS.set current_data None;
  (match outcome with
  | Data_was_exhausted -> ()
  | Valid | Invalid | Interesting _ -> (
      let status, origin =
        match outcome with
        | Valid -> ("VALID", `Null)
        | Invalid -> ("INVALID", `Null)
        | Interesting { origin_text; _ } -> ("INTERESTING", `Text origin_text)
        | Data_was_exhausted -> assert false
      in
      try
        ignore
          (pending_get
             (request channel
                (`Map
                   [
                     (`Text "command", `Text "mark_complete");
                     (`Text "status", `Text status);
                     (`Text "origin", origin);
                   ])))
      with Request_error e when e.error_type = "StopTest" -> ()));
  close_channel channel;
  match outcome with Interesting { exn = Some e; _ } -> raise e | _ -> ()

(** [run_test client ~name ~test_cases ?seed test_fn] runs a property test.

    @param seed
      optional seed for deterministic replay. If [None], the server generates
      its own seed. *)
let run_test client ~name ~test_cases ?seed test_fn =
  if Domain.DLS.get current_data <> None then
    failwith "Cannot nest test cases - already inside a test case";
  let test_channel = new_channel client.connection ~role:"Test" () in
  Mutex.lock client.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock client.lock)
    (fun () ->
      let seed_value = match seed with Some s -> `Int s | None -> `Null in
      ignore
        (pending_get
           (request client.control
              (`Map
                 [
                   (`Text "command", `Text "run_test");
                   (`Text "name", `Text name);
                   (`Text "test_cases", `Int test_cases);
                   (`Text "seed", seed_value);
                   ( `Text "channel_id",
                     `Int (Int32.to_int (channel_id test_channel)) );
                 ]))));
  let extract_channel_id pairs context =
    match List.assoc_opt (`Text "channel_id") pairs with
    | Some v -> Int32.of_int (Cbor_helpers.extract_int v)
    | None -> failwith (context ^ " missing 'channel_id' field")
  in
  let receive_and_run_test_case ~is_final =
    let message_id, message = receive_request test_channel () in
    let pairs = Cbor_helpers.extract_dict message in
    let ch_id = extract_channel_id pairs "test_case event" in
    send_response_value test_channel message_id `Null;
    let test_case_channel =
      connect_channel client.connection ch_id ~role:"Test Case" ()
    in
    run_test_case client test_case_channel test_fn ~is_final
  in
  let rec receive_events () =
    let message_id, message = receive_request test_channel () in
    let pairs = Cbor_helpers.extract_dict message in
    let event =
      match List.assoc_opt (`Text "event") pairs with
      | Some v -> Cbor_helpers.extract_string v
      | None -> ""
    in
    if event = "test_case" then begin
      let ch_id = extract_channel_id pairs "test_case event" in
      send_response_value test_channel message_id `Null;
      let test_case_channel =
        connect_channel client.connection ch_id ~role:"Test Case" ()
      in
      run_test_case client test_case_channel test_fn ~is_final:false;
      receive_events ()
    end
    else if event = "test_done" then begin
      send_response_value test_channel message_id (`Bool true);
      match List.assoc_opt (`Text "results") pairs with
      | Some v -> Cbor_helpers.extract_dict v
      | None -> failwith "test_done event missing 'results' field"
    end
    else begin
      send_response_raw test_channel message_id
        (CBOR.Simple.encode
           (`Map
              [
                ( `Text "error",
                  `Text (Printf.sprintf "Unrecognised event %s" event) );
                (`Text "type", `Text "InvalidMessage");
              ]));
      receive_events ()
    end
  in
  let results = receive_events () in
  let n_interesting =
    match List.assoc_opt (`Text "interesting_test_cases") results with
    | Some v -> Cbor_helpers.extract_int v
    | None ->
        failwith "test_done results missing 'interesting_test_cases' field"
  in
  if n_interesting = 0 then ()
  else if n_interesting = 1 then receive_and_run_test_case ~is_final:true
  else
    let rec replay_interesting remaining acc =
      if remaining = 0 then List.rev acc
      else
        let result =
          try
            receive_and_run_test_case ~is_final:true;
            Error
              (Failure
                 (Printf.sprintf "Expected test case %d to fail but it didn't"
                    (List.length acc)))
          with e -> Error e
        in
        let exn = match result with Error e -> e | Ok () -> assert false in
        replay_interesting (remaining - 1) (exn :: acc)
    in
    let exns = replay_interesting n_interesting [] in
    raise
      (Failure
         (Printf.sprintf "Multiple failures (%d):\n%s" (List.length exns)
            (String.concat "\n"
               (List.mapi
                  (fun i e ->
                    Printf.sprintf "  %d: %s" i (Printexc.to_string e))
                  exns))))
