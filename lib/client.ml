(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. It manages:
    - Test lifecycle (run_test, test_case events, mark_complete)
    - Module-level state for the current data channel (single-threaded)
    - Helper functions (assume, note, target, generate_from_schema)
    - Origin extraction for error reporting *)

open! Core
module Mutex = Caml_threads.Mutex
open Connection

exception Assume_rejected
(** Raised when {!assume} condition is [false]. *)

exception Data_exhausted
(** Raised when the server runs out of test data (StopTest). *)

(** Health checks that can be suppressed during test execution. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(** [health_check_to_string hc] returns the wire protocol name for [hc]. *)
let health_check_to_string = function
  | Filter_too_much -> "filter_too_much"
  | Too_slow -> "too_slow"
  | Test_cases_too_large -> "test_cases_too_large"
  | Large_initial_test_case -> "large_initial_test_case"

(** Controls how much output Hegel produces during test runs. *)
type verbosity = Quiet | Normal | Verbose | Debug

(** The database setting: unset, disabled, or a path. *)
type database = Unset | Disabled | Path of string

type settings = {
  test_cases : int;
  verbosity : verbosity;
  seed : int option;
  derandomize : bool;
  database : database;
  suppress_health_check : health_check list;
}
(** Configuration for a Hegel test run. *)

(** CI environment variables to check for auto-detection. Each entry is
    [(var_name, expected_value)] where [None] means "any value". *)
let ci_vars =
  [
    ("CI", None);
    ("TF_BUILD", Some "true");
    ("BUILDKITE", Some "true");
    ("CIRCLECI", Some "true");
    ("CIRRUS_CI", Some "true");
    ("CODEBUILD_BUILD_ID", None);
    ("GITHUB_ACTIONS", Some "true");
    ("GITLAB_CI", None);
    ("HEROKU_TEST_RUN_ID", None);
    ("TEAMCITY_VERSION", None);
  ]

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
let is_in_ci () =
  List.exists ci_vars ~f:(fun (key, expected) ->
      match (Sys.getenv key, expected) with
      | Some _, None -> true
      | Some v, Some exp -> String.equal v exp
      | None, _ -> false)

(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically. *)
let default_settings () =
  let in_ci = is_in_ci () in
  {
    test_cases = 100;
    verbosity = Normal;
    seed = None;
    derandomize = in_ci;
    database = (if in_ci then Disabled else Unset);
    suppress_health_check = [];
  }

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. *)
let settings ?(test_cases = 100) ?seed () =
  let s = default_settings () in
  let s = { s with test_cases } in
  match seed with Some v -> { s with seed = Some v } | None -> s

(** [with_test_cases n s] returns settings [s] with [test_cases] set to [n]. *)
let with_test_cases n s = { s with test_cases = n }

(** [with_verbosity v s] returns settings [s] with [verbosity] set to [v]. *)
let with_verbosity v s = { s with verbosity = v }

(** [with_seed seed s] returns settings [s] with [seed] set. *)
let with_seed seed s = { s with seed }

(** [with_derandomize b s] returns settings [s] with [derandomize] set to [b].
*)
let with_derandomize b s = { s with derandomize = b }

(** [with_database db s] returns settings [s] with [database] set to [db]. *)
let with_database db s = { s with database = db }

(** [with_suppress_health_check checks s] returns settings [s] with additional
    health checks suppressed. *)
let with_suppress_health_check checks s =
  { s with suppress_health_check = s.suppress_health_check @ checks }

type test_case = {
  channel : channel;
  is_final : bool;
  mutable test_aborted : bool;
}
(** Per-test-case state passed explicitly to the test function. Holds the data
    channel, final-run flag, and abort state. *)

(** Domain-local flag to detect nested test cases. *)
let in_test_context : bool Stdlib.Domain.DLS.key =
  Stdlib.Domain.DLS.new_key (fun () -> false)

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)
let extract_origin exn =
  let bt = Stdlib.Printexc.get_raw_backtrace () in
  let file_line =
    match Stdlib.Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
        Array.fold slots ~init:None ~f:(fun acc slot ->
            Option.value_map (Stdlib.Printexc.Slot.location slot) ~default:acc
              ~f:(fun loc -> Some loc))
        |> Option.map ~f:(fun (loc : Stdlib.Printexc.location) ->
            (loc.filename, loc.line_number))
  in
  match file_line with
  | None -> sprintf "%s at :0" (Stdlib.Printexc.exn_slot_name exn)
  | Some (file, line) ->
      sprintf "%s at %s:%d" (Stdlib.Printexc.exn_slot_name exn) file line

(** [generate_from_schema schema tc] generates a value from a schema by sending
    a generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest. *)
let generate_from_schema schema tc =
  let channel = tc.channel in
  try
    pending_get
      (request channel
         (`Map [ (`Text "command", `Text "generate"); (`Text "schema", schema) ]))
  with Request_error e when String.equal e.error_type "StopTest" ->
    tc.test_aborted <- true;
    raise Data_exhausted

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume _tc condition = if not condition then raise Assume_rejected

(** [note tc message] records a message that will be printed on the final
    (failing) run. *)
let note tc message = if tc.is_final then eprintf "%s\n%!" message

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
let target tc value label =
  let channel = tc.channel in
  ignore
    (pending_get
       (request channel
          (`Map
             [
               (`Text "command", `Text "target");
               (`Text "value", `Float value);
               (`Text "label", `Text label);
             ])))

(** [start_span ?label tc] starts a generation span for better shrinking. *)
let start_span ?(label = 0) tc =
  if tc.test_aborted then ()
  else begin
    let channel = tc.channel in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "start_span");
                 (`Text "label", `Int label);
               ])))
  end

(** [stop_span ?discard tc] ends the current generation span. *)
let stop_span ?(discard = false) tc =
  if tc.test_aborted then ()
  else begin
    let channel = tc.channel in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "stop_span");
                 (`Text "discard", `Bool discard);
               ])))
  end

(** Supported protocol version range. *)
let supported_protocol_lo = 0.1

let supported_protocol_hi = 0.7

type client = { connection : connection; control : channel; lock : Mutex.t }
(** Hegel client for running property-based tests. *)

(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)
let create_client connection =
  let server_version = Float.of_string (send_handshake connection) in
  if
    Float.( < ) server_version supported_protocol_lo
    || Float.( > ) server_version supported_protocol_hi
  then
    failwith
      (sprintf
         "hegel-ocaml supports protocol versions %.1f through %.1f, but the \
          connected server is using protocol version %g. Upgrading hegel-ocaml \
          or downgrading hegel-core might help."
         supported_protocol_lo supported_protocol_hi server_version);
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
  let tc = { channel; is_final; test_aborted = false } in
  Stdlib.Domain.DLS.set in_test_context true;
  let outcome =
    try
      test_fn tc;
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
  Stdlib.Domain.DLS.set in_test_context false;
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
      with Request_error e when String.equal e.error_type "StopTest" -> ()));
  close_channel channel;
  match outcome with Interesting { exn = Some e; _ } -> raise e | _ -> ()

(** [run_test client ~settings ?database_key test_fn] runs a property test using
    the given settings.

    @param database_key
      optional key for persistent failure storage (internal use). *)
let run_test client ~settings ?database_key test_fn =
  if Stdlib.Domain.DLS.get in_test_context then
    failwith "Cannot nest test cases - already inside a test case";
  let test_channel = new_channel client.connection ~role:"Test" () in
  Mutex.lock client.lock;
  Exn.protect
    ~finally:(fun () -> Mutex.unlock client.lock)
    ~f:(fun () ->
      let seed_value =
        match settings.seed with Some s -> `Int s | None -> `Null
      in
      let database_key_value =
        match database_key with Some k -> `Bytes k | None -> `Null
      in
      let base_fields =
        [
          (`Text "command", `Text "run_test");
          (`Text "test_cases", `Int settings.test_cases);
          (`Text "seed", seed_value);
          (`Text "channel_id", `Int (Int32.to_int_exn (channel_id test_channel)));
          (`Text "database_key", database_key_value);
          (`Text "derandomize", `Bool settings.derandomize);
        ]
      in
      let database_field =
        match settings.database with
        | Unset -> []
        | Disabled -> [ (`Text "database", `Null) ]
        | Path p -> [ (`Text "database", `Text p) ]
      in
      let suppress_field =
        match settings.suppress_health_check with
        | [] -> []
        | checks ->
            [
              ( `Text "suppress_health_check",
                `Array
                  (List.map checks ~f:(fun hc ->
                       `Text (health_check_to_string hc))) );
            ]
      in
      let fields = base_fields @ database_field @ suppress_field in
      ignore (pending_get (request client.control (`Map fields))));
  let receive_and_run_test_case ~is_final =
    let message_id, message = receive_request test_channel () in
    let pairs = Cbor_helpers.extract_dict message in
    let ch_id =
      Int32.of_int_exn
        (Cbor_helpers.extract_int
           (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "channel_id")))
    in
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
      Cbor_helpers.extract_string
        (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "event"))
    in
    if String.equal event "test_case" then begin
      let ch_id =
        Int32.of_int_exn
          (Cbor_helpers.extract_int
             (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "channel_id")))
      in
      send_response_value test_channel message_id `Null;
      let test_case_channel =
        connect_channel client.connection ch_id ~role:"Test Case" ()
      in
      run_test_case client test_case_channel test_fn ~is_final:false;
      if server_has_exited client.connection then
        failwith server_crashed_message;
      receive_events ()
    end
    else if String.equal event "test_done" then begin
      send_response_value test_channel message_id (`Bool true);
      Cbor_helpers.extract_dict
        (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "results"))
    end
    else begin
      send_response_raw test_channel message_id
        (CBOR.Simple.encode
           (`Map
              [
                (`Text "error", `Text (sprintf "Unrecognised event %s" event));
                (`Text "type", `Text "InvalidMessage");
              ]));
      receive_events ()
    end
  in
  let results = receive_events () in
  (* Check for server-side errors *)
  (match List.Assoc.find results ~equal:Poly.( = ) (`Text "error") with
  | Some error_val ->
      let error_msg = Cbor_helpers.extract_string error_val in
      failwith (sprintf "Server error: %s" error_msg)
  | None -> ());
  (* Check for health check failure *)
  (match
     List.Assoc.find results ~equal:Poly.( = ) (`Text "health_check_failure")
   with
  | Some failure_val ->
      let failure_msg = Cbor_helpers.extract_string failure_val in
      failwith (sprintf "Health check failure:\n%s" failure_msg)
  | None -> ());
  (* Check for flaky test detection *)
  (match List.Assoc.find results ~equal:Poly.( = ) (`Text "flaky") with
  | Some flaky_val ->
      let flaky_msg = Cbor_helpers.extract_string flaky_val in
      failwith (sprintf "Flaky test detected: %s" flaky_msg)
  | None -> ());
  (* Check passed flag *)
  let passed =
    match List.Assoc.find results ~equal:Poly.( = ) (`Text "passed") with
    | Some (`Bool b) -> b
    | _ -> true
  in
  let n_interesting =
    Cbor_helpers.extract_int
      (List.Assoc.find_exn results ~equal:Poly.( = )
         (`Text "interesting_test_cases"))
  in
  if n_interesting = 0 && passed then ()
  else if n_interesting <= 1 then begin
    if n_interesting = 1 then receive_and_run_test_case ~is_final:true;
    if not passed then failwith "Property test failed"
  end
  else
    let rec replay_interesting remaining acc =
      if remaining = 0 then List.rev acc
      else
        let msg =
          sprintf "Expected test case %d to fail but it didn't"
            (List.length acc)
        in
        let exn =
          try
            receive_and_run_test_case ~is_final:true;
            Failure msg
          with e -> e
        in
        replay_interesting (remaining - 1) (exn :: acc)
    in
    let exns = replay_interesting n_interesting [] in
    raise
      (Failure
         (sprintf "Multiple failures (%d):\n%s" (List.length exns)
            (String.concat ~sep:"\n"
               (List.mapi exns ~f:(fun i e ->
                    sprintf "  %d: %s" i (Exn.to_string e))))))
