(** Test runner and lifecycle management for Hegel.

    This module implements the client-side logic for running property-based
    tests against a Hegel server. It manages:
    - Test lifecycle (run_test, test_case events, mark_complete)
    - Module-level state for the current data stream (single-threaded)
    - Helper functions (assume, note, target, generate_from_schema)
    - Origin extraction for error reporting *)

open! Core
module Mutex = Caml_threads.Mutex
open Connection

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the server runs out of test data (StopTest). *)
exception Data_exhausted

(** Raised when the server detects a flaky strategy definition. *)
exception Flaky_strategy

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
;;

(** Controls how much output Hegel produces during test runs. *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** The database setting: unset, disabled, or a path. *)
type database =
  | Unset
  | Disabled
  | Path of string

(** Controls the test execution mode. *)
type mode =
  | Test_run
  (** Run a full property test: many test cases, shrinking, database
        replay, all other phases. This is the default. *)
  | Single_test_case
  (** Run the test body exactly once, with no shrinking, replay, or
        database. Useful when you want pure data generation without
        property-testing overhead. *)

(** Phases of the test lifecycle. Mirrors [hypothesis.Phase]. *)
type phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

(** [phase_to_string p] returns the lowercase wire-protocol name for [p]. *)
let phase_to_string = function
  | Explicit -> "explicit"
  | Reuse -> "reuse"
  | Generate -> "generate"
  | Target -> "target"
  | Shrink -> "shrink"
;;

(** Configuration for a Hegel test run. *)
type settings =
  { mode : mode
  ; test_cases : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
  ; suppress_health_check : health_check list
  ; phases : phase list option
  }

(** CI environment variables to check for auto-detection. Each entry is
    [(var_name, expected_value)] where [None] means "any value". *)
let ci_vars =
  [ "CI", None
  ; "TF_BUILD", Some "true"
  ; "BUILDKITE", Some "true"
  ; "CIRCLECI", Some "true"
  ; "CIRRUS_CI", Some "true"
  ; "CODEBUILD_BUILD_ID", None
  ; "GITHUB_ACTIONS", Some "true"
  ; "GITLAB_CI", None
  ; "HEROKU_TEST_RUN_ID", None
  ; "TEAMCITY_VERSION", None
  ]
;;

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
let is_in_ci () =
  List.exists ci_vars ~f:(fun (key, expected) ->
    match Sys.getenv key, expected with
    | Some _, None -> true
    | Some v, Some exp -> String.equal v exp
    | None, _ -> false)
;;

(** [default_settings ()] creates settings with defaults. Detects CI
    environments automatically. *)
let default_settings () =
  let in_ci = is_in_ci () in
  { mode = Test_run
  ; test_cases = 100
  ; verbosity = Normal
  ; seed = None
  ; derandomize = in_ci
  ; database = (if in_ci then Disabled else Unset)
  ; suppress_health_check = []
  ; phases = None
  }
;;

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. *)
let settings ?(test_cases = 100) ?seed () =
  let s = default_settings () in
  let s = { s with test_cases } in
  match seed with
  | Some v -> { s with seed = Some v }
  | None -> s
;;

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
;;

(** [with_phases phases s] returns settings [s] with [phases] set. *)
let with_phases phases s = { s with phases = Some phases }

(** [with_mode mode s] returns settings [s] with test [mode] set to [mode]. *)
let with_mode mode s = { s with mode }

(** Per-test-case state passed explicitly to the test function. Holds the data
    stream, final-run flag, and abort state. *)
type test_case =
  { stream : stream
  ; mode : mode
  ; is_final : bool
  ; mutable test_aborted : bool
  }

(** Domain-local flag to detect nested test cases. *)
let in_test_context : bool Stdlib.Domain.DLS.key =
  Stdlib.Domain.DLS.new_key (fun () -> false)
;;

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available. *)
let extract_origin exn =
  let bt = Stdlib.Printexc.get_raw_backtrace () in
  let file_line =
    match Stdlib.Printexc.backtrace_slots bt with
    | None -> None
    | Some slots ->
      Array.fold slots ~init:None ~f:(fun acc slot ->
        Option.value_map (Stdlib.Printexc.Slot.location slot) ~default:acc ~f:(fun loc ->
          Some loc))
      |> Option.map ~f:(fun (loc : Stdlib.Printexc.location) ->
        loc.filename, loc.line_number)
  in
  match file_line with
  | None -> sprintf "%s at :0" (Stdlib.Printexc.exn_slot_name exn)
  | Some (file, line) ->
    sprintf "%s at %s:%d" (Stdlib.Printexc.exn_slot_name exn) file line
;;

(** [generate_from_schema schema tc] generates a value from a schema by sending
    a generate command to the server. Raises {!Data_exhausted} if the server
    signals StopTest. *)
let generate_from_schema schema tc =
  let stream = tc.stream in
  try
    pending_get
      (request
         stream
         (`Map [ `Text "command", `Text "generate"; `Text "schema", schema ]))
  with
  | Request_error e when String.equal e.error_type "StopTest" ->
    tc.test_aborted <- true;
    raise Data_exhausted
  | Request_error e when String.equal e.error_type "FlakyStrategyDefinition" ->
    tc.test_aborted <- true;
    raise Flaky_strategy
;;

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume _tc condition = if not condition then raise Assume_rejected

(** [note tc message] records a message that will be printed on the final
    (failing) run. *)
let note tc message = if tc.is_final then eprintf "%s\n%!" message

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
let target tc value label =
  let stream = tc.stream in
  let (_ : Cbor.t) =
    pending_get
      (request
         stream
         (`Map
             [ `Text "command", `Text "target"
             ; `Text "value", `Float value
             ; `Text "label", `Text label
             ]))
  in
  ()
;;

(** [start_span ?label tc] starts a generation span for better shrinking. *)
let start_span ?(label = 0) tc =
  if tc.test_aborted
  then ()
  else (
    let stream = tc.stream in
    let (_ : Cbor.t) =
      pending_get
        (request
           stream
           (`Map [ `Text "command", `Text "start_span"; `Text "label", `Int label ]))
    in
    ())
;;

(** [stop_span ?discard tc] ends the current generation span. *)
let stop_span ?(discard = false) tc =
  if tc.test_aborted
  then ()
  else (
    let stream = tc.stream in
    let (_ : Cbor.t) =
      pending_get
        (request
           stream
           (`Map [ `Text "command", `Text "stop_span"; `Text "discard", `Bool discard ]))
    in
    ())
;;

(** Helper: send a pool-related request and translate StopTest into
    [Data_exhausted]. *)
let pool_request tc fields =
  let stream = tc.stream in
  try pending_get (request stream (`Map fields)) with
  | Request_error e when String.equal e.error_type "StopTest" ->
    tc.test_aborted <- true;
    raise Data_exhausted
;;

(** [new_pool tc] creates a new server-side variable pool and returns its id. *)
let new_pool tc =
  Cbor_helpers.extract_int (pool_request tc [ `Text "command", `Text "new_pool" ])
;;

(** [pool_add tc ~pool_id] adds a fresh variable to [pool_id] and returns the
    new variable id. *)
let pool_add tc ~pool_id =
  Cbor_helpers.extract_int
    (pool_request tc [ `Text "command", `Text "pool_add"; `Text "pool_id", `Int pool_id ])
;;

(** [pool_generate tc ~pool_id ?consume ()] draws a variable id from [pool_id].
    When [consume] is [true], the variable is also removed from the pool
    server-side. Drawing from an empty pool marks the test case invalid. *)
let pool_generate tc ~pool_id ?(consume = false) () =
  Cbor_helpers.extract_int
    (pool_request
       tc
       [ `Text "command", `Text "pool_generate"
       ; `Text "pool_id", `Int pool_id
       ; `Text "consume", `Bool consume
       ])
;;

(** Supported protocol version range. *)
let supported_protocol_lo = "0.15"

let supported_protocol_hi = "0.15"

(** Compare two "major.minor" version strings numerically. Returns a negative
    number if a < b, 0 if a = b, positive if a > b. *)
let compare_versions a b =
  let parse s =
    match String.split s ~on:'.' with
    | [ major; minor ] -> int_of_string major, int_of_string minor
    | _ ->
      failwith
        (Printf.sprintf "invalid version string '%s': expected 'major.minor' format" s)
  in
  let a_major, a_minor = parse a in
  let b_major, b_minor = parse b in
  let c = Int.compare a_major b_major in
  if c <> 0 then c else Int.compare a_minor b_minor
;;

(** Hegel client for running property-based tests. *)
type client =
  { connection : connection
  ; control : stream
  ; lock : Mutex.t
  }

(** [create_client connection] creates a new client from a connection. The
    connection must not yet have had its handshake performed. *)
let create_client connection =
  let server_version = send_handshake connection in
  if
    compare_versions server_version supported_protocol_lo < 0
    || compare_versions server_version supported_protocol_hi > 0
  then
    failwith
      (sprintf
         "hegel-ocaml supports protocol versions %s through %s, but the connected server \
          is using protocol version %s. Upgrading hegel-ocaml or downgrading hegel-core \
          might help."
         supported_protocol_lo
         supported_protocol_hi
         server_version);
  { connection; control = control_stream connection; lock = Mutex.create () }
;;

(** [run_test client ~settings ?database_key ?test_location test_fn] runs a
    property test using the given settings.

    @param database_key
      optional key for persistent failure storage (internal use).
    @param test_location
      source location of the test, used by the Antithesis integration.
      Provided automatically by the [let%hegel_test] PPX. *)
let run_test client ~(settings : settings) ?database_key ?test_location test_fn =
  if Stdlib.Domain.DLS.get in_test_context
  then failwith "Cannot nest test cases - already inside a test case";
  let emit_antithesis ~passed =
    match test_location with
    | Some loc when Antithesis.is_running_in_antithesis () ->
      Antithesis.emit_assertion loc ~passed
    | _ -> ()
  in
  let test_stream = new_stream client.connection ~role:"Test" () in
  (* Runs a single test case on [stream]. Sets up thread-local state, calls
     [test_fn], reports status via mark_complete, and returns the captured
     exception (when [is_final] and the test raised) or [None]. *)
  let run_test_case stream ~is_final =
    let tc = { stream; mode = settings.mode; is_final; test_aborted = false } in
    Stdlib.Domain.DLS.set in_test_context true;
    let outcome =
      try
        test_fn tc;
        `Valid
      with
      | Assume_rejected -> `Invalid
      | Data_exhausted -> `Data_exhausted
      | Flaky_strategy -> `Flaky_strategy
      | exn -> `Interesting (extract_origin exn, if is_final then Some exn else None)
    in
    Stdlib.Domain.DLS.set in_test_context false;
    (match outcome with
     | `Data_exhausted | `Flaky_strategy -> ()
     | (`Valid | `Invalid | `Interesting _) as o ->
       let status, origin =
         match o with
         | `Valid -> "VALID", `Null
         | `Invalid -> "INVALID", `Null
         | `Interesting (origin_text, _) -> "INTERESTING", `Text origin_text
       in
       (try
          let (_ : Cbor.t) =
            pending_get
              (request
                 stream
                 (`Map
                     [ `Text "command", `Text "mark_complete"
                     ; `Text "status", `Text status
                     ; `Text "origin", origin
                     ]))
          in
          ()
        with
        | Request_error e when String.equal e.error_type "StopTest" -> ()));
    close_stream stream;
    match outcome with
    | `Interesting (_, Some e) -> Some e
    | _ -> None
  in
  (* Extract the data-channel id from a [test_case] event payload and connect
     to it, returning the per-test-case stream. *)
  let connect_test_case_stream pairs =
    let ch_id =
      Int32.of_int_exn
        (Cbor_helpers.extract_int
           (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "stream_id")))
    in
    connect_stream client.connection ch_id ~role:"Test Case" ()
  in
  (* Drive the event loop on [test_stream] until [test_done]. *)
  let receive_events ~on_test_case =
    let rec loop () =
      let message_id, message = receive_request test_stream () in
      let pairs = Cbor_helpers.extract_dict message in
      let event =
        Cbor_helpers.extract_string
          (List.Assoc.find_exn pairs ~equal:Poly.( = ) (`Text "event"))
      in
      if String.equal event "test_case"
      then (
        let test_case_stream = connect_test_case_stream pairs in
        send_response_value test_stream message_id `Null;
        on_test_case test_case_stream;
        loop ())
      else if String.equal event "test_done"
      then (
        send_response_value test_stream message_id (`Bool true);
        match List.Assoc.find pairs ~equal:Poly.( = ) (`Text "results") with
        | Some r -> Cbor_helpers.extract_dict r
        | None -> [])
      else (
        send_response_raw
          test_stream
          message_id
          (Cbor.encode
             (`Map
                 [ `Text "error", `Text (sprintf "Unrecognised event %s" event)
                 ; `Text "type", `Text "InvalidMessage"
                 ]));
        loop ())
    in
    loop ()
  in
  let send_run_command msg =
    Mutex.lock client.lock;
    Exn.protect
      ~finally:(fun () -> Mutex.unlock client.lock)
      ~f:(fun () ->
        let (_ : Cbor.t) = pending_get (request client.control msg) in
        ())
  in
  let seed_value =
    match settings.seed with
    | Some s -> `Int s
    | None -> `Null
  in
  let stream_id_field =
    `Text "stream_id", `Int (Int32.to_int_exn (stream_id test_stream))
  in
  let check_server_alive () =
    if server_has_exited client.connection then failwith server_crashed_message
  in
  let run_test_body () =
    match settings.mode with
    | Single_test_case ->
      send_run_command
        (`Map
            [ `Text "command", `Text "single_test_case"
            ; stream_id_field
            ; `Text "seed", seed_value
            ]);
      let failures = ref [] in
      let on_test_case stream =
        (match run_test_case stream ~is_final:true with
         | Some e -> failures := e :: !failures
         | None -> ());
        check_server_alive ()
      in
      let (_ : (Cbor.t * Cbor.t) list) = receive_events ~on_test_case in
      (match List.rev !failures with
       | [] -> ()
       | [ e ] -> raise e
       | exns ->
         raise
           (Failure
              (sprintf
                 "Multiple failures (%d):\n%s"
                 (List.length exns)
                 (String.concat
                    ~sep:"\n"
                    (List.mapi exns ~f:(fun i e -> sprintf "  %d: %s" i (Exn.to_string e)))))))
    | Test_run ->
      let database_key_value =
        match database_key with
        | Some k -> `Bytes k
        | None -> `Null
      in
      let base_fields =
        [ `Text "command", `Text "run_test"
        ; `Text "test_cases", `Int settings.test_cases
        ; `Text "seed", seed_value
        ; stream_id_field
        ; `Text "database_key", database_key_value
        ; `Text "derandomize", `Bool settings.derandomize
        ]
      in
      let database_field =
        match settings.database with
        | Unset -> []
        | Disabled -> [ `Text "database", `Null ]
        | Path p -> [ `Text "database", `Text p ]
      in
      let suppress_field =
        match settings.suppress_health_check with
        | [] -> []
        | checks ->
          [ ( `Text "suppress_health_check"
            , `Array (List.map checks ~f:(fun hc -> `Text (health_check_to_string hc))) )
          ]
      in
      let phases_field =
        match settings.phases with
        | None -> []
        | Some phases ->
          [ ( `Text "phases"
            , `Array (List.map phases ~f:(fun p -> `Text (phase_to_string p))) )
          ]
      in
      send_run_command
        (`Map (base_fields @ database_field @ suppress_field @ phases_field));
      let on_test_case stream =
        let (_ : exn option) = run_test_case stream ~is_final:false in
        check_server_alive ()
      in
      let results = receive_events ~on_test_case in
      (* Check for server-side errors *)
      (match List.Assoc.find results ~equal:Poly.( = ) (`Text "error") with
       | Some error_val ->
         let error_msg = Cbor_helpers.extract_string error_val in
         failwith (sprintf "Server error: %s" error_msg)
       | None -> ());
      (* Check for health check failure *)
      (match List.Assoc.find results ~equal:Poly.( = ) (`Text "health_check_failure") with
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
          (List.Assoc.find_exn results ~equal:Poly.( = ) (`Text "interesting_test_cases"))
      in
      (* Receive a final-replay [test_case] event on [test_stream] and run it. *)
      let replay_test_case () =
        let message_id, message = receive_request test_stream () in
        let pairs = Cbor_helpers.extract_dict message in
        let test_case_stream = connect_test_case_stream pairs in
        send_response_value test_stream message_id `Null;
        run_test_case test_case_stream ~is_final:true
      in
      if n_interesting = 0 && passed
      then ()
      else if n_interesting <= 1
      then (
        if n_interesting = 1 then Option.iter (replay_test_case ()) ~f:raise;
        if not passed then failwith "Property test failed")
      else (
        let rec replay_interesting remaining acc =
          if remaining = 0
          then List.rev acc
          else (
            let exn =
              match replay_test_case () with
              | Some e | (exception e) -> e
              | None ->
                Failure
                  (sprintf
                     "Expected test case %d to fail but it didn't"
                     (List.length acc))
            in
            replay_interesting (remaining - 1) (exn :: acc))
        in
        let exns = replay_interesting n_interesting [] in
        raise
          (Failure
             (sprintf
                "Multiple failures (%d):\n%s"
                (List.length exns)
                (String.concat
                   ~sep:"\n"
                   (List.mapi exns ~f:(fun i e -> sprintf "  %d: %s" i (Exn.to_string e)))))))
  in
  match run_test_body () with
  | () -> emit_antithesis ~passed:true
  | exception e ->
    emit_antithesis ~passed:false;
    raise e
;;
