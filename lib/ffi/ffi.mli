(* Low-level ctypes bindings to libhegel, the native property-based testing
   engine exposed as a C library by hegel-rust (hegel-c/include/hegel.h).

   This module is a thin, mechanical 1:1 wrapper over the C ABI: it locates and
   [dlopen]s the shared library, declares each exported function, and exposes
   OCaml-native wrappers that copy borrowed C buffers into OCaml strings and
   translate negative status codes into exceptions. *)

(** Opaque settings handle ([hegel_settings_t]). *)
type settings

(** Opaque in-flight run handle ([hegel_run_t]). *)
type run

(** Opaque per-test-case handle ([hegel_test_case_t]), borrowed from its
    parent {!run} and valid only until the case is completed. *)
type test_case

(** Opaque aggregated run result ([hegel_run_result_t]), borrowed from its
    parent {!run} and valid until {!run_free}. *)
type run_result

(** Opaque single-failure handle ([hegel_failure_t]), borrowed from its
    parent {!type:run_result}. *)
type failure

(** Test execution mode ([hegel_mode_t]). *)
type mode =
  | Test_run
  | Single_test_case

(** Engine output verbosity ([hegel_verbosity_t]). *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** Per-test-case outcome ([hegel_status_t]) passed to {!mark_complete}. *)
type status =
  | Valid
  | Invalid
  | Overrun
  | Interesting

(** Raised when a primitive returns [HEGEL_E_STOP_TEST] — the engine has
    exhausted its choice budget for the current test case. *)
exception Stop_test

(** Raised when a primitive returns [HEGEL_E_ASSUME] — the engine rejected the
    current test case as invalid (e.g. an impossible uniqueness constraint that
    exceeds the collection reject limit). Carries no diagnostic. *)
exception Assume_rejected

(** Raised when a libhegel call fails with any other negative status code; the
    payload is {!last_error_message}. *)
exception Backend_error of string

(** {2 Phase bitmask values}

    The [HEGEL_PHASE_*] constants. *)

val phase_explicit : int
val phase_reuse : int
val phase_generate : int
val phase_target : int
val phase_shrink : int

(** {2 Health-check bitmask values}

    The [HEGEL_HC_*] constants. *)

val hc_filter_too_much : int
val hc_too_slow : int
val hc_test_cases_too_large : int
val hc_large_initial_test_case : int

(** {2 Diagnostics} *)

(** [version ()] returns libhegel's version string. *)
val version : unit -> string

(** [last_error_message ()] returns the most recent error on the calling
    thread, or the empty string if the last call succeeded. *)
val last_error_message : unit -> string

(** {2 Settings} *)

(** [settings_new ()] allocates a settings handle with libhegel's defaults.
    Must be released with {!settings_free}. *)
val settings_new : unit -> settings

(** [settings_free s] frees a settings handle. *)
val settings_free : settings -> unit

val settings_mode : settings -> mode -> unit
val settings_test_cases : settings -> int -> unit
val settings_verbosity : settings -> verbosity -> unit

(** [settings_seed s seed] sets the RNG seed ([None] picks a fresh random
    seed at run start). *)
val settings_seed : settings -> int option -> unit

val settings_derandomize : settings -> bool -> unit
val settings_report_multiple_failures : settings -> bool -> unit

(** [settings_database s db] configures the on-disk example database:
    [None] leaves the default, [Some ""] disables it, [Some dir] uses [dir]. *)
val settings_database : settings -> string option -> unit

(** [settings_database_key s key] scopes stored/replayed examples; [None]
    clears it. *)
val settings_database_key : settings -> string option -> unit

(** [settings_phases s mask] enables exactly the phases in the bitmask. *)
val settings_phases : settings -> int -> unit

(** [settings_suppress_health_check s mask] disables the health checks in the
    bitmask. *)
val settings_suppress_health_check : settings -> int -> unit

(** {2 Run lifecycle} *)

(** [run_start s] starts a run with the given settings. Raises
    {!Backend_error} on failure. The handle must be freed with {!run_free}. *)
val run_start : settings -> run

(** [next_test_case run] blocks until the engine produces the next test case,
    or returns [None] when the run is finished. Raises {!Backend_error} on
    engine error or caller misuse. *)
val next_test_case : run -> test_case option

(** [test_case_from_blob settings blob] builds a standalone test case that
    replays the example encoded in a base64 failure [blob]. Raises
    {!Backend_error} (with the engine's diagnostic) when the blob is missing,
    not UTF-8, or cannot be decoded — the engine never returns a null handle
    without setting an error. The handle must be freed with
    {!blob_test_case_free}. *)
val test_case_from_blob : settings -> string option -> test_case

(** [run_result run] returns the aggregated result of a finished run. Raises
    {!Backend_error} if the run has not finished. *)
val run_result : run -> run_result

(** [run_free run] frees a run handle, draining the worker thread. *)
val run_free : run -> unit

(** [blob_test_case_free run] frees a test case created by a failure_blob *)
val blob_test_case_free : test_case -> unit

(** {2 Per-test-case primitives} *)

(** [generate tc schema] draws a value described by the CBOR-encoded [schema],
    returning the CBOR-encoded value bytes. Raises {!Stop_test} when the
    choice budget is exhausted, {!Backend_error} on a malformed schema. *)
val generate : test_case -> string -> string

val start_span : test_case -> int -> unit
val stop_span : test_case -> bool -> unit

(** [new_collection tc ~min_size ~max_size] starts an engine-managed
    collection ([max_size = None] means unbounded) and returns its id. *)
val new_collection : test_case -> min_size:int -> max_size:int option -> int

(** [collection_more tc id] returns whether the engine wants another element. *)
val collection_more : test_case -> int -> bool

(** [collection_reject tc id why] rejects the collection's last element. *)
val collection_reject : test_case -> int -> string option -> unit

(** [new_pool tc] creates a variable pool and returns its id. *)
val new_pool : test_case -> int

(** [pool_add tc ~pool_id] registers a fresh variable and returns its id. *)
val pool_add : test_case -> pool_id:int -> int

(** [pool_generate tc ~pool_id ~consume] draws (and optionally consumes) a
    variable id from the pool. Raises {!Stop_test} if the pool is empty. *)
val pool_generate : test_case -> pool_id:int -> consume:bool -> int

(** [target tc value label] records a targeting observation. *)
val target : test_case -> float -> string -> unit

(** [mark_complete tc status origin] reports the test case's outcome. [origin]
    is used only for {!Interesting} and must be stable per bug. *)
val mark_complete : test_case -> status -> string option -> unit

(** [is_final_replay tc] is [true] iff this is the engine's final replay of a
    minimal failing example. *)
val is_final_replay : test_case -> bool

(** {2 Result inspection} *)

val result_passed : run_result -> bool
val result_failure_count : run_result -> int

(** [result_failure r i] returns the [i]-th distinct failure, or [None] if out
    of range. *)
val result_failure : run_result -> int -> failure option

(** [result_failures r] returns all distinct failures in order. *)
val result_failures : run_result -> failure list

val failure_panic_message : failure -> string option
val failure_diagnostic : failure -> string option
val failure_blob : failure -> string option
val failure_origin : failure -> string option
