(** Internal — 1:1 ctypes bindings to the native [libhegel] C ABI. Not part of
    the public API; used by the Hegel library internally. *)

(* Low-level ctypes bindings to libhegel, the native property-based testing
   engine exposed as a C library by hegel-rust (hegel-c/include/hegel.h).

   This module is a thin, mechanical 1:1 wrapper over the C ABI: it locates and
   [dlopen]s the shared library, declares each exported function, and exposes
   OCaml-native wrappers that copy borrowed C buffers into OCaml strings and
   translate negative status codes into exceptions. *)

(**/**)

(** Opaque context handle ([hegel_context_t]). *)
type context

(** Opaque settings handle ([hegel_settings_t]). *)
type settings

(** Opaque in-flight run handle ([hegel_run_t]). *)
type run

(** Opaque per-test-case handle ([hegel_test_case_t]). *)
type test_case

(** Opaque aggregated run result ([hegel_run_result_t]), borrowed from its
    parent [run] and valid until [run_free]. *)
type run_result

(** Opaque single-failure handle ([hegel_failure_t]), borrowed from its
    parent {!type:run_result}. *)
type failure

(** Test execution mode ([hegel_mode_t]). *)
type mode =
  | Test_run
  | Single_test_case

(** Randomness backend ([hegel_backend_t]), selected via {!settings_backend}.

    - [Auto]: choose automatically (the default) — urandom under Antithesis,
      otherwise the seeded PRNG.
    - [Default]: expand a single seeded PRNG; runs are reproducible and
      shrinking / replay work as usual.
    - [Urandom]: read fresh entropy on every draw (for running under
      Antithesis); you almost certainly don't want it otherwise. *)
type backend =
  | Auto
  | Default
  | Urandom

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

(** Aggregate outcome of a finished run ([hegel_run_status_t]).

    - [Run_passed]: the property held across every generated test case.
    - [Run_failed]: the property failed; inspect each distinct counterexample
      via {!result_failures}.
    - [Run_error]: the run itself failed — a failed health check, a
      nondeterministic test, an engine panic — and produced no verdict on the
      property. There are no failures to inspect; the message is read via
      {!result_error}. *)
type run_status =
  | Run_passed
  | Run_failed
  | Run_error

(** Raised when a primitive returns [HEGEL_E_STOP_TEST] — the engine has
    exhausted its choice budget for the current test case. *)
exception Stop_test

(** Raised when a primitive returns [HEGEL_E_ASSUME] — the engine rejected the
    current test case as invalid (e.g. an impossible uniqueness constraint that
    exceeds the collection reject limit). Carries no diagnostic. *)
exception Assume_rejected

(** Raised when a libhegel call fails with any other negative status code
    ([HEGEL_E_BACKEND], [HEGEL_E_INVALID_HANDLE], [HEGEL_E_INVALID_ARG],
    [HEGEL_E_ALREADY_COMPLETE], [HEGEL_E_NOT_COMPLETE], [HEGEL_E_INTERNAL], or an
    unrecognised code). The payload is a static label identifying the code,
    followed by {!last_error_message} when the engine set one. *)
exception Backend_error of string

(** {2 Phase bitmask values}

    The [HEGEL_PHASE_*] constants. *)

val phase_explicit : int
val phase_reuse : int
val phase_generate : int
val phase_target : int
val phase_shrink : int

(** [phase_all] ([HEGEL_PHASE_ALL]) is all five phases enabled, the engine
    default. *)
val phase_all : int

(** {2 Health-check bitmask values}

    The [HEGEL_HC_*] constants. *)

val hc_filter_too_much : int
val hc_too_slow : int
val hc_test_cases_too_large : int
val hc_large_initial_test_case : int

(** {2 Diagnostics} *)

(** [version ctx] returns libhegel's version string. *)
val version : context -> string

(** [last_error_message ctx] returns the most recent error on the calling
    thread, or the empty string if the last call succeeded. *)
val last_error_message : context -> string

(** {2 Context} *)

val context_new : unit -> context
val context_free : context -> unit

(** {2 Settings} *)

(** [settings_new ctx] allocates a settings handle with libhegel's defaults.
    Must be released with {!settings_free}. *)
val settings_new : context -> settings

(** [settings_free ctx s] frees a settings handle. *)
val settings_free : context -> settings -> unit

val settings_mode : context -> settings -> mode -> unit

(** [settings_backend ctx s b] pins the engine's randomness backend. Pinning is
    one-way: there is no way to return a handle to [Auto] once set. *)
val settings_backend : context -> settings -> backend -> unit

val settings_test_cases : context -> settings -> int -> unit
val settings_verbosity : context -> settings -> verbosity -> unit

(** [settings_seed ctx s seed] sets the RNG seed ([None] picks a fresh random
    seed at run start). *)
val settings_seed : context -> settings -> int option -> unit

val settings_derandomize : context -> settings -> bool -> unit
val settings_report_multiple_failures : context -> settings -> bool -> unit

(** [settings_database ctx s db] configures the on-disk example database:
    [None] leaves the default, [Some ""] disables it, [Some dir] uses [dir]. *)
val settings_database : context -> settings -> string option -> unit

(** [settings_database_key ctx s key] scopes stored/replayed examples; [None]
    clears it. *)
val settings_database_key : context -> settings -> string option -> unit

(** [settings_phases ctx s mask] enables exactly the phases in the bitmask. *)
val settings_phases : context -> settings -> int -> unit

(** [settings_suppress_health_check ctx s mask] disables the health checks in the
    bitmask. *)
val settings_suppress_health_check : context -> settings -> int -> unit

(** {2 Run lifecycle} *)

(** [run_start ctx s] starts a run with the given settings. Raises
    {!Backend_error} on failure. The handle must be freed with {!run_free}. *)
val run_start : context -> settings -> run

(** [next_test_case ctx run] blocks until the engine produces the next test case,
    or returns [None] when the run is finished. Raises {!Backend_error} on
    engine error or caller misuse. *)
val next_test_case : context -> run -> test_case option

(** [test_case_from_blob ctx settings blob] builds a standalone test case that
    replays the example encoded in a base64 failure [blob]. Raises
    {!Backend_error} (with the engine's diagnostic) when the blob is missing,
    not UTF-8, or cannot be decoded — the engine never returns a null handle
    without setting an error. The handle must be freed with
    {!blob_test_case_free}. *)
val test_case_from_blob : context -> settings -> string option -> test_case

(** [run_result ctx run] returns the aggregated result of a finished run. Raises
    {!Backend_error} if the run has not finished. *)
val run_result : context -> run -> run_result

(** [run_free ctx run] frees a run handle, draining the worker thread. *)
val run_free : context -> run -> unit

(** [blob_test_case_free ctx tc] frees a test case created by a failure_blob *)
val blob_test_case_free : context -> test_case -> unit

(** {2 Per-test-case primitives} *)

(** [generate ctx tc schema] draws a value described by the CBOR-encoded [schema],
    returning the CBOR-encoded value bytes. Raises {!Stop_test} when the
    choice budget is exhausted, {!Backend_error} on a malformed schema. *)
val generate : context -> test_case -> string -> string

val primitive_boolean : context -> test_case -> float -> bool option -> bool
val start_span : context -> test_case -> int -> unit
val stop_span : context -> test_case -> bool -> unit

(** [new_collection ctx tc ~min_size ~max_size] starts an engine-managed
    collection ([max_size = None] means unbounded) and returns its id. *)
val new_collection : context -> test_case -> min_size:int -> max_size:int option -> int

(** [collection_more ctx tc id] returns whether the engine wants another element. *)
val collection_more : context -> test_case -> int -> bool

(** [collection_reject ctx tc id why] rejects the collection's last element. *)
val collection_reject : context -> test_case -> int -> string option -> unit

(** [new_pool ctx tc] creates a variable pool and returns its id. *)
val new_pool : context -> test_case -> int

(** [pool_add ctx tc ~pool_id] registers a fresh variable and returns its id. *)
val pool_add : context -> test_case -> pool_id:int -> int

(** [pool_generate ctx tc ~pool_id ~consume] draws (and optionally consumes) a
    variable id from the pool. Raises {!Stop_test} if the pool is empty. *)
val pool_generate : context -> test_case -> pool_id:int -> consume:bool -> int

(** [new_state_machine ctx tc ~rule_names ~invariant_names] registers an
    engine-owned state machine with the named rules and invariants and returns
    its id. The engine owns rule selection (including swarm testing). Raises
    {!Backend_error} if [rule_names] is empty. *)
val new_state_machine
  :  context
  -> test_case
  -> rule_names:string list
  -> invariant_names:string list
  -> int

(** [state_machine_next_rule ctx tc ~state_machine_id] draws the index of the next
    rule to run, in [\[0, num_rules)]. Raises {!Stop_test} when the engine's
    choice budget is exhausted. *)
val state_machine_next_rule : context -> test_case -> state_machine_id:int -> int

(** [target ctx tc value label] records a targeting observation. *)
val target : context -> test_case -> float -> string -> unit

(** [mark_complete ctx tc status origin] reports the test case's outcome. [origin]
    is used only for {!Interesting} and must be stable per bug. *)
val mark_complete : context -> test_case -> status -> string option -> unit

(** {2 Result inspection} *)

(** [result_status ctx r] is the run's aggregate status: passed, failed, or
    errored. *)
val result_status : context -> run_result -> run_status

(** [result_error ctx r] is the run-level error message when the run ended in
    {!Run_error} — a failed health check, a nondeterministic test, or an
    engine panic — or [None] when it completed normally. *)
val result_error : context -> run_result -> string option

val result_failure_count : context -> run_result -> int

(** [result_failure ctx r i] returns the [i]-th distinct failure, or [None] if
    out of range. *)
val result_failure : context -> run_result -> int -> failure option

(** [result_failures ctx r] returns all distinct failures in order. *)
val result_failures : context -> run_result -> failure list

val failure_blob : context -> failure -> string option
val failure_origin : context -> failure -> string option
