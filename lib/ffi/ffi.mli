(* Low-level ctypes bindings to libhegel, the native property-based testing
   engine exposed as a C library by hegel-rust (hegel-c/include/hegel.h).

   This module is a thin, mechanical 1:1 wrapper over the C ABI: it locates and
   [dlopen]s the shared library, declares each exported function, and exposes
   OCaml-native wrappers that copy borrowed C buffers into OCaml strings and
   translate negative status codes into exceptions. *)

(** Opaque context handle ([hegel_context_t]). *)
type context

(** Opaque settings handle ([hegel_settings_t]). *)
type settings

(** Opaque in-flight run handle ([hegel_run_t]). *)
type run

(** Opaque per-test-case handle ([hegel_test_case_t]): caller-owned whatever its
    origin ({!next_test_case}, {!test_case_from_blob}, …), freed with
    {!test_case_free}. A run-owned handle stays valid even after {!run_free}
    releases the run's own reference. *)
type test_case

(** Opaque aggregated run result ([hegel_run_result_t]): a caller-owned snapshot
    independent of its {!run}, valid until freed with {!run_result_free}. *)
type run_result

(** Opaque single-failure handle ([hegel_failure_t]): a caller-owned snapshot
    independent of its {!type:run_result}, valid until freed with
    {!failure_free}. *)
type failure

(** Opaque string-generator handle ([hegel_string_generator_t]): built with a
    context via one of the {!string_generator_text} / {!string_generator_regex}
    / … constructors, drawn from with {!generate_string}, and freed with
    {!string_generator_free}. *)
type string_generator

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
val phase_explain : int

(** [phase_all] ([HEGEL_PHASE_ALL]) is all six phases enabled, the engine
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
    {!test_case_free}. *)
val test_case_from_blob : context -> settings -> string option -> test_case

(** [run_result ctx run] returns the aggregated result of a finished run. Raises
    {!Backend_error} if the run has not finished. *)
val run_result : context -> run -> run_result

(** [run_free ctx run] frees a run handle, draining the worker thread. Run-result
    and failure snapshots are independent and outlive it; free them separately. *)
val run_free : context -> run -> unit

(** [run_result_free ctx r] frees a run-result snapshot from {!val:run_result}. *)
val run_result_free : context -> run_result -> unit

(** [failure_free ctx f] frees a failure snapshot from {!result_failure} /
    {!result_failures}. *)
val failure_free : context -> failure -> unit

(** [test_case_free ctx tc] frees a test-case handle, whatever its origin
    (run-owned from {!next_test_case}, cloned, or from a failure blob). *)
val test_case_free : context -> test_case -> unit

(** {2 Per-test-case primitives} *)

(** [generate_boolean ctx tc p forced] draws a boolean that is [true] with
    probability [p]. When [forced] is [Some b] the result is forced to [b]
    (recorded but consuming no entropy). Raises {!Stop_test} on budget
    exhaustion. *)
val generate_boolean : context -> test_case -> float -> bool option -> bool

(** [generate_integer ctx tc ~min_value ~max_value] draws an integer in
    [\[min_value, max_value\]]. Raises {!Stop_test} on budget exhaustion. *)
val generate_integer : context -> test_case -> min_value:int -> max_value:int -> int

(** [generate_float ctx tc ...] draws a width-64 float in
    [\[min_value, max_value\]] under the given NaN / infinity / exclusion policy.
    Pass [neg_infinity] / [infinity] for unbounded ends and
    [smallest_nonzero_magnitude] (e.g. [5e-324]) for no magnitude restriction.
    Raises {!Stop_test} on budget exhaustion. *)
val generate_float
  :  context
  -> test_case
  -> min_value:float
  -> max_value:float
  -> allow_nan:bool
  -> allow_infinity:bool
  -> exclude_min:bool
  -> exclude_max:bool
  -> smallest_nonzero_magnitude:float
  -> float

(** [generate_bytes ctx tc ~min_size ~max_size] draws a byte string with length
    in [\[min_size, max_size\]] ([max_size = None] means unbounded). Raises
    {!Stop_test} on budget exhaustion. *)
val generate_bytes : context -> test_case -> min_size:int -> max_size:int option -> string

(** [string_generator_text ctx ...] builds a text string generator over the
    described alphabet. [max_size = None] means unbounded. Raises {!Backend_error}
    on invalid parameters. The handle must be freed with {!string_generator_free}. *)
val string_generator_text
  :  context
  -> min_size:int
  -> max_size:int option
  -> codec:string option
  -> min_codepoint:int
  -> max_codepoint:int
  -> categories:string list option
  -> exclude_categories:string list option
  -> include_characters:string option
  -> exclude_characters:string option
  -> string_generator

(** [string_generator_regex ctx ~pattern ~fullmatch] builds a regex string
    generator (Python-[re] syntax). Raises {!Backend_error} on an invalid
    pattern. The handle must be freed with {!string_generator_free}. *)
val string_generator_regex
  :  context
  -> pattern:string
  -> fullmatch:bool
  -> string_generator

(** [string_generator_email ctx] builds an RFC 5321/5322 email generator. *)
val string_generator_email : context -> string_generator

(** [string_generator_url ctx] builds an RFC 3986 http/https URL generator. *)
val string_generator_url : context -> string_generator

(** [string_generator_domain ctx ~max_length] builds an RFC 1035 domain-name
    generator of total length at most [max_length]. Raises {!Backend_error} when
    [max_length] leaves no eligible TLDs. *)
val string_generator_domain : context -> max_length:int -> string_generator

(** [string_generator_free ctx sg] frees a string-generator handle. *)
val string_generator_free : context -> string_generator -> unit

(** [generate_string ctx tc sg] draws a string described by [sg]. Raises
    {!Stop_test} on budget exhaustion and {!Assume_rejected} when the draw
    rejects itself (e.g. an over-length email). *)
val generate_string : context -> test_case -> string_generator -> string

(** [generate_date ctx tc] draws a Gregorian date as [(year, month, day)].
    Raises {!Stop_test} on budget exhaustion. *)
val generate_date : context -> test_case -> int * int * int

(** [generate_time ctx tc] draws a time of day as
    [(hour, minute, second, microsecond)]. Raises {!Stop_test} on budget
    exhaustion. *)
val generate_time : context -> test_case -> int * int * int * int

(** [generate_datetime ctx tc] draws a naive datetime as [(date, time)]. Raises
    {!Stop_test} on budget exhaustion. *)
val generate_datetime
  :  context
  -> test_case
  -> (int * int * int) * (int * int * int * int)

(** [generate_ipv4 ctx tc] draws an IPv4 address as its 4 network-order bytes. *)
val generate_ipv4 : context -> test_case -> string

(** [generate_ipv6 ctx tc] draws an IPv6 address as its 16 network-order bytes. *)
val generate_ipv6 : context -> test_case -> string

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

(** {2 Pretty-printer documents}

    Bindings to the engine's layout machinery ([hegel_printer_*]): a document
    built from text, break points, and groups, with deferred holes, retractable
    speculative regions, and end-of-line comments. The engine owns layout only;
    what is printed — and in which language's syntax — is the client's choice.
    The hegel library wraps these in its OCaml-facing [Pretty] module. *)

(** Opaque printer handle ([hegel_printer_t]): the main document (from
    {!printer_new} or {!test_case_printer}) or a deferred slot (from
    {!printer_deferred}). Every handle is freed with {!printer_free}. *)
type printer

(** [printer_new ctx ~max_width] creates a standalone document that keeps lines
    within [max_width] characters where the group structure allows it. *)
val printer_new : context -> max_width:int -> printer

(** [printer_free ctx p] releases a printer handle. Content already printed
    stays in the shared document. *)
val printer_free : context -> printer -> unit

(** [printer_text ctx p s] emits literal text. Must not contain newlines. *)
val printer_text : context -> printer -> string -> unit

(** [printer_if_break ctx p s] emits [s] only if the innermost open group
    renders broken; a group that fits on one line renders nothing here, and
    [s] never counts toward width. *)
val printer_if_break : context -> printer -> string -> unit

(** [printer_breakable ctx p sep] emits a break point rendering as [sep] when
    the enclosing group fits on one line, and as a newline plus indentation
    when it breaks. *)
val printer_breakable : context -> printer -> string -> unit

(** [printer_comment ctx p text] attaches a comment — passed in full rendered
    form, e.g. ["  (* like this *)"] — to the line being written: it is
    emitted at the end of that line, forces every open group to break, and is
    excluded from width accounting. *)
val printer_comment : context -> printer -> string -> unit

(** [printer_hard_break ctx p] emits an unconditional newline plus the current
    indentation. *)
val printer_hard_break : context -> printer -> unit

(** [printer_begin_group ctx p ~indent open_] opens a group: emits [open_],
    then indents subsequent break points by [indent]. *)
val printer_begin_group : context -> printer -> indent:int -> string -> unit

(** [printer_end_group ctx p ~dedent close] closes the innermost group:
    dedents by [dedent], then emits [close]. *)
val printer_end_group : context -> printer -> dedent:int -> string -> unit

(** [printer_shift_indent ctx p delta] adjusts the indentation applied by
    subsequent break points. *)
val printer_shift_indent : context -> printer -> int -> unit

(** [printer_deferred ctx p] opens a deferred hole at [p]'s current position
    and returns a handle onto its slot; content written to the slot later is
    spliced in at the hole's position when the document renders. *)
val printer_deferred : context -> printer -> printer

(** [printer_begin_speculative ctx p] opens a speculative region: subsequent
    output buffers until committed or aborted. *)
val printer_begin_speculative : context -> printer -> unit

(** [printer_commit_speculative ctx p] keeps the innermost speculative
    region's content. *)
val printer_commit_speculative : context -> printer -> unit

(** [printer_abort_speculative ctx p] discards the innermost speculative
    region's content. *)
val printer_abort_speculative : context -> printer -> unit

(** [printer_resolve ctx p] closes the outstanding deferred session: every
    slot's content is spliced in and the slots die. *)
val printer_resolve : context -> printer -> unit

(** [printer_is_live ctx p] is whether a deferred slot can still be written. *)
val printer_is_live : context -> printer -> bool

(** [printer_value ctx p] lays the document out and returns everything printed
    so far. *)
val printer_value : context -> printer -> string

(** [test_case_printer ctx tc ~max_width] fetches (creating on first use) the
    document shared by [tc]'s test-case family. The document survives the test
    case's completion, so the client can read it after [mark_complete]. *)
val test_case_printer : context -> test_case -> max_width:int -> printer

(** [note ctx tc s] appends a note to the test case's document; each
    newline-separated line of [s] becomes its own output line. *)
val note : context -> test_case -> string -> unit

(** [test_case_choice_count ctx tc] is the number of choices [tc] has recorded
    so far. Snapshotting it around a draw yields the choice slice the draw
    consumed, for matching explain annotations. *)
val test_case_choice_count : context -> test_case -> int

(** [failure_comment_count ctx f] is the number of explain-phase annotations
    on [f]. *)
val failure_comment_count : context -> failure -> int

(** [failure_comment ctx f i] is the [i]-th explain annotation: the half-open
    choice slice [(start, end)] of the shrunk counterexample it applies to and
    its text (without comment syntax). The whole-test "varied together" note
    uses the marker slice [(0, 0)]. *)
val failure_comment : context -> failure -> int -> int * int * string

(** [failure_comments ctx f] is every explain annotation on [f], in slice
    order. *)
val failure_comments : context -> failure -> (int * int * string) list
