(** This module implements the client-side logic for running property-based
    tests against the native libhegel engine (via {!Hegel_ffi.Ffi}).

    Examples in this documentation assume [open Hegel]. *)

(** Raised when {!assume} condition is [false]. *)
exception Assume_rejected

(** Raised when the engine runs out of choice budget for the current test case
    (StopTest). *)
exception Data_exhausted

(** Raised when the engine detects a flaky strategy definition or when the
    client side pool diverges from the engine side pool. *)
exception Flaky_strategy

(** An opaque per-test-case handle, threaded to the test function and to the
    drawing primitives. Created and owned by the run loop.

    @canonical Hegel.test_case *)
type test_case

(**/**)

val is_high_verbosity : test_case -> bool
val should_print : test_case -> bool
val draw_depth : test_case -> int
val incr_draw_depth : test_case -> unit
val decr_draw_depth : test_case -> unit
val set_test_aborted : test_case -> bool -> unit

(** [clone tc] forks a fresh clone of [tc] on an independent choice stream (its
    own native handle and context), owned and freed by the test case once it
    completes.
    Re-exported as [Hegel.clone]. *)
val clone : test_case -> test_case

(** [with_block tc ~indent f] runs [f] on a test case onto the same choice
    stream as [tc] whose print region is a block nested in [tc]'s: every
    {!note}/draw line it prints is indented [indent] columns further than [tc]'s
    lines. The block is freed when [f] returns or raises, so it must not escape
    [f]. *)
val with_block : test_case -> indent:int -> (test_case -> 'a) -> 'a

(** [owned_clone_count tc] is the number of clone handles the test case
    currently owns; they are freed together once the case completes. *)
val owned_clone_count : test_case -> int

(** A running worker spawned by {!spawn}; joined with {!join}. Re-exported as
    [Hegel.worker]. *)
type 'a worker

(** [spawn tc f] runs [f] on a fresh clone of [tc] on a new thread, capturing
    its result or exception. Re-exported as [Hegel.spawn]. *)
val spawn : test_case -> (test_case -> 'a) -> 'a worker

(** [join w] waits for [w] and returns its result — re-raising any exception the
    worker raised. Re-exported as [Hegel.join]. *)
val join : 'a worker -> 'a

(** [extract_origin exn] extracts an InterestingOrigin string from an exception.
    Uses the backtrace if available; derived from the assertion's location so
    the shrinker can group probes for the same bug. *)
val extract_origin : exn -> string

exception Usage_error of string

(** [generate_boolean tc p forced] draws a boolean with probability [p] of
    [true]. If [forced] is [Some b] the value is forced to [b]. Raises
    {!Data_exhausted} on StopTest. *)
val generate_boolean : test_case -> float -> bool option -> bool

(** [generate_integer tc ~min_value ~max_value] draws an integer in the
    inclusive range. Raises {!Data_exhausted} on StopTest. *)
val generate_integer : test_case -> min_value:int -> max_value:int -> int

(** [generate_float tc ...] draws a width-64 float under the given NaN /
    infinity / exclusion policy. Raises {!Data_exhausted} on StopTest. *)
val generate_float
  :  test_case
  -> min_value:float
  -> max_value:float
  -> allow_nan:bool
  -> allow_infinity:bool
  -> exclude_min:bool
  -> exclude_max:bool
  -> smallest_nonzero_magnitude:float
  -> float

(** [generate_bytes tc ~min_size ~max_size] draws a byte string
    ([max_size = None] means unbounded). Raises {!Data_exhausted} on StopTest. *)
val generate_bytes : test_case -> min_size:int -> max_size:int option -> string

(** [generate_text tc ...] draws a text string over the described alphabet.
    Raises {!Data_exhausted} on StopTest. *)
val generate_text
  :  test_case
  -> min_size:int
  -> max_size:int option
  -> codec:string option
  -> min_codepoint:int
  -> max_codepoint:int
  -> categories:string list option
  -> exclude_categories:string list option
  -> include_characters:string option
  -> exclude_characters:string option
  -> string

(** [generate_regex tc ~pattern ~fullmatch] draws a string matching [pattern]
    (Python-[re] syntax). Raises {!Data_exhausted} on StopTest. *)
val generate_regex : test_case -> pattern:string -> fullmatch:bool -> string

(** [generate_email tc] draws an RFC 5321/5322 email address. Raises
    {!Assume_rejected} when the draw rejects itself. *)
val generate_email : test_case -> string

(** [generate_url tc] draws an RFC 3986 http/https URL. *)
val generate_url : test_case -> string

(** [generate_domain tc ~max_length] draws an RFC 1035 domain name. *)
val generate_domain : test_case -> max_length:int -> string

(** A proleptic Gregorian calendar date, as drawn by {!generate_date}. *)
type date = Hegel_ffi.Ffi.date =
  { year : int
  ; month : int
  ; day : int
  }

(** A time of day at nanosecond resolution, as drawn by {!generate_time}. *)
type time = Hegel_ffi.Ffi.time =
  { hour : int
  ; minute : int
  ; second : int
  ; nanosecond : int
  }

(** [generate_date tc ~min_value ~max_value] draws a Gregorian date in the
    inclusive range. *)
val generate_date : test_case -> min_value:date -> max_value:date -> date

(** [generate_time tc ~min_value ~max_value] draws a time in the inclusive
    range. *)
val generate_time : test_case -> min_value:time -> max_value:time -> time

(** [generate_datetime tc ~min_value ~max_value] draws a naive datetime in the
    inclusive range as a [(date, time)] pair. *)
val generate_datetime
  :  test_case
  -> min_value:date * time
  -> max_value:date * time
  -> date * time

(** [generate_ipv4 tc] draws an IPv4 address as its 4 network-order bytes. *)
val generate_ipv4 : test_case -> string

(** [generate_ipv6 tc] draws an IPv6 address as its 16 network-order bytes. *)
val generate_ipv6 : test_case -> string

(**/**)

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. The [tc] handle is accepted for API symmetry with the other
    per-test-case primitives; the rejection is client-side (raising
    {!Assume_rejected}) and does not consult [tc]. *)
val assume : test_case -> bool -> unit

(** [note tc message] prints [message] to stderr subject to the run's
    [Settings.verbosity]: never under [Quiet], only on the final (failing) replay
    under [Normal], and on every test case under [Verbose] or [Debug]. *)
val note : test_case -> string -> unit

(**/**)

(** [color_enabled ~override ~isatty] decides whether ANSI colors are on: an
    [override] of ["1"]/["0"] (the [HEGEL_COLOR] variable) forces it on/off;
    otherwise follow [isatty]. *)
val color_enabled : override:string option -> isatty:bool -> bool

(** [stderr_color_enabled ()] is {!color_enabled} for the failure report's
    stream, reading the environment and stderr's tty state afresh. *)
val stderr_color_enabled : unit -> bool

(** [stderr_color code s] wraps [s] in the ANSI SGR [code] when colors are
    enabled for stderr, else returns [s] unchanged. *)
val stderr_color : string -> string -> string

(** [set_diff_renderer renderer] sets structural diff renderer {!render_diff}
    delegates to. The optional [hegel.jane] library sets a [sexp_diff]-backed
    renderer through this hook. Without one, {!render_diff} prints both values
    in full. *)
val set_diff_renderer
  :  (colored:bool -> original:Sexplib0.Sexp.t -> updated:Sexplib0.Sexp.t -> string) option
  -> unit

(** [render_diff ~colored ~original ~updated] renders the two differing values:
    both values in full by default ([-]/[+], red/green when [colored]), or a
    structural sexp diff when a renderer is set (see {!set_diff_renderer}). *)
val render_diff
  :  colored:bool
  -> original:Sexplib0.Sexp.t
  -> updated:Sexplib0.Sexp.t
  -> string

(**/**)

(** [require tc ?msg condition] fails the current test case when [condition] is
    [false] by raising [Failure msg]. Unlike [assert] the failure message is
    yours to choose, and unlike {!assume} the case counts as a genuine failure
    rather than being discarded. *)
val require : test_case -> ?msg:string -> bool -> unit

(** [require_equal tc ?msg sexp_of lhs rhs] fails the current test case when the
    two values render to different sexps under [sexp_of]. *)
val require_equal
  :  test_case
  -> ?msg:string
  -> ('a -> Sexplib0.Sexp.t)
  -> 'a
  -> 'a
  -> unit

(**/**)

(** [draw_display_name tc ~label ~repeatable] returns the display name to print
    for a drawn value, bumping the per-test-case occurrence counter for [label].
    A [repeatable] name is numbered on every occurrence ([label_1], [label_2],
    …), while a non-repeatable name is printed bare. *)
val draw_display_name : test_case -> label:string -> repeatable:bool -> string

(**/**)

(** [target tc ~label ~value] records a targeting observation to guide the
    search engine toward higher values. *)
val target : test_case -> label:string -> value:float -> unit

(** [render_sexp ctx printer sexp] writes [sexp] to [printer] in s-expression
    syntax. Lists are printed with breaks between elements when the line
    overflows. *)
val render_sexp
  :  Hegel_ffi.Ffi.context
  -> Hegel_ffi.Ffi.printer
  -> Sexplib0.Sexp.t
  -> unit

(** One piece of a printed draw line: literal text, or a sexp laid out by the
    engine. *)
type draw_segment =
  | Text of string
  | Value of Sexplib0.Sexp.t

(** [print_line tc segments] appends one line assembled from [segments] to the
    test case's print region, subject to the run's verbosity (like [note]). *)
val print_line : test_case -> draw_segment list -> unit

(** [event tc ~label] records [label] as observed on this test case for the
    end-of-run statistics report. *)
val event : test_case -> label:string -> unit

(** [event_value tc ~label ~value] records the finite observation [value] under
    [label] for the end-of-run statistics report. *)
val event_value : test_case -> label:string -> value:float -> unit

(**/**)

(** [start_span ?label tc] starts a generation span for better shrinking. *)
val start_span : ?label:int64 -> test_case -> unit

(** [stop_span ?discard tc] ends the current generation span. *)
val stop_span : ?discard:bool -> test_case -> unit

(** {2 Engine-managed collections}

    Collections let the engine choose the length of a variable-length sequence
    while the caller draws elements one at a time. *)

(** An engine-managed collection. Each one must be released exactly once with
    {!collection_free}. *)
type collection = Hegel_ffi.Ffi.collection

(** [new_collection tc ~min_size ~max_size] starts a collection
    ([max_size = None] means unbounded). *)
val new_collection : test_case -> min_size:int -> max_size:int option -> collection

(** [collection_more tc ~collection] returns whether the engine wants another
    element. *)
val collection_more : test_case -> collection:collection -> bool

(** [collection_reject tc ~collection] rejects the collection's last element. *)
val collection_reject : test_case -> collection:collection -> unit

(** [collection_free tc ~collection] releases [collection]. Safe after the test
    case has aborted. *)
val collection_free : test_case -> collection:collection -> unit

(** {2 Variable pools}

    Pools are the engine-side primitive backing variables in stateful testing
    (see {!Stateful.Pool}). A pool is a set of integer "variable ids" that the
    engine can sample from. *)

(** An engine-managed variable pool. Released automatically when the test case
    that created it completes. *)
type pool = Hegel_ffi.Ffi.pool

(** [new_pool tc] creates a new engine-managed variable pool. It is released
    when the test case completes. *)
val new_pool : test_case -> pool

(** [pool_add tc ~pool] adds a fresh variable to [pool] and returns the new
    variable id. *)
val pool_add : test_case -> pool:pool -> int

(** [pool_generate tc ~pool ?consume ()] draws a variable id from [pool]. When
    [consume] is [true] (default [false]), the variable is also removed from the
    pool. Drawing from an empty pool raises {!Assume_rejected} (the engine
    rejects the test case as invalid). *)
val pool_generate : test_case -> pool:pool -> ?consume:bool -> unit -> int

(** An engine-owned state machine. Each one must be released exactly once with
    {!state_machine_free}. *)
type state_machine = Hegel_ffi.Ffi.state_machine

(** [new_state_machine tc ~rule_names ~invariant_names ~invariants_always_check ~step_count] registers a sequential engine-owned state machine with the
    named rules and invariants, running at most [step_count] rules per test
    case. Raises {!Usage_error} if [rule_names] is empty or [step_count] is
    below 1. *)
val new_state_machine
  :  test_case
  -> rule_names:string list
  -> invariant_names:string list
  -> invariants_always_check:bool list
  -> step_count:int
  -> state_machine

(** [state_machine_next_round tc ~state_machine] asks the engine whether the
    machine should run another round of rules: [false] once the step budget for
    the test case is exhausted. Call it before the first rule and after every
    round. Raises {!Data_exhausted} when the engine's choice budget is
    exhausted. *)
val state_machine_next_round : test_case -> state_machine:state_machine -> bool

(** [state_machine_next_rule tc ~state_machine] draws the index (in
    [\[0, num_rules)]) of the next rule to run this round, letting the engine
    choose and shrink the rule sequence, or returns [None] when the round is
    over. Raises {!Data_exhausted} when the engine's choice budget is exhausted. *)
val state_machine_next_rule : test_case -> state_machine:state_machine -> int option

(** [state_machine_rule_rejected tc ~state_machine] reports that the rule last
    returned by {!state_machine_next_rule} did not complete. A rejected rule
    does not count against the step budget. *)
val state_machine_rule_rejected : test_case -> state_machine:state_machine -> unit

(** [state_machine_should_check_invariant tc ~state_machine ~invariant_index]
    decides whether to run invariant [invariant_index] after the current round.
    It always returns [true] for an invariant set in [invariants_always_check].
    Otherwise, it returns the engine's sampling decision. *)
val state_machine_should_check_invariant
  :  test_case
  -> state_machine:state_machine
  -> invariant_index:int
  -> bool

(** [state_machine_free tc ~state_machine] releases [state_machine]. Safe after
    the test case has aborted, and safe in any order relative to freeing it. *)
val state_machine_free : test_case -> state_machine:state_machine -> unit

(**/**)

(** [run_test ~settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test using the given settings against the native engine.

    @param test_location
      source location of the test, used by the Antithesis integration. Provided
      automatically by the [let%hegel_test] PPX. When omitted, no Antithesis
      assertion is emitted.
    @param database_key
      optional key scoping persisted/replayed failing examples and, under
      [derandomize], the per-test seed. Defaults to the test's [test_location]
      (as [file:function_name]) so each [let%hegel_test] gets a stable, distinct
      key; pass an explicit key to override. When both are absent, the engine
      uses its own default key.
    @param from_ppx
      [true] when the run is driven by the [let%hegel_test] PPX; only set by the
      PPX. Selects the [[@@failure_blobs [...]]] attribute form of the
      [rerun with:] hint vs. the [~failure_blobs] argument form a plain caller
      would use.
    @param failure_blobs
      a list of base64 encoded strings (blobs), where each string encodes the
      choices made in a failing test run. When the list is nonempty, only the
      first blob is decoded and run. The blob is only guaranteed to reproduce a
      failure within a specific version of Hegel *)
val run_test
  :  settings:Settings.t
  -> ?test_location:Antithesis.test_location
  -> ?from_ppx:bool
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(**/**)

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, with [settings] defaulting to
    [Settings.default ()]. This is the entry point the [let%hegel_test] PPX targets;
    re-exported as [Hegel.run_hegel_test].

    @param database_key
      overrides the per-test database key / [derandomize] seed. Defaults to the
      test's [test_location] so each [let%hegel_test] is scoped by its own
      identity. *)
val run_hegel_test
  :  ?settings:Settings.t
  -> ?test_location:Antithesis.test_location
  -> ?from_ppx:bool
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit
