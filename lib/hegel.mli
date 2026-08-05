(** {2 Introduction}
    Property-based testing for OCaml, powered by the native {{:https://hegel.dev} Hegel} engine based on {{:https://hypothesis.works} Hypothesis}.

    Hegel runs the test function on many generated inputs. You generate data {e inline},
    drawing values with {!draw} as the test runs, rather than generating the data
    then running the property body. Each {!draw} returns an ordinary OCaml value
    that you bind with [let], compute with, and branch on, so a later draw can 
    depend on an earlier generated value or a value from the system under test.

    Because Hegel uses
    {{:https://hypothesis.works/articles/integrated-shrinking/} integrated
    shrinking}, shrinking comes for free.

    {2 Getting started}
    
    {3 Install Hegel}

    To install Hegel for OCaml:

    {@shell[
      opam install hegel
    ]}

    The version of Hegel in OPAM sometimes lags behind the version in Github. To pin the
    version in Github:
    {@shell[
      opam pin add hegel "git+https://github.com/hegeldev/hegel-ocaml.git"
    ]}

    Hegel for OCaml supports {b Linux} (amd64/arm64) and {b macOS} (Apple Silicon).
    macOS amd64 (Intel) has no published [libhegel] artifact, so on that platform point
    [HEGEL_LIBHEGEL_PATH] at a locally built [libhegel.dylib].

    Hegel works with whatever test framework your project already uses. The
    examples below use {{:https://github.com/mirage/alcotest} Alcotest}.

    Add [hegel] and [alcotest] to your dune test stanza. The examples in this
    documentation also use [ppx_sexp_conv]'s [\[%sexp_of: t\]] to write value
    printers.

    {[
      (test
       (name my_tests)
       (libraries hegel alcotest)
       (preprocess (pps ppx_hegel_test ppx_sexp_conv)))
    ]}

    {3 Write your first test}

    Write a property test using [let%hegel_test]:

    {[
      open Hegel

      let%hegel_test commutative_addition tc =
        let a = draw tc (Generators.integers ~min_value:(-1000) ~max_value:1000 ()) in
        let b = draw tc (Generators.integers ~min_value:(-1000) ~max_value:1000 ()) in
        require_equal tc [%sexp_of: int] (a + b) (b + a)

      let () =
        Alcotest.run
          "my_tests"
          [ "properties", [ Alcotest.test_case "commutative addition" `Quick commutative_addition ] ]
    ]}

    We check the property with {!require_equal} rather than
    [assert (a + b = b + a)]. It takes a printer for the values and, when they
    differ, shows a structural diff of the two sides in the failure report
    instead of a bare "assertion failed". Use {!require} for a boolean check with
    a custom message. A plain [assert] can be used as well, but it does not provide
    as much information as {!require_equal} and {!require}.

    Run [dune runtest]. You should see Alcotest report the test as passing.
    Hegel generates up to 100 random input pairs and reports the minimal
    counterexample if it finds one. When a test fails, Hegel prints each
    value you drew from the failing case, named after the [let] binding it was
    bound to ([a = …], [b = …]).

    The rest of the examples in the documentation assume you have [open Hegel]
    at the top of the test file like in the example above, and refer to
    generators as [Generators.foo].

    Next, let's try a test that fails.

    {[
      let%hegel_test every_int_is_small tc =
        let n = draw tc (Generators.integers ()) in
        assert (n < 50)
    ]}

    This test asserts that any integer is less than 50, which is obviously
    incorrect. Hegel finds a test case that makes the assertion fail, then shrinks
    it to the smallest counterexample ([n = 50]). The final replay prints the
    drawn values, the exception, and a [rerun with:] line that replays the exact case:

    {v
      --- Failure: every_int_is_small (my_tests.ml:3) ------------------
      Falsified after 1 test case (0 discarded):

        n = 50

      Exception: File "my_tests.ml", line 5, characters 2-8: Assertion failed
      rerun with: [@@failure_blobs [ "AAEAAAAACgEAAAAy" ]]
    v}

    To fix this test, you can constrain the integers you generate with [min_value] and [max_value]:

    {[
      let%hegel_test every_int_is_small tc =
        let n = draw tc (Generators.integers ~min_value:0 ~max_value:49 ()) in
        assert (n < 50)
    ]}

    {3 Use generators}

    Hegel provides a rich library of generators that you can use out of the box.
    There are primitive generators, such as {!Generators.integers},
    {!Generators.floats}, and {!Generators.text}, and combinators that build
    generators out of other generators, such as {!Generators.lists} and
    {!Generators.tuples2}.

    For instance, you can use {!Generators.lists} to construct a list of
    integers:

    {[
      let%hegel_test append_increases_length tc =
        let xs = draw tc (Generators.lists (Generators.integers ()) ()) in
        let initial_length = List.length xs in
        let xs = draw tc (Generators.integers ()) :: xs in
        require tc ~msg:"prepending an element must grow the list"
          (List.length xs > initial_length)
    ]}

    Custom generators are also supported. Suppose you have a [person] record that
    requires generation. Build a generator for it with {!Generators.composite},
    drawing each field in sequence:

    {[
      type person =
        { age : int
        ; name : string
        }

      let person =
        Generators.composite (fun tc ->
          let age = draw_silent tc (Generators.integers ()) in
          let name = draw_silent tc (Generators.text ()) in
          { age; name })
    ]}

    You can chain drawing operations together, so a later draw depends on an
    earlier one. For instance, extending [person] with a [driving_license] field
    that can only be [true] once [age] is at least 18:

    {[
      type person =
        { age : int
        ; name : string
        ; driving_license : bool
        }

      let person =
        Generators.composite (fun tc ->
          let age = draw_silent tc (Generators.integers ()) in
          let name = draw_silent tc (Generators.text ()) in
          let driving_license =
            if age >= 18 then draw_silent tc (Generators.booleans ()) else false
          in
          { age; name; driving_license })
    ]}

    {3 Changing test settings}

    To override the default settings, attach a [\[@@settings ...\]] attribute:

    {[
      let%hegel_test commutative_addition tc =
        let a = draw tc (Generators.integers ()) in
        let b = draw tc (Generators.integers ()) in
        require_equal tc [%sexp_of: int] (a + b) (b + a)
      [@@settings settings ~test_cases:500 ()]
    ]}

    This increases the number of test cases run from 100 to 500.

    You can also update settings using the [with_*] functions:
    {[
      let%hegel_test commutative_addition tc =
        let a = draw tc (Generators.integers ()) in
        let b = draw tc (Generators.integers ()) in
        require_equal tc [%sexp_of: int] (a + b) (b + a)
      [@@settings settings ~test_cases:500 () |> with_seed 5 |> with_verbosity Verbose]
    ]}

    {3 Debugging failing test cases}

    Use {!note} to attach debug information:

    {[
      let%hegel_test every_int_is_small tc =
        let n = draw tc (Generators.integers ()) in
        note tc (Printf.sprintf "n is %d" n);
        assert (n < 50)
    ]}

    A failing run prints a framed report: the shrunk counterexample's draws
    and notes, the exception, and a copy-pasteable [rerun with:] line whose
    base64 blob encodes the choice sequence that caused the failure (disable
    it with [with_print_blob false]). On a terminal the report headers (and
    {!require_equal} diffs) print in color; set [HEGEL_COLOR] to [1] or [0]
    to force colors on or off.

    For an equality property, prefer {!require_equal} over [assert (x = y)]: it
    adds a structural diff of the two values to this report, so you see exactly
    how they differ. {!require} is the message-carrying boolean variant.

    {[
      let%hegel_test every_int_is_small tc =
        let n = draw tc (Generators.integers ()) in
        assert (n < 50)
    ]}

    {v
      --- Failure: every_int_is_small (my_tests.ml:3) ------------------
      Falsified after 2 test cases (0 discarded):

        n = 50

      Exception: File "my_tests.ml", line 5, characters 2-8: Assertion failed
      rerun with: [@@failure_blobs [ "AAEAAAAACgEAAAAy" ]]
    v}

    The blob can then be used to replay the failing test case:

    {[
      let%hegel_test every_int_is_small tc =
        let n = draw tc (Generators.integers ()) in
        assert (n < 50)
      [@@failure_blobs [ "AAEAAAAACgEAAAAy" ]]
    ]}

    The blob is only meant to reproduce the failure within a specific version of
    Hegel, since the choice sequence leading to a failure can change from version 
    to version.
    
    {3 Learning more}
    See {!Generators} for the generators and {!Stateful} for state-machine testing.
    *)

(** {2 Hegel module documentation}*)

(** The current version of Hegel for OCaml. *)
val version : string

(** An opaque handle for the current test case, passed to your test function and
    threaded to {!draw} and the other drawing primitives. *)
type test_case = Internal.test_case

(** {3 Submodules} *)

(** Generators for composable test data generation. *)
module Generators = Generators

(** Stateful property-based testing. *)
module Stateful = Stateful

(**/**)

(* Excluded from the documentation, not for direct use *)

module Derive = Derive
module Internal = Internal
module Antithesis = Antithesis

(**/**)

(** {3 Settings}

    Build a {!type:settings} value with {!default_settings} or {!val:settings},
    refine it with the [with_*] functions, and attach it to a [let%hegel_test]
    with the [\[@@settings ...\]] attribute:

    {[
      let%hegel_test many_cases tc =
        let n = draw tc (Generators.integers ~min_value:0 ~max_value:99 ()) in
        assert (n < 100)
      [@@settings settings ~test_cases:500 () |> with_verbosity Verbose]
    ]} *)

(** How much output Hegel produces during a run. *)
type verbosity = Internal.verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** Where Hegel stores and replays failing examples. *)
type database = Internal.database =
  | Unset
  | Disabled
  | Path of string

(** How a test run is executed. *)
type mode = Internal.mode =
  | Test_run (** the default: many cases, shrinking, database replay *)
  | Single_test_case (** run the body once, with no shrinking or replay *)

(** Phases of a test run that can be enabled or disabled with {!with_phases}. *)
type phase = Internal.phase =
  | Explicit
  (** Reserved for future use: hegel-ocaml has no explicit-examples facility
        yet, so selecting this phase currently has no effect. *)
  | Reuse (** replay previously failing examples from the {!type:database} *)
  | Generate (** generate new test cases *)
  | Target (** targeted search guided by {!target} observations *)
  | Shrink (** shrink discovered counterexamples *)

(** Health checks that can be suppressed with {!with_suppress_health_check}. *)
type health_check = Internal.health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(** Configuration for a test run. Build one with {!default_settings} or
    {!val:settings} and refine it with the [with_*] functions below. *)
type settings = Internal.settings =
  { mode : mode
  ; test_cases : int
  ; stateful_step_count : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
    (** Where failing examples are stored. When set, Hegel replays test cases
        from previous failed runs and records new failures as they occur. *)
  ; suppress_health_check : health_check list
  ; phases : phase list option
    (** [None] uses the engine's default phase list (all phases); [Some xs]
          restricts execution to [xs]. *)
  ; print_blob : bool
    (** Print a [rerun with:] line whose base64 blob
        encodes the engine choices that led to a failure. [true] by default. *)
  ; report_multiple_failures : bool (** [false] by default. *)
  }

(** [default_settings ()] creates default test settings, auto-detecting CI. In
    CI, [derandomize] is [true] and the [database] is [Disabled]. *)
val default_settings : unit -> settings

(** [settings ?test_cases ?seed ()] applies the given overrides to
    {!default_settings}. Convenience constructor for the common cases.

    {[
      let s = settings ~test_cases:500 ~seed:42 ()
    ]} *)
val settings : ?test_cases:int -> ?seed:int -> unit -> settings

(** [with_test_cases n s] sets the number of test cases to run. *)
val with_test_cases : int -> settings -> settings

(** [with_stateful_step_count n s] sets the target number of steps per stateful
    test case (see {!Stateful}). Each case runs at least one step and at most
    [n]. Defaults to 50. [n] must be at least 1. *)
val with_stateful_step_count : int -> settings -> settings

(** [with_verbosity v s] sets how much printed output the run produces.

    {[
      let s = default_settings () |> with_verbosity Verbose
    ]} *)
val with_verbosity : verbosity -> settings -> settings

(** [with_seed seed s] sets the run's seed. *)
val with_seed : int option -> settings -> settings

(** [with_derandomize b s] makes the run reproducible by deriving its seed from
    the test's identity instead of fresh randomness. *)
val with_derandomize : bool -> settings -> settings

(** [with_database db s] sets where failing examples are persisted and replayed.

    {[
      let s = default_settings () |> with_database (Path "_hegel_db")
    ]} *)
val with_database : database -> settings -> settings

(** [with_suppress_health_check checks s] disables exactly the given health
    checks, replacing any previously suppressed list (like the other [with_*]
    builders).

    {[
      let s = default_settings () |> with_suppress_health_check [ Filter_too_much; Too_slow ]
    ]} *)
val with_suppress_health_check : health_check list -> settings -> settings

(** [with_phases phases s] restricts the run to the given phases.

    {[
      let s = default_settings () |> with_phases [ Generate; Shrink ]
    ]} *)
val with_phases : phase list -> settings -> settings

(** [with_mode mode s] sets the execution mode. *)
val with_mode : mode -> settings -> settings

(** [with_print_blob b s] controls whether a failing run's report ends with a
    copy-pasteable [rerun with:] line encoding the
    engine choices that led to the failure. On by default. *)
val with_print_blob : bool -> settings -> settings

(** [with_report_multiple_failures b s] makes a failing run report every distinct
    failure it found rather than just the first. *)
val with_report_multiple_failures : bool -> settings -> settings

(** {3 Running tests} *)

(** A source location identifying a single test, used to create the test's key 
    in the {!type:database} and by the Antithesis integration to build its assertion. 
    The [let%hegel_test] PPX builds one automatically. Construct one manually to 
    pass [~test_location] to a direct {!run_hegel_test} call. *)
type test_location = Antithesis.test_location =
  { function_name : string
  ; file : string (** Full source path as captured by [__FILE__]. *)
  ; begin_line : int (** 1-based line number of the test's [let] binding. *)
  }

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, defaulting to
    {!default_settings}. Call it directly to drive a property from a plain
    executable or another test harness:

    {[
      let my_settings = settings ~test_cases:50 ~seed:5 () in
      let () =
        run_hegel_test ~settings:my_settings (fun tc ->
          let n = draw tc (integers ~min_value:0 ~max_value:9 ()) in
          assert (n >= 0 && n <= 9))
    ]}
    @param test_location
    source location of the test, used by the Antithesis integration.
    Provided automatically by the [let%hegel_test] PPX. When omitted, no
    Antithesis assertion is emitted.
    @param database_key
    optional key scoping persisted/replayed failing examples and, under [derandomize],
    the per-test seed. Defaults to the test's [test_location] (as
    [file:function_name]) so each [let%hegel_test] gets a stable, distinct
    key; pass an explicit key to override. When both are absent, the engine
    uses its own default key.
    @param failure_blobs
    a list of base64 encoded strings (blobs), where each string encodes the choices
    made in a failing test run. When the list is nonempty, only the first blob
    is decoded and run. A blob is only guaranteed to reproduce a failure within
    the same version of Hegel. *)
val run_hegel_test
  :  ?settings:settings
  -> ?test_location:test_location
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(**/**)

(** [run_hegel_test_ppx] is {!run_hegel_test} with the PPX replay hint enabled,
    so a failing run suggests the [[@@failure_blobs [...]]] attribute rather than
    the [~failure_blobs] argument. The [let%hegel_test] PPX targets it; not for
    direct use. *)
val run_hegel_test_ppx
  :  ?settings:settings
  -> ?test_location:test_location
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(**/**)

(** {3 Drawing values} *)

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc].

    On the final replay of a failing test (or on every case under verbose
    output), an outermost draw prints its value as [name = value], where [name]
    is [label] when given, else ["draw"]. An unlabeled draw is numbered
    (["draw_1"], ["draw_2"], …) while a [label] is printed bare. To draw a generator
    with no printer, use {!draw_silent} or attach a printer with {!with_printer}.

    Inside a [let%hegel_test], the PPX supplies the binding name as the label, so
    [let x = draw tc gen] prints its value as [x = value].
    When the same name is shadowed or drawn in a loop, its draws are numbered 
    [x_1], [x_2], … in draw order. Pass [?label] to override the name 
    (e.g. [draw ~label:"y" tc gen]).

    {[
      let%hegel_test draw_example tc =
        let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
        assert (n >= 0)
    ]} *)
val draw
  :  ?label:string
  -> test_case
  -> ('a, Generators.printable) Generators.generator
  -> 'a

(**/**)

(** [draw_named ~label ~repeatable tc gen] is the naming-aware draw the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw}). See {!Generators.draw_named}. *)
val draw_named
  :  label:string
  -> repeatable:bool
  -> test_case
  -> ('a, Generators.printable) Generators.generator
  -> 'a

(**/**)

(** [draw_silent tc gen] produces a typed value from any generator without
    recording it for the final-replay output. Use it for draws whose value is not
    a useful part of the printed counterexample, or for generators that carry no
    printer.

    {[
      let%hegel_test draw_silent_example tc =
        let n = draw_silent tc (map (fun x -> x * 2) (integers ~min_value:0 ~max_value:9 ())) in
        assert (n >= 0)
    ]} *)
val draw_silent : test_case -> ('a, 'p) Generators.generator -> 'a

(**/**)

(** [draw_silent_named ~name tc gen] is the naming-aware {!draw_silent} the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw_silent}). See {!Generators.draw_silent_named}. *)
val draw_silent_named : name:string -> test_case -> ('a, 'p) Generators.generator -> 'a

(**/**)

(** {3 Guiding generation} *)

(** Raised by {!assume} when its condition is [false] (rejecting the current test
    case). *)
exception Assume_rejected

(** [assume tc condition] states a {e precondition}. If [condition] is [false]
    the current test case is discarded (not failed) and Hegel generates
    another. Use it to skip inputs that do not apply to a property.

    {[
      let%hegel_test head_cons_tail_reconstructs tc =
        let xs = draw tc (lists (integers ()) ()) in
        (* The property is only meaningful for non-empty lists. *)
        assume tc (xs <> []);
        assert (List.hd xs :: List.tl xs = xs)
    ]}

    Discarding too many cases trips the [Filter_too_much] health check. For a
    narrow precondition, write a generator that generates valid inputs by
    construction (e.g. making the minimum size of the list 1 in the example above).

    The [tc] handle is accepted for API symmetry with the other per-test-case
    primitives; the rejection itself is client-side and does not consult [tc]. *)
val assume : test_case -> bool -> unit

(** [target tc value label] sends a target command to guide the search engine
    toward higher values.

    {[
      let%hegel_test grow_size tc =
        let v = draw tc (integers ~min_value:0 ~max_value:1000 ()) in
        target tc (float_of_int v) "size";
        assert (v <= 1000)
    ]} *)
val target : test_case -> float -> string -> unit

(** {3 Debugging tests} *)

(** [note tc message] prints [message] to stderr subject to the run's verbosity:
    never under [Quiet], only on the final (failing) replay under [Normal], and
    on every test case under [Verbose] or [Debug].

    {[
      let%hegel_test note_value tc =
        let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
        note tc (Printf.sprintf "n is %d" n);
        assert (n < 100)
    ]} *)
val note : test_case -> string -> unit

(** [require tc ?msg condition] fails the current test case when [condition] is
    [false] by raising [Failure msg] ([msg] defaults to a generic message).

    {[
      let%hegel_test balanced tc =
        let l = draw tc (lists (integers ()) ()) in
        require tc ~msg:"sum must stay non-negative" (running_sum l >= 0)
    ]} *)
val require : test_case -> ?msg:string -> bool -> unit

(** [require_equal tc ?msg sexp_of lhs rhs] fails the current test case when
    the two values render to different sexps under [sexp_of]. With the optional 
    [hegel.core] library's structural diff set, a [sexp_diff] two-column diff is 
    printed.

    {[
      let%hegel_test sort_is_stable tc =
        let l = draw tc (lists (integers ()) ()) in
        require_equal
          tc
          [%sexp_of: int list]
          (List.sort compare l)
          (stable_sort l)
    ]} *)
val require_equal
  :  test_case
  -> ?msg:string
  -> ('a -> Sexplib0.Sexp.t)
  -> 'a
  -> 'a
  -> unit

(** [with_printer sexp_of gen] attaches (or replaces) [gen]'s printer, yielding a
    printable generator that {!draw} accepts. This is how a
    [map]/[flat_map]/[sampled_from]/[just] result is made drawable with {!draw}.

    {[
      let%hegel_test with_printer_example tc =
        let doubled = map (fun x -> x * 2) (integers ~min_value:0 ~max_value:9 ()) in
        let n = draw tc (with_printer [%sexp_of: int] doubled) in
        assert (n >= 0)
    ]} *)
val with_printer
  :  ('a -> Sexplib0.Sexp.t)
  -> ('a, 'p) Generators.generator
  -> ('a, Generators.printable) Generators.generator

(** {3 Concurrency and parallelism}

    Hegel can drive generation from more than one thread or domain within a 
    single test. Two rules govern it.

    First, test-case handles may not be shared. A single handle must be drawn
    from by one thread at a time, so give each thread its own {e clone} using
    {!clone}. A clone has its own choice sequence. Drawing from one shared 
    handle on multiple threads throws a concurrent-use error. Concurrently 
    driving one shared collection, pool, or state machine will likely produce 
    flaky results, so always make a new one per unit of concurrency/parallelism.

    Second, a draw is a synchronous engine call that holds its domain's runtime
    lock and never yields, so it cannot cooperate with an event loop or overlap
    another draw on the same domain.

    As long as you follow these two rules and your code is deterministic, you
    will be able to replay failures. 

    Some advice for common concurrency/parallelism libraries:

    Use {b Threads} for interleaving of concurrent operations and overlapping 
    blocking work, not parallel generation, since draws serialize under the 
    runtime lock. You should use {!spawn} / {!join} rather than [Thread.create]. 
    [Thread.join] drops a worker's exception, whereas {!join} re-raises it into
    the runner.

    {[
      let%hegel_test concurrent_workers tc =
        let w = spawn tc (fun worker -> draw_silent worker gen) in
        let mine = draw_silent tc gen in
        ignore (mine, join w)
    ]}

    Use {b Domainslib} or any domain pool for when you need true parallelism, 
    such as higher generation throughput. We strongly recommend that you do not
    use domains directly, as they are expensive to create and destruct. Set up
    the pool once and reuse it. Clone up front then [Task.async] each clone and 
    [Task.await] it.

    {[
      (* the pool is created once and reused across cases *)
      let pool = Domainslib.Task.setup_pool ~num_domains:2 ()

      let%hegel_test parallel_generation tc =
        Domainslib.Task.run pool (fun () ->
          let worker = clone tc in
          let p = Domainslib.Task.async pool (fun () -> draw_silent worker gen) in
          let mine = draw_silent tc gen in
          ignore (mine, Domainslib.Task.await pool p))
    ]}

    Use {b Eio} for concurrent generation with structured concurrency or if your
    code already uses Eio. Each fiber should draw its own data from its own clone. 
    Since a draw does not yield, only separate domains make draws truly parallel. 
    Here two workers race increments onto a shared atomic. The property is that 
    no update is lost.

    {[
      Eio_main.run @@ fun env ->
      let dmgr = Eio.Stdenv.domain_mgr env in
      run_hegel_test (fun tc ->
        let counter = Atomic.make 0 in
        let amounts = integers ~min_value:0 ~max_value:100 () in
        let worker g () =
          Eio.Domain_manager.run dmgr (fun () ->
            let n = draw_silent g amounts in
            ignore (Atomic.fetch_and_add counter n : int);
            n)
        in
        let worker_b = clone tc in
        let sum_a, sum_b = Eio.Fiber.pair (worker tc) (worker worker_b) in
        require_equal tc [%sexp_of: int] (sum_a + sum_b) (Atomic.get counter))
    ]} *)

(** [clone tc] creates a clone of [tc], an independent stream of the same
    test case. A single [test_case] handle must not be drawn from concurrently,
    so give each thread its own clone.

    Because [Thread.join] drops a worker's exception, you must capture the 
    worker's result or its exception and re-raise it on the calling thread. 
    {!spawn} / {!join} wrap that pattern for you.

    {[
      let%hegel_test two_hands_two_dice_manual tc =
        let die = integers ~min_value:1 ~max_value:6 () in
        let other_hand = clone tc in
        let out = ref (Error (Failure "unset")) in
        let rolling =
          Thread.create
            (fun () -> out := (try Ok (draw_silent other_hand die) with e -> Error e))
            ()
        in
        let right_hand = draw_silent tc die in
        Thread.join rolling;
        match !out with
        | Ok left_hand -> assert (right_hand + left_hand >= 2)
        | Error e -> raise e
    ]} *)
val clone : test_case -> test_case

(** A running worker started by {!spawn} and awaited with {!join}. *)
type 'a worker

(** [spawn tc f] clones [tc] and runs [f clone] on a new thread. {!join} awaits
    it. The example below is functionally identical to the example for {!clone},
    but more ergonomic.

    {[
      let%hegel_test two_hands_two_dice tc =
        let die = integers ~min_value:1 ~max_value:6 () in
        let other_hand = spawn tc (fun worker -> draw_silent worker die) in
        let this_hand = draw_silent tc die in
        assert (this_hand + join other_hand >= 2)
    ]} *)
val spawn : test_case -> (test_case -> 'a) -> 'a worker

(** [join w] waits for worker [w] to finish and returns its result. It re-raises
    any exception [w] raised on the caller's thread. Join before the test body
    returns. *)
val join : 'a worker -> 'a
