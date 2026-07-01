(** Property-based testing for OCaml, powered by the native Hegel engine.

    All code examples in this documentation assume [open Hegel] and
    [open Hegel.Generators] and that [ppx_hegel_test] is used.

    Write a property as a [let%hegel_test], drawing inputs with {!draw} from the
    combinators in {!Generators}. Hegel runs the body on many generated inputs
    and, on failure, shrinks to a minimal counterexample and prints each drawn
    value (named after the [let] binding it was bound to).

    {[
      let%hegel_test addition_commutes tc =
        let a = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
        let b = draw tc (integers ~min_value:(-1000) ~max_value:1000 ()) in
        assert (a + b = b + a)
      ;;
    ]}

    Run the suite with [dune runtest]. {!run_hegel_test} is the entry point the
    PPX targets (and may be called directly); see {!Generators} for the
    generator combinators and {!Stateful} for state-machine testing. *)

(** The current version of Hegel for OCaml. *)
val version : string

(** Generator combinators for composable test data generation. *)
module Generators = Generators

(** Stateful property-based testing on top of {!Generators}. *)
module Stateful = Stateful

(**/**)

(* Internal modules and handles: accessible (used by generated code and tests)
   but excluded from the generated documentation. *)

module Client = Client
module Cbor_helpers = Cbor_helpers
module Derive = Derive
module Antithesis = Antithesis

type test_case = Client.test_case

(**/**)

(** {2 Settings}

    Build a {!type:settings} value with {!default_settings} or {!val:settings},
    refine it with the [with_*] functions, and pass it to {!run_hegel_test} — or
    attach it to a [let%hegel_test] with the [\[@@settings ...\]] attribute:

    {[
      let%hegel_test many_cases tc =
        let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
        assert (n < 100)
      [@@settings settings ~test_cases:500 () |> with_verbosity Verbose]
      ;;
    ]} *)

(** Configuration for a test run. Build one with {!default_settings} or
    {!val:settings} and refine it with the [with_*] functions below. *)
type settings = Client.settings

(** How much output Hegel produces during a run. *)
type verbosity = Client.verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** Where Hegel stores and replays failing examples. *)
type database = Client.database =
  | Unset
  | Disabled
  | Path of string

(** How a test run is executed. *)
type mode = Client.mode =
  | Test_run (** the default: many cases, shrinking, database replay *)
  | Single_test_case (** run the body once, with no shrinking or replay *)

(** Phases of a test run that can be enabled or disabled with {!with_phases}. *)
type phase = Client.phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

(** Health checks that can be suppressed with {!with_suppress_health_check}. *)
type health_check = Client.health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(** [default_settings ()] creates default test settings, auto-detecting CI: in
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

(** [with_stateful_step_count n s] sets the maximum number of steps per stateful
    test (see {!Stateful}). *)
val with_stateful_step_count : int -> settings -> settings

(** [with_verbosity v s] sets how much output the run produces.

    {[
      let s = default_settings () |> with_verbosity Verbose
    ]} *)
val with_verbosity : verbosity -> settings -> settings

(** [with_seed seed s] pins the run's random seed. *)
val with_seed : int option -> settings -> settings

(** [with_derandomize b s] makes the run reproducible by deriving its seed from
    the test's identity instead of fresh randomness. *)
val with_derandomize : bool -> settings -> settings

(** [with_database db s] sets where failing examples are persisted and replayed.

    {[
      let s = default_settings () |> with_database (Path "_hegel_db")
    ]} *)
val with_database : database -> settings -> settings

(** [with_suppress_health_check checks s] disables the given health checks. *)
val with_suppress_health_check : health_check list -> settings -> settings

(** [with_phases phases s] restricts the run to the given phases.

    {[
      let s = default_settings () |> with_phases [ Generate; Shrink ]
    ]} *)
val with_phases : phase list -> settings -> settings

(** [with_mode mode s] sets the execution mode. *)
val with_mode : mode -> settings -> settings

(** [with_print_blob b s] makes a failing run print a replay blob (and replay
    runs report which blobs reproduced the failure). *)
val with_print_blob : bool -> settings -> settings

(** [with_report_multiple_failures b s] makes a failing run report every distinct
    failure it found rather than just the first. *)
val with_report_multiple_failures : bool -> settings -> settings

(** {2 Running tests} *)

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, defaulting to
    {!default_settings}. This is the entry point the [let%hegel_test] PPX targets;
    it can also be called directly, e.g. to drive a property from a plain
    executable or another test harness:

    {[
      let () =
        run_hegel_test (fun tc ->
          let n = draw tc (integers ~min_value:0 ~max_value:9 ()) in
          assert (n >= 0 && n <= 9))
    ]}

    @param database_key
      overrides the key scoping this test's persisted corpus and [derandomize]
      seed; defaults to the test's [test_location] so each [let%hegel_test] is
      scoped by its own identity. *)
val run_hegel_test
  :  ?settings:settings
  -> ?test_location:Antithesis.test_location
  -> ?database_key:string
  -> ?failure_blobs:string list
  -> (test_case -> unit)
  -> unit

(** Raised by {!assume} when its condition is [false] (rejecting the current test
    case). The runner catches it, so you rarely handle it directly. *)
exception Assume_rejected

(** [assume tc condition] rejects the current test case if [condition] is
    [false].

    {[
      let%hegel_test only_even tc =
        let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
        assume tc (n mod 2 = 0);
        assert (n mod 2 = 0)
      ;;
    ]} *)
val assume : test_case -> bool -> unit

(** [note tc message] prints [message] to stderr subject to the run's verbosity:
    never under [Quiet], only on the final (failing) replay under [Normal], and
    on every test case under [Verbose] or [Debug].

    {[
      let%hegel_test note_value tc =
        let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
        note tc (Printf.sprintf "n = %d" n);
        assert (n < 100)
      ;;
    ]} *)
val note : test_case -> string -> unit

(** [target tc value label] sends a target command to guide the search engine
    toward higher values.

    {[
      let%hegel_test grow_size tc =
        let v = draw tc (integers ~min_value:0 ~max_value:1000 ()) in
        target tc (float_of_int v) "size";
        assert (v <= 1000)
      ;;
    ]} *)
val target : test_case -> float -> string -> unit

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc]. On the final replay of a failing test (or on
    every case under verbose output), an outermost draw prints its value as
    [name = value], where [name] is [label] (else ["draw"]); an unlabeled draw is
    numbered ([draw_1], [draw_2], …) while a [label] is printed bare. See
    {!Generators.draw}. *)
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

(** [draw_silent tc gen] is {!draw} without printing the value on the final
    replay, and accepts a generator with no printer.

    {[
      let%hegel_test parity tc =
        let bit =
          draw_silent tc (map (fun n -> n mod 2) (integers ~min_value:0 ~max_value:99 ()))
        in
        assert (bit = 0 || bit = 1)
      ;;
    ]} *)
val draw_silent : test_case -> ('a, 'p) Generators.generator -> 'a

(** [with_printer sexp_of gen] attaches [sexp_of] as [gen]'s printer so it can
    be drawn with {!draw}. See {!Generators.with_printer}.

    {[
      let%hegel_test parity_printed tc =
        let bit =
          draw tc (with_printer [%sexp_of: int] (map (fun n -> n mod 2) (integers ~min_value:0 ~max_value:99 ())))
        in
        assert (bit = 0 || bit = 1)
      ;;
    ]} *)
val with_printer
  :  ('a -> Core.Sexp.t)
  -> ('a, 'p) Generators.generator
  -> ('a, Generators.printable) Generators.generator
