(** The current version of Hegel for OCaml. *)
let version = "0.8.0"

(** Generator combinators for composable test data generation. *)
module Generators = Generators

(** Stateful property-based testing on top of {!Generators}. *)
module Stateful = Stateful

(** Runtime support called by [@@deriving hegel_generator]-generated code; not
    intended for direct use. *)
module Derive = Derive

(** Test runner and run-loop internals; re-exported (doc-hidden) for white-box
    tests, not for direct use. *)
module Internal = Internal

(** CBOR extractor helpers; re-exported (doc-hidden) for white-box tests. *)
module Cbor_helpers = Cbor_helpers

(** Antithesis integration; re-exported (doc-hidden) for white-box tests. *)
module Antithesis = Antithesis

(* Settings, test-case, and test-location types re-exported so the whole
   public API lives directly under Hegel. The module re-exports above are all
   doc-hidden in the mli: Derive is called by generated code, the rest are
   white-box surfaces for the test suite. *)

type test_case = Internal.test_case

type test_location = Antithesis.test_location =
  { function_name : string
  ; file : string
  ; begin_line : int
  }

type verbosity = Internal.verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

type database = Internal.database =
  | Unset
  | Disabled
  | Path of string

type mode = Internal.mode =
  | Test_run
  | Single_test_case

type phase = Internal.phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

type health_check = Internal.health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

type settings = Internal.settings =
  { mode : mode
  ; test_cases : int
  ; stateful_step_count : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
  ; suppress_health_check : health_check list
  ; phases : phase list option
  ; print_blob : bool
  ; report_multiple_failures : bool
  }

exception Assume_rejected = Internal.Assume_rejected

(** {2 Convenience re-exports} *)

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, defaulting to
    {!default_settings}. This is the entry point the [let%hegel_test] PPX
    targets. *)
let run_hegel_test = Internal.run_hegel_test

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume = Internal.assume

(** [note tc message] prints [message] to stderr subject to the run's verbosity:
    never under [Quiet], only on the final (failing) replay under [Normal], and
    on every test case under [Verbose] or [Debug]. *)
let note = Internal.note

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
let target = Internal.target

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen]. On the final replay of a failing test, an outermost draw prints its
    value. See {!Generators.draw}. *)
let draw = Generators.draw

(** [draw_named ~label ~repeatable tc gen] is the naming-aware draw the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw}). See {!Generators.draw_named}. *)
let draw_named = Generators.draw_named

(** [draw_silent tc gen] is {!draw} without printing the value on the final
    replay, and accepts a generator with no printer. *)
let draw_silent = Generators.draw_silent

(** [with_printer sexp_of gen] attaches [sexp_of] so [gen] can be drawn with
    {!draw}. See {!Generators.with_printer}. *)
let with_printer = Generators.with_printer

(** [default_settings ()] creates default test settings with CI auto-detection.
*)
let default_settings = Internal.default_settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)
let settings = Internal.settings

let with_test_cases = Internal.with_test_cases
let with_stateful_step_count = Internal.with_stateful_step_count
let with_verbosity = Internal.with_verbosity
let with_seed = Internal.with_seed
let with_derandomize = Internal.with_derandomize
let with_database = Internal.with_database
let with_suppress_health_check = Internal.with_suppress_health_check
let with_phases = Internal.with_phases
let with_mode = Internal.with_mode
let with_print_blob = Internal.with_print_blob
let with_report_multiple_failures = Internal.with_report_multiple_failures
