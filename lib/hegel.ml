(** The current version of Hegel for OCaml. *)
let version = "0.20.1"

(** Generators for composable test data generation. *)
module Generators = Generators

(** Stateful property-based testing on top of {!Generators}. *)
module Stateful = Stateful

module Settings = Settings

(** Auxiliary submodule for [@@deriving hegel_generator]. Included
    below so [open Hegel] alone makes derived code resolve. *)
module Derive = Derive

include Derive

(** Test runner and run-loop internals; re-exported (doc-hidden) for white-box
    tests, not for direct use. *)
module Internal = Internal

(** Antithesis integration; re-exported (doc-hidden) for white-box tests. *)
module Antithesis = Antithesis

type ('a, 'p) generator = ('a, 'p) Generators.generator
type printable = Generators.printable
type unprintable = Generators.unprintable

type date = Generators.date =
  { year : int
  ; month : int
  ; day : int
  }

type time = Generators.time =
  { hour : int
  ; minute : int
  ; second : int
  ; nanosecond : int
  }

let booleans = Generators.booleans
let integers = Generators.integers
let floats = Generators.floats
let text = Generators.text
let characters = Generators.characters
let make_characters = Generators.make_characters
let chars = Generators.chars
let binary = Generators.binary
let just = Generators.just
let lists = Generators.lists
let assoc_lists = Generators.assoc_lists
let make_hash_tables = Generators.make_hash_tables
let hash_tables = Generators.hash_tables
let sampled_from = Generators.sampled_from
let one_of = Generators.one_of
let optional = Generators.optional
let tuples2 = Generators.tuples2
let tuples3 = Generators.tuples3
let tuples4 = Generators.tuples4
let functions = Generators.functions
let functions2 = Generators.functions2
let functions3 = Generators.functions3
let emails = Generators.emails
let urls = Generators.urls
let domains = Generators.domains
let make_dates = Generators.make_dates
let make_times = Generators.make_times
let make_datetimes = Generators.make_datetimes
let dates = Generators.dates
let times = Generators.times
let datetimes = Generators.datetimes
let ip_addresses = Generators.ip_addresses
let from_regex = Generators.from_regex
let composite = Generators.composite
let map = Generators.map
let flat_map = Generators.flat_map
let filter = Generators.filter

(* Test-case and test-location types re-exported so the whole
   public API lives directly under Hegel. The module re-exports above are
   doc-hidden in the mli: white-box surfaces for the test suite. *)

type test_case = Internal.test_case

type test_location = Antithesis.test_location =
  { function_name : string
  ; file : string
  ; begin_line : int
  }

exception Assume_rejected = Internal.Assume_rejected
exception Usage_error = Hegel_ffi.Ffi.Usage_error

(** {2 Convenience re-exports} *)

(** [run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn]
    runs a property test against the native engine, defaulting to
    [Settings.default ()]. The [let%hegel_test] PPX runs tests through the
    equivalent {!run_hegel_test_ppx}. *)
let run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn =
  Internal.run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn
;;

(** [run_hegel_test_ppx] is {!run_hegel_test} with the PPX replay hint enabled;
    the [let%hegel_test] PPX targets it. Not for direct use. *)
let run_hegel_test_ppx ?settings ?test_location ?database_key ?failure_blobs test_fn =
  Internal.run_hegel_test
    ?settings
    ?test_location
    ~from_ppx:true
    ?database_key
    ?failure_blobs
    test_fn
;;

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume = Internal.assume

(** [note tc message] prints [message] to stderr subject to the run's verbosity:
    never under [Quiet], only on the final (failing) replay under [Normal], and
    on every test case under [Verbose] or [Debug]. *)
let note = Internal.note

(** [require tc ?msg condition] fails the current test case when [condition] is
    [false]. See {!Internal.require}. *)
let require = Internal.require

(** [require_equal tc ?msg sexp_of lhs rhs] fails the current test case when the
    two values render to different sexps, printing a sexp diff in the failure
    report. See {!Internal.require_equal}. *)
let require_equal = Internal.require_equal

(** [target tc ~label ~value] sends a target command to guide the search engine
    toward higher values. *)
let target = Internal.target

(** [event tc ~label] records [label] as observed on this test case for the
    end-of-run statistics report. *)
let event = Internal.event

(** [event_value tc ~label ~value] records the finite observation [value] under
    [label] for the end-of-run statistics report. *)
let event_value = Internal.event_value

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

(** [draw_silent_named ~name tc gen] is the naming-aware {!draw_silent} the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw_silent}). See {!Generators.draw_silent_named}. *)
let draw_silent_named = Generators.draw_silent_named

(** [clone tc] forks an independent clone of [tc] for driving generation from
    another thread; its native resources are freed by a GC finaliser once the
    clone is unreachable. See {!Internal.clone}. *)
let clone = Internal.clone

type 'a worker = 'a Internal.worker

(** [spawn tc f] runs [f] on a fresh clone of [tc] on a new thread. See
    {!Internal.spawn}. *)
let spawn = Internal.spawn

(** [join w] waits for worker [w] and returns its result, re-raising any
    exception the worker raised. See {!Internal.join}. *)
let join = Internal.join

(** [with_printer sexp_of gen] attaches [sexp_of] so [gen] can be drawn with
    {!draw}. See {!Generators.with_printer}. *)
let with_printer = Generators.with_printer
