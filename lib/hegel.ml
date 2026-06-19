(** The current version of Hegel for OCaml. *)
let version = "0.6.3"

(** CBOR encoding/decoding with type-safe extractor helpers. *)
module Cbor_helpers = Cbor_helpers

(** Test runner and lifecycle management. *)
module Client = Client

(** Generator combinators for composable test data generation. *)
module Generators = Generators

(** Runtime support for [@@deriving hegel_generator]. *)
module Derive = Derive

(** Stateful property-based testing on top of {!Generators}. *)
module Stateful = Stateful

(** Antithesis integration. *)
module Antithesis = Antithesis

(** {2 Convenience re-exports} *)

(** [run_hegel_test ?settings ?test_location test_fn] runs a property test
    against the native engine, defaulting to {!default_settings}. This is the
    entry point the [let%hegel_test] PPX targets. *)
let run_hegel_test = Client.run_hegel_test

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
let assume = Client.assume

(** [note tc message] prints [message] to stderr subject to the run's verbosity:
    never under [Quiet], only on the final (failing) replay under [Normal], and
    on every test case under [Verbose] or [Debug]. *)
let note = Client.note

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
let target = Client.target

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
let default_settings = Client.default_settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)
let settings = Client.settings
