(** The current version of Hegel for OCaml. *)
let version = "0.5.2"

(** CBOR encoding/decoding with type-safe extractor helpers. *)
module Cbor_helpers = Cbor_helpers

(** Test runner and lifecycle management. *)
module Client = Client

(** Generator combinators for composable test data generation. *)
module Generators = Generators

(** Runtime support for [@@deriving generator]. *)
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

(** [note tc message] records a message that will be printed on the final
    (failing) run. *)
let note = Client.note

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
let target = Client.target

(** [draw tc gen] produces a typed value from generator [gen] using test case
    [tc]. *)
let draw = Generators.draw

(** [default_settings ()] creates default test settings with CI auto-detection.
*)
let default_settings = Client.default_settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)
let settings = Client.settings
