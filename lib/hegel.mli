(** The current version of Hegel for OCaml. *)
val version : string

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
val run_hegel_test
  :  ?settings:Client.settings
  -> ?test_location:Antithesis.test_location
  -> (Client.test_case -> unit)
  -> unit

(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)
val assume : Client.test_case -> bool -> unit

(** [note tc message] records a message that will be printed on the final
    (failing) run. *)
val note : Client.test_case -> string -> unit

(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)
val target : Client.test_case -> float -> string -> unit

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc]. On the final replay of a failing test, an
    outermost draw prints its value (as [label = value], or just the value).
    See {!Generators.draw}. *)
val draw
  :  ?label:string
  -> Client.test_case
  -> ('a, Generators.printable) Generators.generator
  -> 'a

(** [draw_silent tc gen] is {!draw} without printing the value on the final
    replay, and accepts a generator with no printer. *)
val draw_silent : Client.test_case -> ('a, 'p) Generators.generator -> 'a

(** [with_printer sexp_of gen] attaches [sexp_of] as [gen]'s printer so it can
    be drawn with {!draw}. See {!Generators.with_printer}. *)
val with_printer
  :  ('a -> Core.Sexp.t)
  -> ('a, 'p) Generators.generator
  -> ('a, Generators.printable) Generators.generator

(** [default_settings ()] creates default test settings with CI auto-detection.
*)
val default_settings : unit -> Client.settings

(** [settings ?test_cases ?seed ()] creates settings with the given overrides
    applied to {!default_settings}. Convenience constructor for common cases. *)
val settings : ?test_cases:int -> ?seed:int -> unit -> Client.settings
