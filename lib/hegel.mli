val version : string
(** The current version of Hegel for OCaml. *)

module Protocol = Protocol
(** Binary wire protocol for packet serialization and deserialization. *)

module Cbor_helpers = Cbor_helpers
(** CBOR encoding/decoding with type-safe extractor helpers. *)

module Connection = Connection
(** Multiplexed connection and channel abstractions. *)

module Client = Client
(** Test runner and lifecycle management. *)

module Session = Session
(** Global session management for running property tests. *)

module Generators = Generators
(** Generator combinators for composable test data generation. *)

module Conformance = Conformance
(** Conformance test helpers for writing conformance binaries. *)

module Derive = Derive
(** Runtime support for [@@deriving generator]. *)

(** {2 Convenience re-exports} *)

val assume : Client.test_case -> bool -> unit
(** [assume tc condition] rejects the current test case if [condition] is
    [false]. *)

val note : Client.test_case -> string -> unit
(** [note tc message] records a message that will be printed on the final
    (failing) run. *)

val target : Client.test_case -> float -> string -> unit
(** [target tc value label] sends a target command to guide the search engine
    toward higher values. *)

val draw : Client.test_case -> 'a Generators.generator -> 'a
(** [draw tc gen] produces a typed value from generator [gen] using test case
    [tc]. *)

val run_hegel_test :
  ?settings:Client.settings ->
  ?test_cases:int ->
  ?seed:int ->
  (Client.test_case -> unit) ->
  unit
(** [run_hegel_test ?settings ?test_cases ?seed test_fn] runs a property test.
*)

val default_settings : unit -> Client.settings
(** [default_settings ()] creates default test settings with CI auto-detection.
*)
