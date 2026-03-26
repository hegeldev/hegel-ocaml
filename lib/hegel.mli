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

val assume : bool -> unit
(** [assume condition] rejects the current test case if [condition] is [false].
*)

val note : string -> unit
(** [note message] records a message that will be printed on the final (failing)
    run. *)

val target : float -> string -> unit
(** [target value label] sends a target command to guide the search engine
    toward higher values. *)

val draw : 'a Generators.generator -> 'a
(** [draw gen] produces a typed value from generator [gen]. Must be called from
    within a Hegel test body. *)

val run_hegel_test :
  ?settings:Client.settings ->
  ?test_cases:int ->
  ?seed:int ->
  (unit -> unit) ->
  unit
(** [run_hegel_test ?settings ?test_cases ?seed test_fn] runs a property test.
*)

val default_settings : unit -> Client.settings
(** [default_settings ()] creates default test settings with CI auto-detection.
*)
