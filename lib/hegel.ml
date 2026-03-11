(** The current version of the Hegel OCaml SDK. *)
let version = "0.1.2"

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

(** [assume condition] rejects the current test case if [condition] is [false].
*)
let assume = Client.assume

(** [note message] records a message that will be printed on the final (failing)
    run. *)
let note = Client.note

(** [target value label] sends a target command to guide the search engine
    toward higher values. *)
let target = Client.target

(** [draw gen] produces a typed value from generator [gen]. Must be called from
    within a Hegel test body. *)
let draw = Generators.draw

(** [run_hegel_test ?test_cases ?seed test_fn] runs a property test. *)
let run_hegel_test = Session.run_hegel_test
