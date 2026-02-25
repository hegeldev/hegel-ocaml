(** Hegel property-based testing SDK for OCaml.

    This module provides the client SDK for communicating with the Hegel server
    (hegeld, powered by Hypothesis) via Unix sockets using a binary protocol,
    enabling property-based testing from OCaml. *)

(** The current version of the Hegel OCaml SDK. *)
val version : string

(** Binary wire protocol for packet serialization and deserialization. *)
module Protocol = Protocol

(** CBOR encoding/decoding with type-safe extractor helpers. *)
module Cbor_helpers = Cbor_helpers

(** Multiplexed connection and channel abstractions. *)
module Connection = Connection

(** Test runner and lifecycle management. *)
module Client = Client

(** Global session management for running property tests. *)
module Session = Session

(** Generator combinators for composable test data generation. *)
module Generators = Generators

(** Conformance test helpers for writing conformance binaries. *)
module Conformance = Conformance

(** Runtime support for [@@deriving generator]. *)
module Derive = Derive

(** {2 Convenience re-exports} *)

(** [assume condition] rejects the current test case if [condition] is [false]. *)
val assume : bool -> unit

(** [note message] records a message that will be printed on the final (failing)
    run. *)
val note : string -> unit

(** [target value label] sends a target command to guide the search engine
    toward higher values. *)
val target : float -> string -> unit

(** [generate gen] produces a typed value from generator [gen]. *)
val generate : 'a Generators.generator -> 'a

(** [run_hegel_test ?test_cases ?name test_fn] runs a property test using the
    shared hegeld process. *)
val run_hegel_test : ?test_cases:int -> ?name:string -> (unit -> unit) -> unit
