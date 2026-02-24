(** Hegel property-based testing SDK for OCaml.

    This module provides the client SDK for communicating with the Hegel server
    (hegeld, powered by Hypothesis) via Unix sockets using a binary protocol,
    enabling property-based testing from OCaml. *)

(** The current version of the Hegel OCaml SDK. *)
let version = "0.1.0"

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
