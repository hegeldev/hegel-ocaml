(** CBOR encoding/decoding helpers for the Hegel SDK.

    This module provides convenience functions for encoding and decoding CBOR
    values, plus type-safe extractor functions that produce clear error messages
    when a value has an unexpected type. *)

type t = CBOR.Simple.t
(** The type of CBOR values, re-exported from [CBOR.Simple]. *)

val encode : t -> string
(** [encode v] serializes the CBOR value [v] to a binary string. *)

val decode : string -> t
(** [decode s] deserializes a CBOR value from the binary string [s]. *)

val extract_int : t -> int
(** [extract_int v] extracts an integer from a CBOR value.

    Raises [Failure] if [v] is not an [`Int]. *)

val extract_float : t -> float
(** [extract_float v] extracts a float from a CBOR value.

    Raises [Failure] if [v] is not a [`Float]. *)

val extract_string : t -> string
(** [extract_string v] extracts a text string from a CBOR value.

    Raises [Failure] if [v] is not a [`Text]. *)

val extract_bool : t -> bool
(** [extract_bool v] extracts a boolean from a CBOR value.

    Raises [Failure] if [v] is not a [`Bool]. *)

val extract_bytes : t -> string
(** [extract_bytes v] extracts a byte string from a CBOR value.

    Raises [Failure] if [v] is not a [`Bytes]. *)

val extract_list : t -> t list
(** [extract_list v] extracts a list of CBOR values from a CBOR value.

    Raises [Failure] if [v] is not an [`Array]. *)

val extract_dict : t -> (t * t) list
(** [extract_dict v] extracts a key-value map from a CBOR value. Keys and values
    remain as CBOR values.

    Raises [Failure] if [v] is not a [`Map]. *)

val type_name : t -> string
(** [type_name v] returns a human-readable name for the CBOR type of [v]. *)

val is_null : t -> bool
(** [is_null v] returns [true] if [v] is [`Null]. *)
