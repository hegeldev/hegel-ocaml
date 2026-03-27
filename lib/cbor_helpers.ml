(** CBOR encoding/decoding helpers for Hegel.

    This module provides convenience functions for encoding and decoding CBOR
    values, plus type-safe extractor functions that produce clear error messages
    when a value has an unexpected type. *)

open! Core

type t = CBOR.Simple.t
(** The type of CBOR values, re-exported from [CBOR.Simple]. *)

(** [encode v] serializes the CBOR value [v] to a binary string. *)
let encode = CBOR.Simple.encode

(** [decode s] deserializes a CBOR value from the binary string [s]. *)
let decode = CBOR.Simple.decode

(** [type_name v] returns a human-readable name for the CBOR type of [v]. *)
let type_name = function
  | `Null -> "null"
  | `Undefined -> "undefined"
  | `Simple _ -> "simple"
  | `Bool _ -> "bool"
  | `Int _ -> "int"
  | `Float _ -> "float"
  | `Bytes _ -> "bytes"
  | `Text _ -> "text"
  | `Array _ -> "array"
  | `Map _ -> "map"
  | `Tag _ -> "tag"

(** [extract_int v] extracts an integer from a CBOR value.

    Raises [Failure] if [v] is not an [`Int]. *)
let extract_int = function
  | `Int i -> i
  | other -> failwith (sprintf "Expected CBOR int, got %s" (type_name other))

(** [extract_float v] extracts a float from a CBOR value.

    Raises [Failure] if [v] is not a [`Float]. *)
let extract_float = function
  | `Float f -> f
  | other -> failwith (sprintf "Expected CBOR float, got %s" (type_name other))

(** [extract_string v] extracts a text string from a CBOR value.

    Raises [Failure] if [v] is not a [`Text]. *)
let extract_string = function
  | `Text s -> s
  | other -> failwith (sprintf "Expected CBOR text, got %s" (type_name other))

(** [extract_bool v] extracts a boolean from a CBOR value.

    Raises [Failure] if [v] is not a [`Bool]. *)
let extract_bool = function
  | `Bool b -> b
  | other -> failwith (sprintf "Expected CBOR bool, got %s" (type_name other))

(** [extract_bytes v] extracts a byte string from a CBOR value.

    Raises [Failure] if [v] is not a [`Bytes]. *)
let extract_bytes = function
  | `Bytes b -> b
  | other -> failwith (sprintf "Expected CBOR bytes, got %s" (type_name other))

(** [extract_list v] extracts a list of CBOR values from a CBOR value.

    Raises [Failure] if [v] is not an [`Array]. *)
let extract_list = function
  | `Array l -> l
  | other -> failwith (sprintf "Expected CBOR array, got %s" (type_name other))

(** [extract_dict v] extracts a key-value map from a CBOR value. Keys and values
    remain as CBOR values.

    Raises [Failure] if [v] is not a [`Map]. *)
let extract_dict = function
  | `Map m -> m
  | other -> failwith (sprintf "Expected CBOR map, got %s" (type_name other))

(** [is_null v] returns [true] if [v] is [`Null]. *)
let is_null = function `Null -> true | _ -> false
