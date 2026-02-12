type t =
  | Unsigned of int
  | Negative of int
  | Text of string
  | Bytes of string
  | Array of t list
  | Map of (t * t) list
  | Bool of bool
  | Null
  | Float of float
  | Tag of int * t

val encode : Buffer.t -> t -> unit
val encode_to_string : t -> string
val decode : string -> int -> t * int
val decode_string : string -> t

(** Convenience helpers *)

val map_get : t -> string -> t option
val map_get_exn : t -> string -> t
val as_text : t -> string option
val as_int : t -> int option
val as_bool : t -> bool option
val as_float : t -> float option
val as_array : t -> t list option
val as_map : t -> (t * t) list option
val to_diagnostic : t -> string
