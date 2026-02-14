(** Generators for property-based testing values. *)

exception Assume_rejected

type 'a basic = {
  schema : Cbor.t;
  parse : Cbor.t -> 'a;
}

type 'a t = {
  generate : unit -> 'a;
  as_basic : unit -> 'a basic option;
}

(** {2 Primitives} *)

val unit : unit -> unit t
val bool : unit -> bool t
val just : 'a -> 'a t

(** {2 Numeric} *)

val int : ?min:int -> ?max:int -> unit -> int t
val int32 : ?min:int32 -> ?max:int32 -> unit -> int32 t
val int64 : ?min:int64 -> ?max:int64 -> unit -> int64 t
val float : ?min:float -> ?max:float -> ?allow_nan:bool -> ?allow_infinity:bool -> unit -> float t

(** {2 Strings} *)

val string : ?min_size:int -> ?max_size:int -> unit -> string t
val from_regex : ?fullmatch:bool -> string -> string t

(** {2 Binary} *)

val binary : ?min_size:int -> ?max_size:int -> unit -> string t

(** {2 Collections} *)

val list : ?min_size:int -> ?max_size:int -> 'a t -> 'a list t
val array : ?min_size:int -> ?max_size:int -> 'a t -> 'a array t

(** {2 Tuples} *)

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** {2 Combinators} *)

val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
val one_of : 'a t list -> 'a t
val optional : 'a t -> 'a option t
val sampled_from : 'a list -> 'a t

(** {2 Formats} *)

val email : unit -> string t
val url : unit -> string t
val domain : ?max_length:int -> unit -> string t
val ip_address : ?version:[ `V4 | `V6 ] -> unit -> string t
val date : unit -> string t
val time : unit -> string t
val datetime : unit -> string t

(** {2 Helpers for generated code} *)

val group : int -> (unit -> 'a) -> 'a
val cbor_array_value : Cbor.t -> Cbor.t list

(** {2 Low-level} *)

val generate_raw : Cbor.t -> Cbor.t
val start_span : int -> unit
val stop_span : bool -> unit
val note : string -> unit
val target : ?label:string -> float -> unit
val base64_decode : string -> string
val string_contains : string -> string -> bool
val cbor_int_value : Cbor.t -> int
val cbor_float_value : Cbor.t -> float
val cbor_text_value : Cbor.t -> string
val cbor_bool_value : Cbor.t -> bool

(** {2 Span labels} *)

module Labels : sig
  val list : int
  val list_element : int
  val set : int
  val set_element : int
  val map : int
  val map_entry : int
  val tuple : int
  val one_of : int
  val optional : int
  val fixed_dict : int
  val flat_map : int
  val filter : int
  val mapped : int
  val sampled_from : int
end
