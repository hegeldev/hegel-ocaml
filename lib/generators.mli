(** Generator combinators for the Hegel SDK.

    This module provides a composable generator API for property-based testing.
    Generators produce typed OCaml values and can be combined using {!map},
    {!flat_map}, and {!filter}. *)

(** Constants for span labels used in generation tracking. *)
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
  val enum_variant : int
end

(** The type of generators. Generators produce typed OCaml values and can be
    combined using {!map}, {!flat_map}, and {!filter}. *)
type 'a generator

(** Maximum number of filter attempts before calling [assume false]. *)
val max_filter_attempts : int

(** [group label f] runs [f ()] inside a span with the given [label]. The span
    is stopped with [discard:false] regardless of whether [f] raises. *)
val group : int -> (unit -> 'a) -> 'a

(** [discardable_group label f] runs [f ()] inside a span with [label]. If [f]
    raises, the span is stopped with [discard:true]; otherwise [discard:false]. *)
val discardable_group : int -> (unit -> 'a) -> 'a

(** A collection handle for generating variable-length sequences. *)
type collection = {
  mutable finished : bool;
  mutable server_name : CBOR.Simple.t option;
  min_size : int;
  max_size : int option;
}

(** [new_collection ~min_size ?max_size ()] creates a new collection handle. *)
val new_collection : min_size:int -> ?max_size:int -> unit -> collection

(** [collection_more coll] returns [true] if more elements should be generated,
    [false] when the collection is complete. *)
val collection_more : collection -> bool

(** [collection_reject coll] rejects the last element of the collection. *)
val collection_reject : collection -> unit

(** [generate gen] produces a typed value from generator [gen]. *)
val generate : 'a generator -> 'a

(** [map f gen] transforms values from [gen] using [f].

    When [gen] is a [Basic] generator, the schema is preserved and transforms
    are composed. Otherwise, a [Mapped] generator is created. *)
val map : ('a -> 'b) -> 'a generator -> 'b generator

(** [flat_map f gen] creates a dependent generator. The function [f] receives
    the generated value and returns a new generator whose value is the final
    result. *)
val flat_map : ('a -> 'b generator) -> 'a generator -> 'b generator

(** [filter predicate gen] filters values from [gen] using [predicate]. Tries
    multiple times; calls [assume false] if all attempts fail. *)
val filter : ('a -> bool) -> 'a generator -> 'a generator

(** [schema gen] returns the schema for a [Basic] generator, or [None]. *)
val schema : 'a generator -> CBOR.Simple.t option

(** [is_basic gen] returns [true] if [gen] is a [Basic] generator. *)
val is_basic : 'a generator -> bool

(** [as_basic gen] returns [Some (schema, transform)] if [gen] is [Basic], or
    [None] otherwise. *)
val as_basic : 'a generator -> (CBOR.Simple.t * (CBOR.Simple.t -> 'a)) option

(** {2 Primitive generators} *)

(** [booleans ()] creates a generator for boolean values. *)
val booleans : unit -> bool generator

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
val integers : ?min_value:int -> ?max_value:int -> unit -> int generator

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
    ?allow_infinity ()] creates a generator for floating-point values. *)
val floats :
  ?min_value:float ->
  ?max_value:float ->
  ?exclude_min:bool ->
  ?exclude_max:bool ->
  ?allow_nan:bool ->
  ?allow_infinity:bool ->
  unit ->
  float generator

(** [text ?min_size ?max_size ()] creates a generator for Unicode text strings. *)
val text : ?min_size:int -> ?max_size:int -> unit -> string generator

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings. *)
val binary : ?min_size:int -> ?max_size:int -> unit -> string generator

(** [just value] creates a generator that always produces [value]. *)
val just : 'a -> 'a generator

(** {2 Collection generators} *)

(** [lists elements ?min_size ?max_size ()] creates a generator for lists. *)
val lists : 'a generator -> ?min_size:int -> ?max_size:int -> unit -> 'a list generator

(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps). [keys] and [values] must be basic generators. *)
val hashmaps :
  'a generator ->
  'b generator ->
  ?min_size:int ->
  ?max_size:int ->
  unit ->
  ('a * 'b) list generator

(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values. *)
val sampled_from : 'a list -> 'a generator

(** [one_of generators] creates a generator that picks from one of the given
    [generators]. Requires at least 2 generators. *)
val one_of : 'a generator list -> 'a generator

(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element]. *)
val optional : 'a generator -> 'a option generator

(** {2 Tuple generators} *)

(** [tuples2 g1 g2] creates a generator for 2-element tuples. *)
val tuples2 : 'a generator -> 'b generator -> ('a * 'b) generator

(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples. *)
val tuples3 : 'a generator -> 'b generator -> 'c generator -> ('a * 'b * 'c) generator

(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples. *)
val tuples4 :
  'a generator ->
  'b generator ->
  'c generator ->
  'd generator ->
  ('a * 'b * 'c * 'd) generator

(** {2 Format generators} *)

(** [emails ()] creates a generator for valid email address strings. *)
val emails : unit -> string generator

(** [urls ()] creates a generator for valid URL strings. *)
val urls : unit -> string generator

(** [domains ?max_length ()] creates a generator for domain name strings. *)
val domains : ?max_length:int -> unit -> string generator

(** [dates ()] creates a generator for ISO 8601 date strings (YYYY-MM-DD). *)
val dates : unit -> string generator

(** [times ()] creates a generator for time strings. *)
val times : unit -> string generator

(** [datetimes ()] creates a generator for ISO 8601 datetime strings. *)
val datetimes : unit -> string generator

(** [ip_addresses ?version ()] creates a generator for IP address strings. *)
val ip_addresses : ?version:int -> unit -> string generator

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern]. *)
val from_regex : string -> ?fullmatch:bool -> unit -> string generator
