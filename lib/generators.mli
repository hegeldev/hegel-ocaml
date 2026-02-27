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

type 'a generator =
  | Basic : {
      schema : CBOR.Simple.t;
      transform : CBOR.Simple.t -> 'a;
    }
      -> 'a generator
  | Mapped : { source : 'b generator; f : 'b -> 'a } -> 'a generator
  | FlatMapped : {
      source : 'b generator;
      f : 'b -> 'a generator;
    }
      -> 'a generator
  | Filtered : { source : 'a generator; predicate : 'a -> bool } -> 'a generator
  | CompositeList : {
      elements : 'a generator;
      min_size : int;
      max_size : int option;
    }
      -> 'a list generator
  | Composite : { label : int; generate_fn : unit -> 'a } -> 'a generator
      (** The type of generators. Generators produce typed OCaml values and can
          be combined using {!map}, {!flat_map}, and {!filter}.

          - [Basic] generators hold a raw schema and a mandatory client-side
            transform. Calling {!map} on a [Basic] generator preserves the
            schema and composes transforms.
          - [Mapped] generators wrap a source generator and a transform
            function.
          - [FlatMapped] generators wrap a source and a function returning a
            generator.
          - [Filtered] generators wrap a source and a predicate.
          - [CompositeList] generators use the collection protocol to generate
            lists of non-basic elements, creating a fresh collection per
            generate call.
          - [Composite] generators wrap a [generate_fn] thunk inside a span with
            the given [label]. Used for tuples and one_of with non-basic
            elements. *)

val max_filter_attempts : int
(** Maximum number of filter attempts before calling [assume false]. *)

val group : int -> (unit -> 'a) -> 'a
(** [group label f] runs [f ()] inside a span with the given [label]. The span
    is stopped with [discard:false] regardless of whether [f] raises. *)

val discardable_group : int -> (unit -> 'a) -> 'a
(** [discardable_group label f] runs [f ()] inside a span with [label]. If [f]
    raises, the span is stopped with [discard:true]; otherwise [discard:false].
*)

type collection = {
  mutable finished : bool;
  mutable server_name : CBOR.Simple.t option;
  min_size : int;
  max_size : int option;
}
(** A collection handle for generating variable-length sequences. *)

val new_collection : min_size:int -> ?max_size:int -> unit -> collection
(** [new_collection ~min_size ?max_size ()] creates a new collection handle. *)

val collection_more : collection -> bool
(** [collection_more coll] returns [true] if more elements should be generated,
    [false] when the collection is complete. *)

val collection_reject : collection -> unit
(** [collection_reject coll] rejects the last element of the collection. *)

val generate : 'a generator -> 'a
(** [generate gen] produces a typed value from generator [gen]. *)

val map : ('a -> 'b) -> 'a generator -> 'b generator
(** [map f gen] transforms values from [gen] using [f].

    When [gen] is a [Basic] generator, the schema is preserved and transforms
    are composed. Otherwise, a [Mapped] generator is created. *)

val flat_map : ('a -> 'b generator) -> 'a generator -> 'b generator
(** [flat_map f gen] creates a dependent generator. The function [f] receives
    the generated value and returns a new generator whose value is the final
    result. *)

val filter : ('a -> bool) -> 'a generator -> 'a generator
(** [filter predicate gen] filters values from [gen] using [predicate]. Tries
    multiple times; calls [assume false] if all attempts fail. *)

val schema : 'a generator -> CBOR.Simple.t option
(** [schema gen] returns the schema for a [Basic] generator, or [None]. *)

val is_basic : 'a generator -> bool
(** [is_basic gen] returns [true] if [gen] is a [Basic] generator. *)

val as_basic : 'a generator -> (CBOR.Simple.t * (CBOR.Simple.t -> 'a)) option
(** [as_basic gen] returns [Some (schema, transform)] if [gen] is [Basic], or
    [None] otherwise. *)

(** {2 Primitive generators} *)

val booleans : unit -> bool generator
(** [booleans ()] creates a generator for boolean values. *)

val integers : ?min_value:int -> ?max_value:int -> unit -> int generator
(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)

val floats :
  ?min_value:float ->
  ?max_value:float ->
  ?exclude_min:bool ->
  ?exclude_max:bool ->
  ?allow_nan:bool ->
  ?allow_infinity:bool ->
  unit ->
  float generator
(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values. *)

val text : ?min_size:int -> ?max_size:int -> unit -> string generator
(** [text ?min_size ?max_size ()] creates a generator for Unicode text strings.
*)

val binary : ?min_size:int -> ?max_size:int -> unit -> string generator
(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)

val just : 'a -> 'a generator
(** [just value] creates a generator that always produces [value]. *)

(** {2 Collection generators} *)

val lists :
  'a generator -> ?min_size:int -> ?max_size:int -> unit -> 'a list generator
(** [lists elements ?min_size ?max_size ()] creates a generator for lists. *)

val hashmaps :
  'a generator ->
  'b generator ->
  ?min_size:int ->
  ?max_size:int ->
  unit ->
  ('a * 'b) list generator
(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps). [keys] and [values] must be basic generators. *)

val sampled_from : 'a list -> 'a generator
(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values. *)

val one_of : 'a generator list -> 'a generator
(** [one_of generators] creates a generator that picks from one of the given
    [generators]. Requires at least 2 generators. *)

val optional : 'a generator -> 'a option generator
(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element]. *)

(** {2 Tuple generators} *)

val tuples2 : 'a generator -> 'b generator -> ('a * 'b) generator
(** [tuples2 g1 g2] creates a generator for 2-element tuples. *)

val tuples3 :
  'a generator -> 'b generator -> 'c generator -> ('a * 'b * 'c) generator
(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples. *)

val tuples4 :
  'a generator ->
  'b generator ->
  'c generator ->
  'd generator ->
  ('a * 'b * 'c * 'd) generator
(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples. *)

(** {2 Format generators} *)

val emails : unit -> string generator
(** [emails ()] creates a generator for valid email address strings. *)

val urls : unit -> string generator
(** [urls ()] creates a generator for valid URL strings. *)

val domains : ?max_length:int -> unit -> string generator
(** [domains ?max_length ()] creates a generator for domain name strings. *)

val dates : unit -> string generator
(** [dates ()] creates a generator for ISO 8601 date strings (YYYY-MM-DD). *)

val times : unit -> string generator
(** [times ()] creates a generator for time strings. *)

val datetimes : unit -> string generator
(** [datetimes ()] creates a generator for ISO 8601 datetime strings. *)

val ip_addresses : ?version:int -> unit -> string generator
(** [ip_addresses ?version ()] creates a generator for IP address strings. *)

val from_regex : string -> ?fullmatch:bool -> unit -> string generator
(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern]. *)
