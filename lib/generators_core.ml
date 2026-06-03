open! Core

(** Constants for span labels used in generation tracking. *)
module Labels = struct
  let list = 1
  let list_element = 2
  let set = 3
  let set_element = 4
  let map = 5
  let map_entry = 6
  let tuple = 7
  let one_of = 8
  let optional = 9
  let fixed_dict = 10
  let flat_map = 11
  let filter = 12
  let mapped = 13
  let sampled_from = 14
  let enum_variant = 15
  let stateful_rule = 16
end

(** The type of generators. Generators produce typed OCaml values and can be
    combined using {!map}, {!flat_map}, and {!filter}.

    - [Basic] generators hold a raw schema and a mandatory client-side
      transform. Calling {!map} on a [Basic] generator preserves the schema and
      composes transforms. The [unique_safe] flag tracks whether [transform] is
      known to preserve distinctness over the schema's value space (i.e. is
      injective); leaf generators set it to [true], and [map] sets it to
      [false] since the user's function may collapse distinct inputs. The
      optional [sexp_of] is the printer used to render a drawn value on the
      final replay; it is [None] when the output type's printer is not known.
    - [Mapped] generators wrap a source generator and a transform function.
    - [FlatMapped] generators wrap a source and a function returning a
      generator.
    - [Filtered] generators wrap a source and a predicate.
    - [CompositeList] generators use the collection protocol to generate lists
      of non-basic elements, creating a fresh collection per generate call.
    - [Composite] generators wrap a [generate_fn] thunk inside a span with the
      given [label]. Used for tuples and one_of with non-basic elements. *)
type 'a generator =
  | Basic :
      { schema : Cbor.t
      ; transform : Cbor.t -> 'a
      ; unique_safe : bool
      ; sexp_of : ('a -> Sexp.t) option
      }
      -> 'a generator
  | Mapped :
      { source : 'b generator
      ; f : 'b -> 'a
      }
      -> 'a generator
  | FlatMapped :
      { source : 'b generator
      ; f : 'b -> 'a generator
      }
      -> 'a generator
  | Filtered :
      { source : 'a generator
      ; predicate : 'a -> bool
      }
      -> 'a generator
  | CompositeList :
      { elements : 'a generator
      ; min_size : int
      ; max_size : int option
      }
      -> 'a list generator
  | Composite :
      { label : int
      ; generate_fn : Client.test_case -> 'a
      ; sexp_of : ('a -> Sexp.t) option
      }
      -> 'a generator

(** [basic ~schema ~transform ?sexp_of ?unique_safe ()] builds a {!Basic}
    generator. [sexp_of], when given, is the printer used to render a drawn
    value on the final replay. [unique_safe] defaults to [true] (leaf
    generators preserve distinctness); set it to [false] for transforms that
    may collapse distinct inputs. *)
let basic ?sexp_of ?(unique_safe = true) ~schema ~transform () =
  Basic { schema; transform; unique_safe; sexp_of }
;;

(** [composite ~label ~generate_fn ?sexp_of ()] builds a {!Composite} generator.
    [sexp_of], when given, renders a drawn value on the final replay. *)
let composite ?sexp_of ~label ~generate_fn () = Composite { label; generate_fn; sexp_of }

(** [printer gen] returns the printer carried by [gen], if any. [Basic] and
    [Composite] generators carry one when known; [Filtered] delegates to its
    source; a [CompositeList] composes a list printer from its elements'.
    [Mapped]/[FlatMapped] carry none, since their output type is chosen by user
    code. *)
let rec printer : type a. a generator -> (a -> Sexp.t) option = function
  | Basic { sexp_of; _ } -> sexp_of
  | Composite { sexp_of; _ } -> sexp_of
  | Filtered { source; _ } -> printer source
  | CompositeList { elements; _ } ->
    Option.map (printer elements) ~f:(fun elt xs -> Sexp.List (List.map xs ~f:elt))
  (* [Mapped]/[FlatMapped]: the output type is chosen by user code. *)
  | _ -> None
;;

(** Maximum number of filter attempts before calling [assume false]. *)
let max_filter_attempts = 3

(** [group label data f] runs [f ()] inside a span with the given [label]. The
    span is stopped with [discard:false] regardless of whether [f] raises. *)
let group label data f =
  Client.start_span ~label data;
  Exn.protect ~finally:(fun () -> Client.stop_span data) ~f:(fun () -> f ())
;;

(** [discardable_group label data f] runs [f ()] inside a span with [label]. If
    [f] raises, the span is stopped with [discard:true]; otherwise
    [discard:false]. *)
let discardable_group label data f =
  Client.start_span ~label data;
  match f () with
  | v ->
    Client.stop_span data;
    v
  | exception e ->
    Client.stop_span ~discard:true data;
    raise e
;;

(** A collection handle for generating variable-length sequences.

    Collections ask the engine when to stop generating elements. The [finished]
    flag short-circuits subsequent {!collection_more} calls once the engine
    signals completion. The engine-side collection is created on first use. *)
type collection =
  { mutable finished : bool
  ; mutable collection_id : int option
  ; min_size : int
  ; max_size : int option
  }

(** [new_collection ~min_size ?max_size data ()] creates a new collection
    handle. *)
let new_collection ~min_size ?max_size data () =
  ignore data;
  { finished = false; collection_id = None; min_size; max_size }
;;

(** [get_collection_id coll data] initializes the engine-side collection
    and returns its id. Raises {!Client.Data_exhausted} on StopTest. *)
let get_collection_id coll data =
  match coll.collection_id with
  | Some id -> id
  | None ->
    let id = Client.new_collection data ~min_size:coll.min_size ~max_size:coll.max_size in
    coll.collection_id <- Some id;
    id
;;

(** [collection_more coll data] returns [true] if more elements should be
    generated, [false] when the collection is complete. Once it returns [false],
    subsequent calls return [false] immediately. Raises {!Client.Data_exhausted}
    on StopTest. *)
let collection_more coll data =
  if coll.finished
  then false
  else (
    let collection_id = get_collection_id coll data in
    let more = Client.collection_more data ~collection_id in
    if not more then coll.finished <- true;
    more)
;;

(** [collection_reject coll data] rejects the last element of the collection.
    No-op if the collection is already finished. Raises {!Client.Data_exhausted}
    on StopTest. *)
let collection_reject coll data =
  if not coll.finished
  then (
    let collection_id = get_collection_id coll data in
    Client.collection_reject data ~collection_id)
;;

(** [do_draw gen data] produces a typed value from generator [gen] using the
    given test case [data]. *)
let rec do_draw : type a. a generator -> Client.test_case -> a =
  fun gen data ->
  match gen with
  | Basic { schema; transform; _ } -> transform (Client.generate_from_schema schema data)
  | Mapped { source; f } ->
    group Labels.mapped data (fun () ->
      let value = do_draw source data in
      f value)
  | FlatMapped { source; f } ->
    discardable_group Labels.flat_map data (fun () ->
      let first = do_draw source data in
      let second_gen = f first in
      do_draw second_gen data)
  | Filtered { source; predicate } ->
    let rec attempt i =
      if i > max_filter_attempts
      then raise Client.Assume_rejected
      else (
        Client.start_span ~label:Labels.filter data;
        let value = do_draw source data in
        if predicate value
        then (
          Client.stop_span data;
          value)
        else (
          Client.stop_span ~discard:true data;
          attempt (i + 1)))
    in
    attempt 1
  | CompositeList { elements; min_size; max_size } ->
    group Labels.list data (fun () ->
      let coll = new_collection ~min_size ?max_size data () in
      let rec collect acc =
        if collection_more coll data
        then collect (do_draw elements data :: acc)
        else List.rev acc
      in
      collect [])
  | Composite { label; generate_fn; _ } -> group label data (fun () -> generate_fn data)
;;

(** [draw ?label ?sexp_of tc gen] produces a typed value from generator [gen]
    using test case [tc].

    On the final replay of a failing test, an outermost draw prints its value
    through {!Client.note} — as [label = value] when [label] is given, or just
    the value otherwise. Draws nested inside a span (e.g. composite elements)
    are suppressed so only the outermost value shows. [sexp_of], when given,
    overrides the printer carried by [gen]; if neither is available nothing is
    printed. *)
let draw ?label ?sexp_of tc gen =
  let value = do_draw gen tc in
  if tc.Client.is_final && tc.Client.draw_depth = 0
  then
    Option.iter
      (Option.first_some sexp_of (printer gen))
      ~f:(fun f ->
        let rendered = Sexp.to_string_hum (f value) in
        Client.note
          tc
          (Option.value_map label ~default:rendered ~f:(fun l ->
             sprintf "%s = %s" l rendered)));
  value
;;

(** [draw_silent tc gen] is {!draw} without recording the value for the
    final-replay output. Use it for draws whose value is not a useful part of
    the printed counterexample. *)
let draw_silent tc gen = do_draw gen tc

(** [map f gen] transforms values from [gen] using [f].

    When [gen] is a [Basic] generator, the schema is preserved and transforms
    are composed. Otherwise, a [Mapped] generator is created. *)
let map : type a b. (a -> b) -> a generator -> b generator =
  fun f gen ->
  match gen with
  | Basic { schema; transform; _ } ->
    (* The user's [f] may collapse distinct inputs, so the composed transform
       is no longer known to preserve uniqueness. Consumers that rely on this
       (e.g. [lists ~unique:true]) must fall back to a post-transform dedup
       path when [unique_safe] is [false]. *)
    basic ~schema ~transform:(fun x -> f (transform x)) ~unique_safe:false ()
  | other -> Mapped { source = other; f }
;;

(** [flat_map f gen] creates a dependent generator. The function [f] receives
    the generated value and returns a new generator whose value is the final
    result. *)
let flat_map f gen = FlatMapped { source = gen; f }

(** [filter predicate gen] filters values from [gen] using [predicate]. Tries up
    to {!max_filter_attempts} times; calls [assume false] if all attempts fail.
*)
let filter predicate gen = Filtered { source = gen; predicate }

(** [schema gen] returns the schema for a [Basic] generator, or [None]. *)
let schema : type a. a generator -> Cbor.t option = function
  | Basic { schema; _ } -> Some schema
  | _ -> None
;;

(** [is_basic gen] returns [true] if [gen] is a [Basic] generator. *)
let is_basic : type a. a generator -> bool = function
  | Basic _ -> true
  | _ -> false
;;

(** [as_basic gen] returns [Some (schema, transform)] if [gen] is [Basic], or
    [None] otherwise. *)
let as_basic : type a. a generator -> (Cbor.t * (Cbor.t -> a)) option = function
  | Basic { schema; transform; _ } -> Some (schema, transform)
  | _ -> None
;;

(** [basic_unique_safe gen] returns [true] iff [gen] is a [Basic] generator
    whose transform is known to preserve distinctness (i.e. is injective over
    the schema's value space). Used by [lists ~unique:true] to decide between
    the server-side fast path and the client-side dedup fallback. *)
let basic_unique_safe : type a. a generator -> bool = function
  | Basic { unique_safe; _ } -> unique_safe
  | _ -> false
;;
