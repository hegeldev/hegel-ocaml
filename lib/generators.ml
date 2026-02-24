(** Generator combinators for the Hegel SDK.

    This module provides a composable generator API for property-based testing.
    Generators produce typed OCaml values and can be combined using {!map},
    {!flat_map}, and {!filter}. {!Basic} generators preserve schemas through
    {!map} for optimized server communication.

    The collection protocol helpers ({!new_collection}, {!collection_more},
    {!collection_reject}) support variable-length sequence generation. *)

open Connection

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
end

(** The type of generators. Generators produce typed OCaml values and can be
    combined using {!map}, {!flat_map}, and {!filter}.

    - [Basic] generators hold a raw schema and a mandatory client-side
      transform. Calling {!map} on a [Basic] generator preserves the schema and
      composes transforms.
    - [Mapped] generators wrap a source generator and a transform function.
    - [FlatMapped] generators wrap a source and a function returning a
      generator.
    - [Filtered] generators wrap a source and a predicate.
    - [CompositeList] generators use the collection protocol to generate lists
      of non-basic elements, creating a fresh collection per generate call. *)
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

(** Maximum number of filter attempts before calling [assume false]. *)
let max_filter_attempts = 3

(** [group label f] runs [f ()] inside a span with the given [label]. The span
    is stopped with [discard:false] regardless of whether [f] raises. *)
let group label f =
  Client.start_span ~label ();
  Fun.protect ~finally:(fun () -> Client.stop_span ()) (fun () -> f ())

(** [discardable_group label f] runs [f ()] inside a span with [label]. If [f]
    raises, the span is stopped with [discard:true]; otherwise [discard:false].
*)
let discardable_group label f =
  Client.start_span ~label ();
  match f () with
  | v ->
      Client.stop_span ();
      v
  | exception e ->
      Client.stop_span ~discard:true ();
      raise e

type collection = {
  mutable finished : bool;
  mutable server_name : CBOR.Simple.t option;
  min_size : int;
  max_size : int option;
}
(** A collection handle for generating variable-length sequences.

    Collections communicate with the server to determine when to stop generating
    elements. The [finished] flag short-circuits subsequent {!collection_more}
    calls once the server signals completion. *)

(** [new_collection ~min_size ?max_size ()] creates a new collection handle. *)
let new_collection ~min_size ?max_size () =
  { finished = false; server_name = None; min_size; max_size }

(** [get_server_name coll] lazily initializes the server-side collection and
    returns its handle. Raises {!Client.Data_exhausted} on StopTest. *)
let get_server_name coll =
  match coll.server_name with
  | Some name -> name
  | None ->
      let channel = Client.get_channel () in
      let max_size_val =
        match coll.max_size with Some ms -> `Int ms | None -> `Null
      in
      let result =
        try
          pending_get
            (request channel
               (`Map
                  [
                    (`Text "command", `Text "new_collection");
                    (`Text "min_size", `Int coll.min_size);
                    (`Text "max_size", max_size_val);
                  ]))
        with Request_error e when e.error_type = "StopTest" ->
          Client.test_aborted := true;
          raise Client.Data_exhausted
      in
      coll.server_name <- Some result;
      result

(** [collection_more coll] returns [true] if more elements should be generated,
    [false] when the collection is complete. Once it returns [false], subsequent
    calls return [false] immediately. Raises {!Client.Data_exhausted} on
    StopTest. *)
let collection_more coll =
  if coll.finished then false
  else
    let server_name = get_server_name coll in
    let channel = Client.get_channel () in
    let result =
      try
        pending_get
          (request channel
             (`Map
                [
                  (`Text "command", `Text "collection_more");
                  (`Text "collection", server_name);
                ]))
      with Request_error e when e.error_type = "StopTest" ->
        Client.test_aborted := true;
        raise Client.Data_exhausted
    in
    let more = Cbor_helpers.extract_bool result in
    if not more then coll.finished <- true;
    more

(** [collection_reject coll] rejects the last element of the collection. No-op
    if the collection is already finished. *)
let collection_reject coll =
  if not coll.finished then begin
    let server_name = get_server_name coll in
    let channel = Client.get_channel () in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "collection_reject");
                 (`Text "collection", server_name);
               ])))
  end

(** [generate gen] produces a typed value from generator [gen]. *)
let rec generate : type a. a generator -> a =
 fun gen ->
  match gen with
  | Basic { schema; transform } ->
      transform (Client.generate_from_schema schema)
  | Mapped { source; f } ->
      group Labels.mapped (fun () ->
          let value = generate source in
          f value)
  | FlatMapped { source; f } ->
      group Labels.flat_map (fun () ->
          let first = generate source in
          let second_gen = f first in
          generate second_gen)
  | Filtered { source; predicate } ->
      let rec attempt i =
        if i > max_filter_attempts then raise Client.Assume_rejected
        else begin
          Client.start_span ~label:Labels.filter ();
          let value = generate source in
          if predicate value then begin
            Client.stop_span ();
            value
          end
          else begin
            Client.stop_span ~discard:true ();
            attempt (i + 1)
          end
        end
      in
      attempt 1
  | CompositeList { elements; min_size; max_size } ->
      group Labels.list (fun () ->
          let coll = new_collection ~min_size ?max_size () in
          let result = ref [] in
          while collection_more coll do
            result := generate elements :: !result
          done;
          List.rev !result)

(** [map f gen] transforms values from [gen] using [f].

    When [gen] is a [Basic] generator, the schema is preserved and transforms
    are composed. Otherwise, a [Mapped] generator is created. *)
let map : type a b. (a -> b) -> a generator -> b generator =
 fun f gen ->
  match gen with
  | Basic { schema; transform } ->
      Basic { schema; transform = (fun x -> f (transform x)) }
  | other -> Mapped { source = other; f }

(** [flat_map f gen] creates a dependent generator. The function [f] receives
    the generated value and returns a new generator whose value is the final
    result. *)
let flat_map f gen = FlatMapped { source = gen; f }

(** [filter predicate gen] filters values from [gen] using [predicate]. Tries up
    to {!max_filter_attempts} times; calls [assume false] if all attempts fail.
*)
let filter predicate gen = Filtered { source = gen; predicate }

(** [schema gen] returns the schema for a [Basic] generator, or [None]. *)
let schema : type a. a generator -> CBOR.Simple.t option = function
  | Basic { schema; _ } -> Some schema
  | _ -> None

(** [is_basic gen] returns [true] if [gen] is a [Basic] generator. *)
let is_basic : type a. a generator -> bool = function
  | Basic _ -> true
  | _ -> false

(** [as_basic gen] returns [Some (schema, transform)] if [gen] is [Basic], or
    [None] otherwise. *)
let as_basic : type a.
    a generator -> (CBOR.Simple.t * (CBOR.Simple.t -> a)) option = function
  | Basic { schema; transform } -> Some (schema, transform)
  | _ -> None

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
let integers ?min_value ?max_value () =
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "integer");
        Option.map (fun v -> (`Text "min_value", `Int v)) min_value;
        Option.map (fun v -> (`Text "max_value", `Int v)) max_value;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_int }

(** [booleans ()] creates a generator for boolean values. *)
let booleans () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "boolean") ];
      transform = Cbor_helpers.extract_bool;
    }

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    Uses schema type ["number"] as required by the Hegel server. The fields
    [allow_nan], [allow_infinity], [exclude_min], [exclude_max], and [width] are
    always sent (required by the server). Defaults follow Hypothesis:
    - [allow_nan]: [true] only when no bounds are set
    - [allow_infinity]: [true] when at most one bound is set *)
let floats ?min_value ?max_value ?(exclude_min = false) ?(exclude_max = false)
    ?allow_nan ?allow_infinity () =
  let has_min = Option.is_some min_value in
  let has_max = Option.is_some max_value in
  let eff_allow_nan =
    match allow_nan with Some v -> v | None -> (not has_min) && not has_max
  in
  let eff_allow_infinity =
    match allow_infinity with
    | Some v -> v
    | None -> (not has_min) || not has_max
  in
  let pairs =
    [
      (`Text "type", `Text "number");
      (`Text "allow_nan", `Bool eff_allow_nan);
      (`Text "allow_infinity", `Bool eff_allow_infinity);
      (`Text "exclude_min", `Bool exclude_min);
      (`Text "exclude_max", `Bool exclude_max);
      (`Text "width", `Int 64);
    ]
  in
  let pairs =
    pairs
    @ List.filter_map Fun.id
        [
          Option.map (fun v -> (`Text "min_value", `Float v)) min_value;
          Option.map (fun v -> (`Text "max_value", `Float v)) max_value;
        ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_float }

(** [text ?min_size ?max_size ()] creates a generator for Unicode text strings.

    Uses schema type ["string"] as required by the Hegel server. *)
let text ?(min_size = 0) ?max_size () =
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "string");
        Some (`Text "min_size", `Int min_size);
        Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)
let binary ?(min_size = 0) ?max_size () =
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "binary");
        Some (`Text "min_size", `Int min_size);
        Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_bytes }

(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values.

    Implemented as an integer index generator: picks an index in [0, n-1] and
    returns [options.(index)]. *)
let sampled_from options =
  let arr = Array.of_list options in
  let n = Array.length arr in
  map (fun i -> arr.(i)) (integers ~min_value:0 ~max_value:(n - 1) ())

(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps). [keys] and [values] must be basic generators.

    The server returns the dict as a list of [[key, value]] pairs. The
    [hashmaps] generator automatically transforms this to a list of
    [(key, value)] tuples. *)
let hashmaps keys values ?(min_size = 0) ?max_size () =
  let key_schema, key_transform =
    match keys with
    | Basic { schema; transform } -> (schema, transform)
    | _ -> failwith "hashmaps: keys generator must be a Basic generator"
  in
  let val_schema, val_transform =
    match values with
    | Basic { schema; transform } -> (schema, transform)
    | _ -> failwith "hashmaps: values generator must be a Basic generator"
  in
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "dict");
        Some (`Text "keys", key_schema);
        Some (`Text "values", val_schema);
        Some (`Text "min_size", `Int min_size);
        Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
      ]
  in
  (* The server returns dicts as [[k, v], ...] lists. We transform to typed
     pairs. *)
  let transform raw =
    match raw with
    | `Array kv_pairs ->
        List.map
          (function
            | `Array [ k; v ] -> (key_transform k, val_transform v)
            | _ -> failwith "hashmaps: expected [k, v] pair from server")
          kv_pairs
    | _ -> failwith "hashmaps: expected array from server"
  in
  Basic { schema = `Map pairs; transform }

(** [lists elements ?min_size ?max_size ()] creates a generator for lists.

    When [elements] is a [Basic] generator, sends a [list] schema to the server
    and lets it generate the entire list (fast path). The element transform is
    lifted to apply to every item in the resulting list.

    When [elements] is non-basic (e.g. filtered or flat-mapped), uses the
    collection protocol inside a {!Labels.list} span to generate elements one at
    a time. A fresh collection is created on each call to [generate]. *)
let lists elements ?(min_size = 0) ?max_size () =
  match as_basic elements with
  | Some (elem_schema, elem_transform) ->
      let pairs =
        List.filter_map Fun.id
          [
            Some (`Text "type", `Text "list");
            Some (`Text "elements", elem_schema);
            Some (`Text "min_size", `Int min_size);
            Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
          ]
      in
      let raw_schema = `Map pairs in
      let list_transform raw_list =
        match raw_list with
        | `Array items -> List.map elem_transform items
        | _ ->
            (* Server always returns Array for list schema *)
            assert false
      in
      Basic { schema = raw_schema; transform = list_transform }
  | None ->
      (* Non-basic element: use CompositeList, which creates a fresh
         collection per generate() call via the CompositeList arm of
         [generate]. *)
      CompositeList { elements; min_size; max_size }
