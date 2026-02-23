(** Generator combinators for the Hegel SDK.

    This module provides a composable generator API for property-based testing.
    Generators produce CBOR values and can be combined using {!map},
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

(** The type of generators. Generators produce CBOR values and can be combined
    using {!map}, {!flat_map}, and {!filter}.

    - [Basic] generators hold a raw schema and an optional client-side
      transform. Calling {!map} on a [Basic] generator preserves the schema and
      composes transforms.
    - [Mapped] generators wrap a source generator and a transform function.
    - [FlatMapped] generators wrap a source and a function returning a
      generator.
    - [Filtered] generators wrap a source and a predicate. *)
type generator =
  | Basic of {
      schema : CBOR.Simple.t;
      transform : (CBOR.Simple.t -> CBOR.Simple.t) option;
    }
  | Mapped of { source : generator; f : CBOR.Simple.t -> CBOR.Simple.t }
  | FlatMapped of { source : generator; f : CBOR.Simple.t -> generator }
  | Filtered of { source : generator; predicate : CBOR.Simple.t -> bool }

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

(** [generate gen] produces a value from generator [gen]. *)
let rec generate gen =
  match gen with
  | Basic { schema; transform = None } -> Client.generate_from_schema schema
  | Basic { schema; transform = Some f } ->
      f (Client.generate_from_schema schema)
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

(** [map f gen] transforms values from [gen] using [f].

    When [gen] is a [Basic] generator, the schema is preserved and transforms
    are composed. Otherwise, a [Mapped] generator is created. *)
let map f gen =
  match gen with
  | Basic { schema; transform = None } -> Basic { schema; transform = Some f }
  | Basic { schema; transform = Some existing } ->
      Basic { schema; transform = Some (fun x -> f (existing x)) }
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
let schema gen = match gen with Basic { schema; _ } -> Some schema | _ -> None

(** [is_basic gen] returns [true] if [gen] is a [Basic] generator. *)
let is_basic gen = match gen with Basic _ -> true | _ -> false

(** [as_basic gen] returns [Some (schema, transform)] if [gen] is [Basic], or
    [None] otherwise. *)
let as_basic gen =
  match gen with
  | Basic { schema; transform } -> Some (schema, transform)
  | _ -> None

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
          Client._test_aborted := true;
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
        Client._test_aborted := true;
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

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
let integers ?min_value ?max_value () =
  let pairs = [ (`Text "type", `Text "integer") ] in
  let pairs =
    match min_value with
    | Some v -> pairs @ [ (`Text "min_value", `Int v) ]
    | None -> pairs
  in
  let pairs =
    match max_value with
    | Some v -> pairs @ [ (`Text "max_value", `Int v) ]
    | None -> pairs
  in
  Basic { schema = `Map pairs; transform = None }
