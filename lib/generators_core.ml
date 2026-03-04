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
      of non-basic elements, creating a fresh collection per generate call.
    - [Composite] generators wrap a [generate_fn] thunk inside a span with the
      given [label]. Used for tuples and one_of with non-basic elements. *)
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
  | Composite : {
      label : int;
      generate_fn : Client.test_case_data -> 'a;
    }
      -> 'a generator

(** Maximum number of filter attempts before calling [assume false]. *)
let max_filter_attempts = 3

(** [group label data f] runs [f ()] inside a span with the given [label]. The
    span is stopped with [discard:false] regardless of whether [f] raises. *)
let group label data f =
  Client.start_span ~label data;
  Fun.protect ~finally:(fun () -> Client.stop_span data) (fun () -> f ())

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

(** [new_collection ~min_size ?max_size data ()] creates a new collection
    handle. *)
let new_collection ~min_size ?max_size data () =
  ignore data;
  { finished = false; server_name = None; min_size; max_size }

(** [get_server_name coll data] lazily initializes the server-side collection
    and returns its handle. Raises {!Client.Data_exhausted} on StopTest. *)
let get_server_name coll data =
  match coll.server_name with
  | Some name -> name
  | None ->
      let channel = data.Client.channel in
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
          data.test_aborted <- true;
          raise Client.Data_exhausted
      in
      coll.server_name <- Some result;
      result

(** [collection_more coll data] returns [true] if more elements should be
    generated, [false] when the collection is complete. Once it returns [false],
    subsequent calls return [false] immediately. Raises {!Client.Data_exhausted}
    on StopTest. *)
let collection_more coll data =
  if coll.finished then false
  else
    let server_name = get_server_name coll data in
    let channel = data.Client.channel in
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
        data.test_aborted <- true;
        raise Client.Data_exhausted
    in
    let more = Cbor_helpers.extract_bool result in
    if not more then coll.finished <- true;
    more

(** [collection_reject coll data] rejects the last element of the collection.
    No-op if the collection is already finished. *)
let collection_reject coll data =
  if not coll.finished then begin
    let server_name = get_server_name coll data in
    let channel = data.Client.channel in
    ignore
      (pending_get
         (request channel
            (`Map
               [
                 (`Text "command", `Text "collection_reject");
                 (`Text "collection", server_name);
               ])))
  end

(** [do_draw gen data] produces a typed value from generator [gen] using the
    given test case [data]. *)
let rec do_draw : type a. a generator -> Client.test_case_data -> a =
 fun gen data ->
  match gen with
  | Basic { schema; transform } ->
      transform (Client.generate_from_schema schema data)
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
        if i > max_filter_attempts then raise Client.Assume_rejected
        else begin
          Client.start_span ~label:Labels.filter data;
          let value = do_draw source data in
          if predicate value then begin
            Client.stop_span data;
            value
          end
          else begin
            Client.stop_span ~discard:true data;
            attempt (i + 1)
          end
        end
      in
      attempt 1
  | CompositeList { elements; min_size; max_size } ->
      group Labels.list data (fun () ->
          let coll = new_collection ~min_size ?max_size data () in
          let rec collect acc =
            if collection_more coll data then
              collect (do_draw elements data :: acc)
            else List.rev acc
          in
          collect [])
  | Composite { label; generate_fn } ->
      group label data (fun () -> generate_fn data)

(** [draw gen] produces a typed value from generator [gen]. Must be called from
    within a Hegel test body. *)
let draw gen =
  match Domain.DLS.get Client.current_data with
  | None -> failwith "draw() cannot be called outside of a Hegel test"
  | Some data -> do_draw gen data

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
