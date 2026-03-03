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

    Uses schema type ["float"] as required by the Hegel server. The fields
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
      (`Text "type", `Text "float");
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
            failwith "Internal error: server returned non-array for list schema"
      in
      Basic { schema = raw_schema; transform = list_transform }
  | None ->
      (* Non-basic element: use CompositeList, which creates a fresh
         collection per generate() call via the CompositeList arm of
         [generate]. *)
      CompositeList { elements; min_size; max_size }

(** [just value] creates a generator that always produces [value].

    The schema uses [{"const": null}] and the transform ignores the server
    result, returning the constant [value]. *)
let just value =
  Basic
    { schema = `Map [ (`Text "const", `Null) ]; transform = (fun _ -> value) }

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern].

    When [fullmatch] is [true] (the default), the entire string must match the
    pattern. When [false], a substring match suffices. *)
let from_regex pattern ?(fullmatch = true) () =
  Basic
    {
      schema =
        `Map
          [
            (`Text "type", `Text "regex");
            (`Text "pattern", `Text pattern);
            (`Text "fullmatch", `Bool fullmatch);
          ];
      transform = Cbor_helpers.extract_string;
    }

(** [emails ()] creates a generator for valid email address strings. *)
let emails () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "email") ];
      transform = Cbor_helpers.extract_string;
    }

(** [urls ()] creates a generator for valid URL strings. *)
let urls () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "url") ];
      transform = Cbor_helpers.extract_string;
    }

(** [domains ?max_length ()] creates a generator for domain name strings.

    If [max_length] is provided, generated domains will not exceed that length.
*)
let domains ?max_length () =
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "domain");
        Option.map (fun ml -> (`Text "max_length", `Int ml)) max_length;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [dates ()] creates a generator for ISO 8601 date strings (YYYY-MM-DD). *)
let dates () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "date") ];
      transform = Cbor_helpers.extract_string;
    }

(** [times ()] creates a generator for time strings. *)
let times () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "time") ];
      transform = Cbor_helpers.extract_string;
    }

(** [datetimes ()] creates a generator for ISO 8601 datetime strings. *)
let datetimes () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "datetime") ];
      transform = Cbor_helpers.extract_string;
    }

(** [one_of generators] creates a generator that picks from one of the given
    [generators].

    Three code paths depending on generator types:
    - All basic with identity transforms: simple [one_of] schema.
    - All basic with some transforms: tagged-tuple schema with dispatch.
    - Any non-basic: compositional via [ONE_OF] span.

    Requires at least 2 generators. *)
let one_of (generators : 'a generator list) =
  if List.length generators < 2 then
    failwith "one_of requires at least 2 generators";
  let basics = List.filter_map as_basic generators in
  if List.length basics <> List.length generators then begin
    (* Path 3: composite — generate index in ONE_OF span, then delegate *)
    let gens = Array.of_list generators in
    let n = Array.length gens in
    Composite
      {
        label = Labels.one_of;
        generate_fn =
          (fun data ->
            let idx =
              Cbor_helpers.extract_int
                (Client.generate_from_schema
                   (`Map
                      [
                        (`Text "type", `Text "integer");
                        (`Text "min_value", `Int 0);
                        (`Text "max_value", `Int (n - 1));
                      ])
                   data)
            in
            do_draw gens.(idx) data);
      }
  end
  else
    (* Check if all have identity transforms by testing if the transform is
       literally the identity. We can't test function equality, so we check
       if all transforms act as identity on a canary value. Instead, we use
       the tagged-tuple path which is always correct. *)
    (* Path 2: tagged-tuple schema with dispatch transform *)
    let tagged_schemas =
      List.mapi
        (fun i (s, _) ->
          `Map
            [
              (`Text "type", `Text "tuple");
              (`Text "elements", `Array [ `Map [ (`Text "const", `Int i) ]; s ]);
            ])
        basics
    in
    let transforms = Array.of_list (List.map snd basics) in
    let dispatch raw =
      match raw with
      | `Array [ tag; value ] ->
          let idx = Cbor_helpers.extract_int tag in
          transforms.(idx) value
      | _ -> failwith "one_of: expected [tag, value] tuple from server"
    in
    Basic
      {
        schema = `Map [ (`Text "one_of", `Array tagged_schemas) ];
        transform = dispatch;
      }

(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element].

    Equivalent to [one_of [just None; map (fun x -> Some x) element]]. *)
let optional element = one_of [ just None; map (fun x -> Some x) element ]

(** [ip_addresses ?version ()] creates a generator for IP address strings.

    - [version = Some 4]: generates IPv4 addresses (dotted decimal).
    - [version = Some 6]: generates IPv6 addresses (colon hex).
    - [version = None] (default): generates either IPv4 or IPv6. *)
let rec ip_addresses ?version () =
  match version with
  | Some 4 ->
      Basic
        {
          schema = `Map [ (`Text "type", `Text "ipv4") ];
          transform = Cbor_helpers.extract_string;
        }
  | Some 6 ->
      Basic
        {
          schema = `Map [ (`Text "type", `Text "ipv6") ];
          transform = Cbor_helpers.extract_string;
        }
  | None -> one_of [ ip_addresses ~version:4 (); ip_addresses ~version:6 () ]
  | Some v -> failwith (Printf.sprintf "ip_addresses: invalid version %d" v)

(** [tuples2 g1 g2] creates a generator for 2-element tuples.

    When both elements are basic, a single [generate] command is sent using a
    [tuple] schema. Otherwise, elements are generated separately inside a
    {!Labels.tuple} span. *)
let tuples2 (type a b) (g1 : a generator) (g2 : b generator) : (a * b) generator
    =
  match (as_basic g1, as_basic g2) with
  | Some (s1, t1), Some (s2, t2) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple"); (`Text "elements", `Array [ s1; s2 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2 ] -> (t1 v1, t2 v2)
              | _ -> failwith "tuples2: expected 2-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              (a, b));
        }

(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples.

    When all elements are basic, a single [generate] command is sent. Otherwise,
    elements are generated separately inside a {!Labels.tuple} span. *)
let tuples3 (type a b c) (g1 : a generator) (g2 : b generator)
    (g3 : c generator) : (a * b * c) generator =
  match (as_basic g1, as_basic g2, as_basic g3) with
  | Some (s1, t1), Some (s2, t2), Some (s3, t3) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple");
            (`Text "elements", `Array [ s1; s2; s3 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3 ] -> (t1 v1, t2 v2, t3 v3)
              | _ -> failwith "tuples3: expected 3-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              let c = do_draw g3 data in
              (a, b, c));
        }

(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples.

    When all elements are basic, a single [generate] command is sent. Otherwise,
    elements are generated separately inside a {!Labels.tuple} span. *)
let tuples4 (type a b c d) (g1 : a generator) (g2 : b generator)
    (g3 : c generator) (g4 : d generator) : (a * b * c * d) generator =
  match (as_basic g1, as_basic g2, as_basic g3, as_basic g4) with
  | Some (s1, t1), Some (s2, t2), Some (s3, t3), Some (s4, t4) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple");
            (`Text "elements", `Array [ s1; s2; s3; s4 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3; v4 ] -> (t1 v1, t2 v2, t3 v3, t4 v4)
              | _ -> failwith "tuples4: expected 4-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              let c = do_draw g3 data in
              let d = do_draw g4 data in
              (a, b, c, d));
        }
