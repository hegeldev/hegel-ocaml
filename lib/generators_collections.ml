open Generators_core

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
