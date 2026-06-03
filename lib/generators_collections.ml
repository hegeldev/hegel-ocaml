open! Core
open Generators_core

(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps).

    When both [keys] and [values] are basic generators, sends a [dict] schema to
    the server (fast path). When either is non-basic, uses the collection
    protocol to generate key-value pairs one at a time. *)
let hashmaps keys values ?(min_size = 0) ?max_size () =
  if min_size < 0
  then raise (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  let sexp_of =
    Option.map
      (Option.both (printer keys) (printer values))
      ~f:(fun (pk, pv) kvs ->
        Sexp.List (List.map kvs ~f:(fun (k, v) -> Sexp.List [ pk k; pv v ])))
  in
  match as_basic keys, as_basic values with
  | Some (key_schema, key_transform), Some (val_schema, val_transform) ->
    let pairs =
      List.filter_opt
        [ Some (`Text "type", `Text "dict")
        ; Some (`Text "keys", key_schema)
        ; Some (`Text "values", val_schema)
        ; Some (`Text "min_size", `Int min_size)
        ; Option.map max_size ~f:(fun ms -> `Text "max_size", `Int ms)
        ]
    in
    let transform raw =
      match raw with
      | `Array kv_pairs ->
        List.map kv_pairs ~f:(function
          | `Array [ k; v ] -> key_transform k, val_transform v
          | _ -> failwith "hashmaps: expected [k, v] pair from server")
      | _ -> failwith "hashmaps: expected array from server"
    in
    basic
      ?sexp_of
      ~schema:(`Map pairs)
      ~transform
      ~unique_safe:(basic_unique_safe keys && basic_unique_safe values)
      ()
  | _ ->
    (* dict semantics require unique keys, so we dedup client-side here just
       like [lists ~unique:true] does *)
    composite
      ?sexp_of
      ~label:Labels.map
      ~generate_fn:(fun data ->
        let coll = new_collection ~min_size ?max_size data () in
        let rec collect acc =
          if collection_more coll data
          then (
            let k = do_draw keys data in
            if List.exists acc ~f:(fun (k', _) -> Poly.equal k' k)
            then (
              collection_reject coll data;
              collect acc)
            else (
              let v = do_draw values data in
              collect ((k, v) :: acc)))
          else List.rev acc
        in
        collect [])
      ()
;;

(** [lists elements ?min_size ?max_size ?unique ()] creates a generator for
    lists.

    When [elements] is a [Basic] generator, sends a [list] schema to the server
    and lets it generate the entire list (fast path). The element transform is
    lifted to apply to every item in the resulting list.

    When [elements] is non-basic (e.g. filtered or flat-mapped), uses the
    collection protocol inside a {!Labels.list} span to generate elements one at
    a time. A fresh collection is created on each call to [generate].

    When [unique] is [true], the generated list will contain only distinct
    elements. For basic elements this is handled server-side; for non-basic
    elements, duplicates are rejected via the collection protocol. *)
let lists elements ?(min_size = 0) ?max_size ?(unique = false) () =
  if min_size < 0
  then raise (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  let sexp_of =
    Option.map (printer elements) ~f:(fun elt xs -> Sexp.List (List.map xs ~f:elt))
  in
  match as_basic elements with
  | Some (elem_schema, elem_transform) when (not unique) || basic_unique_safe elements ->
    (* Server-side uniqueness is only safe when the element transform
       preserves distinctness (i.e. is injective). Otherwise distinct raw
       values from the server can collapse to the same OCaml value, leaving
       the post-transform list with duplicates. When that happens we fall
       through to the [unique]-aware dedup path below. *)
    let pairs =
      List.filter_opt
        [ Some (`Text "type", `Text "list")
        ; Some (`Text "unique", `Bool unique)
        ; Some (`Text "elements", elem_schema)
        ; Some (`Text "min_size", `Int min_size)
        ; Option.map max_size ~f:(fun ms -> `Text "max_size", `Int ms)
        ]
    in
    let raw_schema = `Map pairs in
    let list_transform raw_list =
      match raw_list with
      | `Array items -> List.map items ~f:elem_transform
      | _ -> failwith "Internal error: server returned non-array for list schema"
    in
    basic
      ?sexp_of
      ~schema:raw_schema
      ~transform:list_transform
      ~unique_safe:(basic_unique_safe elements)
      ()
  | _ ->
    if not unique
    then
      (* Non-basic element without uniqueness: use CompositeList. *)
      CompositeList { elements; min_size; max_size }
    else
      (* Non-basic with uniqueness: use Composite with collection protocol
           and duplicate rejection.  The server's own rejection limit
           (via [many.reject]) will send StopTest when too many duplicates
           occur, which [collection_reject] converts to [Data_exhausted]. *)
      composite
        ?sexp_of
        ~label:Labels.list
        ~generate_fn:(fun data ->
          let coll = new_collection ~min_size ?max_size data () in
          let rec collect acc =
            if collection_more coll data
            then (
              let elem = do_draw elements data in
              if List.mem acc elem ~equal:Poly.equal
              then (
                collection_reject coll data;
                collect acc)
              else collect (elem :: acc))
            else List.rev acc
          in
          collect [])
        ()
;;
