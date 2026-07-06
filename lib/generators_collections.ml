open! Core
open Generators_core

(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps) over printable [keys] and [values].

    Key-value pairs are generated one at a time via the collection protocol.
    Dict semantics require unique keys, so duplicate keys are rejected
    client-side. *)
let hashmaps
      (keys : ('a, printable) generator)
      (values : ('b, printable) generator)
      ?(min_size = 0)
      ?max_size
      ()
  : (('a * 'b) list, printable) generator
  =
  if min_size < 0
  then raise (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  let pk = printer keys
  and pv = printer values in
  let sexp_of kvs =
    Sexp.List (List.map kvs ~f:(fun (k, v) -> Sexp.List [ pk k; pv v ]))
  in
  let core =
    Composite
      { label = Labels.map
      ; generate_fn =
          (fun data ->
            let coll = new_collection ~min_size ?max_size data () in
            let rec collect acc =
              if collection_more coll data
              then (
                let k = do_draw (core_of keys) data in
                if List.exists acc ~f:(fun (k', _) -> Poly.equal k' k)
                then (
                  collection_reject coll data;
                  collect acc)
                else (
                  let v = do_draw (core_of values) data in
                  collect ((k, v) :: acc)))
              else List.rev acc
            in
            collect [])
      }
  in
  Printable { core; sexp_of }
;;

(** [lists elements ?min_size ?max_size ?unique ()] creates a generator for
    lists of printable [elements].

    Elements are generated one at a time via the collection protocol inside a
    {!Labels.list} span. When [unique] is [true], duplicate elements are rejected
    client-side so the generated list contains only distinct elements. *)
let lists
      (elements : ('a, printable) generator)
      ?(min_size = 0)
      ?max_size
      ?(unique = false)
      ()
  : ('a list, printable) generator
  =
  if min_size < 0
  then raise (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  let elt = printer elements in
  let sexp_of xs = Sexp.List (List.map xs ~f:elt) in
  let core =
    if not unique
    then CompositeList { elements = core_of elements; min_size; max_size }
    else
      (* With uniqueness, drive the collection protocol and reject duplicates.
         The engine's own rejection limit sends StopTest when too many
         duplicates occur, which [collection_reject] converts to
         [Data_exhausted]. *)
      Composite
        { label = Labels.list
        ; generate_fn =
            (fun data ->
              let coll = new_collection ~min_size ?max_size data () in
              let rec collect acc =
                if collection_more coll data
                then (
                  let elem = do_draw (core_of elements) data in
                  if List.mem acc elem ~equal:Poly.equal
                  then (
                    collection_reject coll data;
                    collect acc)
                  else collect (elem :: acc))
                else List.rev acc
              in
              collect [])
        }
  in
  Printable { core; sexp_of }
;;
