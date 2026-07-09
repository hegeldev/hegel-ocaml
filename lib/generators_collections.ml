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
  (* The printing twin of [generate_fn] below: each key-value entry prints
     inside a speculative region (its separator included), so an entry
     retracted for a duplicate key leaves no text behind — the key prints
     through its own generator as it draws, and a duplicate retracts that
     text along with the rest of the entry. The layout is OCaml's: flat as
     [[ (k, v); … ]], broken with each entry on its own line behind a
     leading semicolon. *)
  let print tc doc =
    group Labels.map tc (fun () ->
      let coll = new_collection ~min_size ?max_size tc () in
      Pretty.begin_group doc ~indent:0 "[";
      let rec collect acc =
        if collection_more coll tc
        then (
          Pretty.begin_speculative doc;
          match
            if List.is_empty acc
            then Pretty.text doc " "
            else (
              Pretty.breakable doc "; ";
              Pretty.if_break doc "; ");
            Pretty.begin_group doc ~indent:0 "(";
            print_draw keys tc doc
          with
          | exception exn ->
            Pretty.abort_speculative doc;
            raise exn
          | k ->
            if List.exists acc ~f:(fun (k', _) -> Poly.equal k' k)
            then (
              Pretty.abort_speculative doc;
              collection_reject coll tc;
              collect acc)
            else (
              match
                Pretty.breakable doc ", ";
                Pretty.if_break doc ", ";
                let v = print_draw values tc doc in
                Pretty.end_group doc ~dedent:0 ")";
                v
              with
              | v ->
                Pretty.commit_speculative doc;
                collect ((k, v) :: acc)
              | exception exn ->
                Pretty.abort_speculative doc;
                raise exn))
        else List.rev acc
      in
      let entries = collect [] in
      if not (List.is_empty entries) then Pretty.breakable doc " ";
      Pretty.end_group doc ~dedent:0 "]";
      entries)
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
  Printable { core; sexp_of; print_draw = Some print }
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
  (* The printing twin of the list interpreters: elements print one at a time
     between the collection protocol's calls, each inside a speculative region
     (its separator included) so a rejected duplicate leaves no text behind.
     Non-unique lists never reject, but share the shape. The layout is
     OCaml's: [[]] empty, [[ 0; 1 ]] flat, and broken with each element on
     its own line behind a leading semicolon and the close bracket on its own
     line. *)
  let print tc doc =
    group Labels.list tc (fun () ->
      let coll = new_collection ~min_size ?max_size tc () in
      Pretty.begin_group doc ~indent:0 "[";
      let rec collect acc =
        if collection_more coll tc
        then (
          Pretty.begin_speculative doc;
          match
            if List.is_empty acc
            then Pretty.text doc " "
            else (
              Pretty.breakable doc "; ";
              Pretty.if_break doc "; ");
            print_draw elements tc doc
          with
          | exception exn ->
            Pretty.abort_speculative doc;
            raise exn
          | elem ->
            if unique && List.mem acc elem ~equal:Poly.equal
            then (
              Pretty.abort_speculative doc;
              collection_reject coll tc;
              collect acc)
            else (
              Pretty.commit_speculative doc;
              collect (elem :: acc)))
        else List.rev acc
      in
      let elems = collect [] in
      if not (List.is_empty elems) then Pretty.breakable doc " ";
      Pretty.end_group doc ~dedent:0 "]";
      elems)
  in
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
  Printable { core; sexp_of; print_draw = Some print }
;;
