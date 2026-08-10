module Sexp = Sexplib0.Sexp

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
  let _feature_flag = 16
  let stateful_rule = 31
  let function_result = 1001
end

(** The pure generation structure of a generator, carrying no printer. A
    {!generator} is a {!core} paired (or not) with a printer; see {!generator}.

    - [Leaf] cores hold a [draw] closure that performs a single engine draw (via
      one of the typed {!Internal} primitives) and returns the typed value.
      Mapping a [Leaf] composes the closure in place, staying a leaf (no extra
      span), because the engine already wraps each primitive draw in its own
      span.
    - [Mapped] cores wrap a source and a transform function.
    - [FlatMapped] cores wrap a source and a function returning a core.
    - [Filtered] cores wrap a source and a predicate.
    - [CompositeList] cores use the collection protocol to generate lists of
      non-basic elements, creating a fresh collection per generate call.
    - [Composite] cores wrap a [generate_fn] thunk inside a span with the given
      [label]. Used for tuples and one_of with non-basic elements. *)
type 'a core =
  | Leaf : { draw : Internal.test_case -> 'a } -> 'a core
  | Mapped :
      { source : 'b core
      ; f : 'b -> 'a
      }
      -> 'a core
  | FlatMapped :
      { source : 'b core
      ; f : 'b -> 'a core
      }
      -> 'a core
  | Filtered :
      { source : 'a core
      ; predicate : 'a -> bool
      }
      -> 'a core
  | CompositeList :
      { elements : 'a core
      ; min_size : int
      ; max_size : int option
      }
      -> 'a list core
  | Composite :
      { label : int
      ; generate_fn : Internal.test_case -> 'a
      }
      -> 'a core
  | Values :
      { pool : Internal.pool
      ; find : int -> 'a option
      ; remove : int -> unit
      ; is_empty : unit -> bool
      ; consume : bool
      }
      -> 'a core
  | Function : { build : name:string option -> Internal.test_case -> 'a } -> 'a core

(** Phantom witness that a generator carries a printer and so may be drawn with
    {!draw}. Defined as a private polymorphic variant (not left abstract) so
    the two witnesses are provably distinct: since OCaml 5.5 the exhaustiveness
    checker no longer assumes two module-local abstract types differ
    (ocaml/ocaml#13712), which would make matches on [(_, printable) generator]
    partial. *)
type printable = private [ `Printable ]

(** Phantom witness that a generator carries no printer; such a generator can
    only be drawn with {!draw_silent} (or upgraded with {!with_printer}). See
    {!printable} for why this is not a bare abstract type. *)
type unprintable = private [ `Unprintable ]

(** A generator: a {!core} (how to generate) plus a phantom ['p] recording
    whether a printer is present. [Printable] structurally carries the printer,
    so {!draw} can always render its value; [Unprintable] carries none.

    Generators produce typed OCaml values and can be combined using {!map},
    {!flat_map}, and {!filter}. The phantom is what makes {!draw} require a
    printer at compile time while {!draw_silent} accepts any generator. *)
type ('a, 'p) generator =
  | Printable :
      { core : 'a core
      ; sexp_of : 'a -> Sexp.t
      }
      -> ('a, printable) generator
  | Unprintable : { core : 'a core } -> ('a, unprintable) generator

(** [core_of gen] is the generation structure of [gen], discarding printability.
*)
let core_of : type a p. (a, p) generator -> a core = function
  | Printable { core; _ } -> core
  | Unprintable { core } -> core
;;

(** [leaf ~draw ~sexp_of] builds a printable {!Leaf} generator. [draw] performs
    a single engine draw and returns the typed value; [sexp_of] renders it on
    the final replay. *)
let leaf ~draw ~sexp_of = Printable { core = Leaf { draw }; sexp_of }

(** [leaf_silent ~draw] builds an unprintable {!Leaf} generator, for leaves
    whose output type has no known printer (e.g. {!just}). *)
let leaf_silent ~draw = Unprintable { core = Leaf { draw } }

(** [with_printer sexp_of gen] attaches (or replaces) [gen]'s printer, yielding a
    printable generator that {!draw} accepts. This is the explicit way to make a
    [map]/[flat_map]/[sampled_from]/[just] result printable. *)
let with_printer : type a p. (a -> Sexp.t) -> (a, p) generator -> (a, printable) generator
  =
  fun sexp_of gen -> Printable { core = core_of gen; sexp_of }
;;

(** [printer gen] is the printer carried by the printable generator [gen]. *)
let printer : type a. (a, printable) generator -> a -> Sexp.t = function
  | Printable { sexp_of; _ } -> sexp_of
  | _ -> .
;;

(** [composite_with_label ~label generate_fn] builds an unprintable generator
    whose [generate_fn] draws run inside a span tagged [label]. Internal: it lets
    the library and the derive PPX tag composites with the right structural label
    (e.g. {!Labels.enum_variant}); user code uses {!composite}, which always tags
    the struct/record label. *)
let composite_with_label ~label generate_fn =
  Unprintable { core = Composite { label; generate_fn } }
;;

(** [composite generate_fn] builds an unprintable generator from an imperative
    [generate_fn] that draws sub-values from the test case and returns a value.
    The draws run inside a {!Labels.fixed_dict} span (the struct/record grouping),
    so they are suppressed on the final replay and only an outer [draw] of the
    whole value prints. *)
let composite generate_fn = composite_with_label ~label:Labels.fixed_dict generate_fn

(** [make_pool_values ~pool ~find ~remove ~is_empty ~consume] builds an
    unprintable generator that picks a value from the engine pool [pool],
    resolving the drawn id via [find]. When [consume], [remove] deletes the
    picked value. [is_empty] reports whether the backing table is empty. *)
let make_pool_values ~pool ~find ~remove ~is_empty ~consume =
  Unprintable { core = Values { pool; find; remove; is_empty; consume } }
;;

(** Maximum number of filter attempts before calling [assume false]. *)
let max_filter_attempts = 3

(** [group label data f] runs [f ()] inside a span with the given [label]. The
    span is stopped with [discard:false] regardless of whether [f] raises.

    A group also increments [draw_depth] for the duration of [f], marking [f]'s
    draws as nested so only the outermost value prints on the final replay. (A
    counter, not a flag, so nested groups compose.) *)
let group label data f =
  Internal.start_span ~label data;
  Internal.incr_draw_depth data;
  Fun.protect
    ~finally:(fun () ->
      Internal.decr_draw_depth data;
      Internal.stop_span data)
    f
;;

(** [discardable_group label data f] runs [f ()] inside a span with [label],
    incrementing [draw_depth] like {!group}. If [f] raises, the span is stopped
    with [discard:true]; otherwise [discard:false]. *)
let discardable_group label data f =
  Internal.start_span ~label data;
  Internal.incr_draw_depth data;
  match f () with
  | v ->
    Internal.decr_draw_depth data;
    Internal.stop_span data;
    v
  | exception e ->
    Internal.decr_draw_depth data;
    Internal.stop_span ~discard:true data;
    raise e
;;

(** A collection handle for generating variable-length sequences.

    Collections ask the engine when to stop generating elements. The [finished]
    flag short-circuits subsequent {!collection_more} calls once the engine
    signals completion. The engine-side collection is created on first use and
    released when the {!with_collection} scope that owns it ends. *)
type collection =
  { mutable finished : bool
  ; mutable handle : Internal.collection option
  ; min_size : int
  ; max_size : int option
  }

(** [get_collection coll data] initializes the engine-side collection and returns its
    handle. Raises {!Internal.Data_exhausted} on StopTest. *)
let get_collection coll data =
  match coll.handle with
  | Some h -> h
  | None ->
    let h =
      Internal.new_collection data ~min_size:coll.min_size ~max_size:coll.max_size
    in
    coll.handle <- Some h;
    h
;;

(** [with_collection ~min_size ?max_size data f] runs [f coll] with a new
    collection and releases the engine-side handle after. *)
let with_collection ~min_size ?max_size data f =
  let coll = { finished = false; handle = None; min_size; max_size } in
  Fun.protect
    ~finally:(fun () ->
      Option.iter
        (fun collection -> Internal.collection_free data ~collection)
        coll.handle)
    (fun () -> f coll)
;;

(** [collection_more coll data] returns [true] if more elements should be
    generated, [false] when the collection is complete. Once it returns [false],
    subsequent calls return [false] immediately. Raises {!Internal.Data_exhausted}
    on StopTest. *)
let collection_more coll data =
  if coll.finished
  then false
  else (
    let collection = get_collection coll data in
    let more = Internal.collection_more data ~collection in
    if not more then coll.finished <- true;
    more)
;;

(** [collection_reject coll data] rejects the last element of the collection.
    No-op if the collection is already finished. Raises {!Internal.Data_exhausted}
    on StopTest. *)
let collection_reject coll data =
  if not coll.finished
  then (
    let collection = get_collection coll data in
    Internal.collection_reject data ~collection)
;;

(* separated out for unit testing *)
let resolve_pool_draw ~find ~remove ~consume variable_id =
  match find variable_id with
  | Some v ->
    if consume then remove variable_id;
    v
  | None ->
    (* State diverged between the engine and the client, or a bug in the
        pool bookkeeping. *)
    raise Internal.Flaky_strategy
;;

let pick tc ~find ~remove ~is_empty pool ~consume =
  Internal.assume tc (not (is_empty ()));
  let variable_id = Internal.pool_generate tc ~pool ~consume () in
  resolve_pool_draw ~find ~remove ~consume variable_id
;;

(** Defined for convenience *)
module Int_table = Stdlib.Hashtbl.Make (struct
    type t = int

    let equal = Int.equal
    let hash = Stdlib.Hashtbl.hash
  end)

(** [Make_pool (Tbl)] specializes the pool-drawing machinery
    ({!make_pool_values}/{!resolve_pool_draw}) to a concrete int-keyed hashtable
    module [Tbl]. *)
module Make_pool (Tbl : Stdlib.Hashtbl.S with type key = int) = struct
  (** [resolve_draw values ~consume variable_id] resolves a drawn pool id
      against the local [values] table, removing it when [consume]. Raises
      [Internal.Flaky_strategy] on an unknown id (an engine-contract
      violation). *)
  let resolve_draw values ~consume variable_id =
    resolve_pool_draw
      ~find:(fun id -> Tbl.find_opt values id)
      ~remove:(fun id -> Tbl.remove values id)
      ~consume
      variable_id
  ;;

  (** [pool_values ~pool ~values ~consume] builds an unprintable generator
      that picks a value from the engine pool [pool], resolving the drawn id
      against the local [values] table. When [consume], the picked value is
      removed from the pool. *)
  let pool_values ~pool ~values ~consume =
    make_pool_values
      ~pool
      ~find:(fun id -> Tbl.find_opt values id)
      ~remove:(fun id -> Tbl.remove values id)
      ~is_empty:(fun () -> Tbl.length values = 0)
      ~consume
  ;;
end

(** [do_draw core data] produces a typed value from generation structure [core]
    using the given test case [data]. *)
let rec do_draw : type a. a core -> Internal.test_case -> a =
  fun core data ->
  match core with
  | Leaf { draw } -> draw data
  | Mapped { source; f } ->
    group Labels.mapped data (fun () ->
      let value = do_draw source data in
      f value)
  | FlatMapped { source; f } ->
    discardable_group Labels.flat_map data (fun () ->
      let first = do_draw source data in
      let second_core = f first in
      do_draw second_core data)
  | Filtered { source; predicate } ->
    let rec attempt i =
      if i > max_filter_attempts
      then raise Internal.Assume_rejected
      else (
        Internal.start_span ~label:Labels.filter data;
        let value = do_draw source data in
        if predicate value
        then (
          Internal.stop_span data;
          value)
        else (
          Internal.stop_span ~discard:true data;
          attempt (i + 1)))
    in
    attempt 1
  | CompositeList { elements; min_size; max_size } ->
    group Labels.list data (fun () ->
      with_collection ~min_size ?max_size data (fun coll ->
        let rec collect acc =
          if collection_more coll data
          then collect (do_draw elements data :: acc)
          else List.rev acc
        in
        collect []))
  | Composite { label; generate_fn } -> group label data (fun () -> generate_fn data)
  | Values { pool; find; remove; is_empty; consume } ->
    pick data ~find ~remove ~is_empty pool ~consume
  | Function { build } -> build ~name:None data
;;

(** [draw_named ~label ~repeatable tc gen] is the naming-aware draw the
    [let%hegel_test] PPX rewrites bindings to; it is not intended for direct use
    (prefer {!draw}). On the final replay of a failing test (or on every case
    under verbose output), an outermost draw prints its value through
    {!note} as [name = value], where [name] is [label], printed bare on
    its sole use and numbered ([label_1], [label_2], …) when [repeatable] is set
    — which the PPX does for a binding name that is reused or drawn in a loop.
    Draws nested inside a span (e.g. composite elements) are suppressed so only
    the outermost value shows. *)
let draw_named
  : type a.
    label:string -> repeatable:bool -> Internal.test_case -> (a, printable) generator -> a
  =
  fun ~label ~repeatable tc gen ->
  match gen with
  | Printable { core = Function { build }; sexp_of } ->
    let name = Internal.draw_display_name tc ~label ~repeatable in
    let value = build ~name:(Some name) tc in
    if Internal.draw_depth tc = 0
    then
      Internal.note
        tc
        (Printf.sprintf "%s = %s" name (Sexp.to_string_hum (sexp_of value)));
    value
  | Printable { core; sexp_of } ->
    let value = do_draw core tc in
    if Internal.draw_depth tc = 0
    then (
      let name = Internal.draw_display_name tc ~label ~repeatable in
      (* Render through Format so the pretty-printer breaks the sexp knowing
         it starts after "name = ": continuation lines align under the value
         instead of landing at column 0. *)
      let rendered = Stdlib.Format.asprintf "%s = %a" name Sexp.pp_hum (sexp_of value) in
      Internal.note tc rendered);
    value
  | _ -> .
;;

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc].

    On the final replay of a failing test (or on every case under verbose
    output), an outermost draw prints its value through {!Internal.note} as
    [name = value]. The [name] is [label] when given, else ["draw"]; an
    unlabeled draw is numbered ([draw_1], [draw_2], …) while a [label] is printed
    bare. Draws nested inside a span (e.g. composite elements) are suppressed so
    only the outermost value shows. To draw a generator that carries no printer,
    use {!draw_silent}, or attach a printer with {!with_printer}. *)
let draw ?label tc gen =
  draw_named
    ~label:(Option.value label ~default:"draw")
    ~repeatable:(Option.is_none label)
    tc
    gen
;;

(** [draw_silent tc gen] produces a typed value from any generator [gen] without
    recording it for the final-replay output. Use it for draws whose value is
    not a useful part of the printed counterexample, or for generators that
    carry no printer. *)
let draw_silent : type a p. Internal.test_case -> (a, p) generator -> a =
  fun tc gen -> do_draw (core_of gen) tc
;;

(** [draw_silent_named ~name tc gen] is {!draw_silent} threading the draw-site
    [name] into a function generator ({!Generators.functions}). Not intended for 
    direct use (prefer {!draw_silent}). *)
let draw_silent_named
  : type a p. name:string -> Internal.test_case -> (a, p) generator -> a
  =
  fun ~name tc gen ->
  match core_of gen with
  | Function { build } -> build ~name:(Some name) tc
  | core -> do_draw core tc
;;

(** [map f gen] transforms values from [gen] using [f]. The result carries no
    printer (the output type is the user's), so it is {!unprintable}; use
    {!with_printer} to make it drawable with {!draw}.

    When [gen]'s core is a [Leaf], the draw closure is composed in place (no
    extra span); otherwise a [Mapped] core is created. *)
let map : type a b p. (a -> b) -> (a, p) generator -> (b, unprintable) generator =
  fun f gen ->
  match core_of gen with
  | Leaf { draw } -> Unprintable { core = Leaf { draw = (fun tc -> f (draw tc)) } }
  | other -> Unprintable { core = Mapped { source = other; f } }
;;

(** [flat_map f gen] creates a dependent generator. [f] receives the generated
    value and returns a generator whose value is the final result. The result
    carries no printer; use {!with_printer} to draw it with {!draw}. *)
let flat_map
  : type a b p q.
    (a -> (b, q) generator) -> (a, p) generator -> (b, unprintable) generator
  =
  fun f gen ->
  Unprintable { core = FlatMapped { source = core_of gen; f = (fun x -> core_of (f x)) } }
;;

(** [filter predicate gen] filters values from [gen] using [predicate], keeping
    [gen]'s printability. Tries up to {!max_filter_attempts} times; calls
    [assume false] if all attempts fail. *)
let filter : type a p. (a -> bool) -> (a, p) generator -> (a, p) generator =
  fun predicate gen ->
  match gen with
  | Printable { core; sexp_of } ->
    Printable { core = Filtered { source = core; predicate }; sexp_of }
  | Unprintable { core } -> Unprintable { core = Filtered { source = core; predicate } }
;;
