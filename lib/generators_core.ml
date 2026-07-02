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

  (* Labels 1-15 above mirror the engine's `HEGEL_LABEL_*` generator
     constants. 16 is the engine's internal `HEGEL_LABEL_FEATURE_FLAG` (swarm
     rule-enable flags), so the stateful-rule span must avoid it: sharing a
     label makes span mutation lump per-step spans together with per-flag spans
     into one mixed-length group, whose length-changing mutations shift the
     replayed choice sequence and misalign stop signals. Use 17, the first free
     label past the engine's range. *)
  let _feature_flag = 16
  let stateful_rule = 17
end

(** The pure generation structure of a generator, carrying no printer. A
    {!generator} is a {!core} paired (or not) with a printer; see {!generator}.

    - [Basic] cores hold a raw schema and a mandatory client-side transform.
      Mapping a [Basic] preserves the schema and composes transforms. The
      [unique_safe] flag tracks whether [transform] is known to preserve
      distinctness over the schema's value space (i.e. is injective); leaf
      generators set it to [true], and [map] sets it to [false] since the
      user's function may collapse distinct inputs.
    - [Mapped] cores wrap a source and a transform function.
    - [FlatMapped] cores wrap a source and a function returning a core.
    - [Filtered] cores wrap a source and a predicate.
    - [CompositeList] cores use the collection protocol to generate lists of
      non-basic elements, creating a fresh collection per generate call.
    - [Composite] cores wrap a [generate_fn] thunk inside a span with the given
      [label]. Used for tuples and one_of with non-basic elements. *)
type 'a core =
  | Basic :
      { schema : Cbor.t
      ; transform : Cbor.t -> 'a
      ; unique_safe : bool
      }
      -> 'a core
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
      { pool_id : int
      ; values : (int, 'a) Base.Hashtbl.t
      ; consume : bool
      }
      -> 'a core

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

(** [basic ~schema ~transform ~sexp_of ?unique_safe ()] builds a printable
    {!Basic} generator. [sexp_of] is the printer used to render a drawn value on
    the final replay. [unique_safe] defaults to [true] (leaf generators preserve
    distinctness); set it to [false] for transforms that may collapse distinct
    inputs. *)
let basic ~schema ~transform ~sexp_of ?(unique_safe = true) () =
  Printable { core = Basic { schema; transform; unique_safe }; sexp_of }
;;

(** [basic_silent ~schema ~transform ()] builds an unprintable {!Basic}
    generator, for leaves whose output type has no known printer (e.g. {!just}).
    Its transform is treated as not distinctness-preserving ([unique_safe =
    false]); such leaves are constants today, which collapse all inputs. *)
let basic_silent ~schema ~transform () =
  Unprintable { core = Basic { schema; transform; unique_safe = false } }
;;

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

(** [composite generate_fn] builds an unprintable generator from an imperative
    [generate_fn] that draws sub-values from the test case and returns a value.
    The draws run inside a span, so they are suppressed on the final replay and
    only an outer [draw] of the whole value prints. *)
let composite generate_fn =
  Unprintable { core = Composite { label = Labels.fixed_dict; generate_fn } }
;;

(** [pool_values ~pool_id ~values ~consume] builds an unprintable generator that
    picks a value from the engine pool [pool_id], resolving the drawn id against
    the local [values] table. When [consume], the picked value is removed from
    the pool. Carries no printer (the output type is the caller's). *)
let pool_values ~pool_id ~values ~consume =
  Unprintable { core = Values { pool_id; values; consume } }
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
  Exn.protect
    ~finally:(fun () ->
      Internal.decr_draw_depth data;
      Internal.stop_span data)
    ~f
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
    and returns its id. Raises {!Internal.Data_exhausted} on StopTest. *)
let get_collection_id coll data =
  match coll.collection_id with
  | Some id -> id
  | None ->
    let id =
      Internal.new_collection data ~min_size:coll.min_size ~max_size:coll.max_size
    in
    coll.collection_id <- Some id;
    id
;;

(** [collection_more coll data] returns [true] if more elements should be
    generated, [false] when the collection is complete. Once it returns [false],
    subsequent calls return [false] immediately. Raises {!Internal.Data_exhausted}
    on StopTest. *)
let collection_more coll data =
  if coll.finished
  then false
  else (
    let collection_id = get_collection_id coll data in
    let more = Internal.collection_more data ~collection_id in
    if not more then coll.finished <- true;
    more)
;;

(** [collection_reject coll data] rejects the last element of the collection.
    No-op if the collection is already finished. Raises {!Internal.Data_exhausted}
    on StopTest. *)
let collection_reject coll data =
  if not coll.finished
  then (
    let collection_id = get_collection_id coll data in
    Internal.collection_reject data ~collection_id)
;;

(* separated out for unit testing *)
let resolve_draw values ~consume variable_id =
  match Hashtbl.find values variable_id with
  | Some v ->
    if consume then Hashtbl.remove values variable_id;
    v
  | None ->
    (* State diverged between the engine and the client, or a bug in the
        pool bookkeeping. *)
    raise Internal.Flaky_strategy
;;

let pick tc values pool_id ~consume =
  Internal.assume tc (not (Hashtbl.is_empty values));
  let variable_id = Internal.pool_generate tc ~pool_id ~consume () in
  let value = resolve_draw values ~consume variable_id in
  value
;;

(** [do_draw core data] produces a typed value from generation structure [core]
    using the given test case [data]. *)
let rec do_draw : type a. a core -> Internal.test_case -> a =
  fun core data ->
  match core with
  | Basic { schema; transform; _ } ->
    transform (Internal.generate_from_schema schema data)
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
      let coll = new_collection ~min_size ?max_size data () in
      let rec collect acc =
        if collection_more coll data
        then collect (do_draw elements data :: acc)
        else List.rev acc
      in
      collect [])
  | Composite { label; generate_fn } -> group label data (fun () -> generate_fn data)
  | Values { pool_id; values; consume } -> pick data values pool_id ~consume
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
  | Printable { core; sexp_of } ->
    let value = do_draw core tc in
    if Internal.draw_depth tc = 0
    then (
      let name = Internal.draw_display_name tc ~label ~repeatable in
      let rendered = Sexp.to_string_hum (sexp_of value) in
      Internal.note tc (sprintf "%s = %s" name rendered));
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

(** [map f gen] transforms values from [gen] using [f]. The result carries no
    printer (the output type is the user's), so it is {!unprintable}; use
    {!with_printer} to make it drawable with {!draw}.

    When [gen]'s core is [Basic], the schema is preserved and transforms are
    composed; otherwise a [Mapped] core is created. *)
let map : type a b p. (a -> b) -> (a, p) generator -> (b, unprintable) generator =
  fun f gen ->
  match core_of gen with
  | Basic { schema; transform; _ } ->
    (* The user's [f] may collapse distinct inputs, so the composed transform
       is no longer known to preserve uniqueness. *)
    Unprintable
      { core =
          Basic { schema; transform = (fun x -> f (transform x)); unique_safe = false }
      }
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

(** [schema_core core] returns the schema for a [Basic] core, or [None]. *)
let schema_core : type a. a core -> Cbor.t option = function
  | Basic { schema; _ } -> Some schema
  | _ -> None
;;

(** [schema gen] returns the schema for a [Basic] generator, or [None]. *)
let schema : type a p. (a, p) generator -> Cbor.t option =
  fun gen -> schema_core (core_of gen)
;;

(** [is_basic gen] returns [true] if [gen] has a [Basic] core. *)
let is_basic : type a p. (a, p) generator -> bool =
  fun gen ->
  match core_of gen with
  | Basic _ -> true
  | _ -> false
;;

(** [as_basic_core core] returns [Some (schema, transform)] if [core] is
    [Basic], or [None] otherwise. *)
let as_basic_core : type a. a core -> (Cbor.t * (Cbor.t -> a)) option = function
  | Basic { schema; transform; _ } -> Some (schema, transform)
  | _ -> None
;;

(** [as_basic gen] returns [Some (schema, transform)] if [gen] has a [Basic]
    core, or [None] otherwise. *)
let as_basic : type a p. (a, p) generator -> (Cbor.t * (Cbor.t -> a)) option =
  fun gen -> as_basic_core (core_of gen)
;;

(** [basic_unique_safe gen] returns [true] iff [gen] has a [Basic] core whose
    transform is known to preserve distinctness (i.e. is injective over the
    schema's value space). Used by [lists ~unique:true] to decide between the
    engine-side fast path and the client-side dedup fallback. *)
let basic_unique_safe : type a p. (a, p) generator -> bool =
  fun gen ->
  match core_of gen with
  | Basic { unique_safe; _ } -> unique_safe
  | _ -> false
;;
