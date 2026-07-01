(** This module provides a composable generator API for property-based testing.
    Generators produce typed OCaml values and can be combined using {!map},
    {!flat_map}, and {!filter}.

    Every generator carries a phantom ['p] recording whether it holds a printer.
    Primitive generators are {!printable} and may be drawn with
    {!Hegel.draw}, which prints the drawn value on a failing replay.
    
    All examples in this documentation assume [open Hegel] and [open Hegel.Generators]. *)

(**/**)

(** Constants for span labels used in generation tracking. *)
module Labels : sig
  val list : int
  val list_element : int
  val set : int
  val set_element : int
  val map : int
  val map_entry : int
  val tuple : int
  val one_of : int
  val optional : int
  val fixed_dict : int
  val flat_map : int
  val filter : int
  val mapped : int
  val sampled_from : int
  val enum_variant : int
  val stateful_rule : int
end

(**/**)

(** A generator producing values of type ['a]. The phantom ['p] is {!printable}
    when the generator carries a printer (and so may be drawn with {!Hegel.draw})
    and {!unprintable} otherwise. *)
type ('a, 'p) generator

(** Phantom witness that a generator carries a printer; see {!generator}. *)
type printable

(** Phantom witness that a generator carries no printer; see {!generator}. *)
type unprintable

(**/**)

(** Maximum number of filter attempts before calling [assume false]. *)
val max_filter_attempts : int

(** [group label data f] runs [f ()] inside a span with the given [label]. The
    span is stopped with [discard:false] regardless of whether [f] raises. *)
val group : int -> Client.test_case -> (unit -> 'a) -> 'a

(** [discardable_group label data f] runs [f ()] inside a span with [label]. If
    [f] raises, the span is stopped with [discard:true]; otherwise
    [discard:false]. *)
val discardable_group : int -> Client.test_case -> (unit -> 'a) -> 'a

(** [resolve_draw values ~consume id] resolves a drawn pool [id] against the
    local [values] table, removing it when [consume]. Raises
    {!Client.Flaky_strategy} on an unknown id (an engine-contract violation,
    unreachable through the normal engine-driven path). Exposed only so that
    branch can be unit-tested. *)
val resolve_draw : (int, 'a) Core.Hashtbl.t -> consume:bool -> int -> 'a

(** A collection handle for generating variable-length sequences. *)
type collection =
  { mutable finished : bool
  ; mutable collection_id : int option
  ; min_size : int
  ; max_size : int option
  }

(** [new_collection ~min_size ?max_size data ()] creates a new collection
    handle. *)
val new_collection
  :  min_size:int
  -> ?max_size:int
  -> Client.test_case
  -> unit
  -> collection

(** [collection_more coll data] returns [true] if more elements should be
    generated, [false] when the collection is complete. *)
val collection_more : collection -> Client.test_case -> bool

(** [collection_reject coll data] rejects the last element of the collection.
    Raises {!Client.Data_exhausted} on StopTest. *)
val collection_reject : collection -> Client.test_case -> unit

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc].

    On the final replay of a failing test (or on every case under verbose
    output), an outermost draw prints its value through {!Client.note} as
    [name = value]. The [name] is [label] when given, else ["draw"]; an unlabeled
    draw is numbered ([draw_1], [draw_2], …) while a [label] is printed bare.
    Draws nested inside a span (e.g. composite elements) are suppressed so only
    the outermost value shows. To draw a generator with no printer, use
    {!draw_silent} or attach a printer with {!with_printer}.

    {[
      let%hegel_test draw_example tc =
        let n = draw tc (integers ~min_value:0 ~max_value:100 ()) in
        assert (n >= 0)
      ;;
    ]} *)
val draw : ?label:string -> Client.test_case -> ('a, printable) generator -> 'a

(** [draw_named ~label ~repeatable tc gen] is the naming-aware draw the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw}). It prints [label = value] (bare), or [label_1 = value],
    [label_2 = value], … when [repeatable] is set — which the PPX does for a
    binding name that is reused or drawn in a loop. *)
val draw_named
  :  label:string
  -> repeatable:bool
  -> Client.test_case
  -> ('a, printable) generator
  -> 'a

(** [draw_silent tc gen] produces a typed value from any generator without
    recording it for the final-replay output. Use it for draws whose value is
    not a useful part of the printed counterexample, or for generators that
    carry no printer.

    {[
      let%hegel_test draw_silent_example tc =
        let n = draw_silent tc (map (fun x -> x * 2) (integers ~min_value:0 ~max_value:9 ())) in
        assert (n >= 0)
      ;;
    ]} *)
val draw_silent : Client.test_case -> ('a, 'p) generator -> 'a

(** [with_printer sexp_of gen] attaches (or replaces) [gen]'s printer, yielding
    a printable generator that {!draw} accepts. This is how a [map]/[flat_map]/
    [sampled_from]/[just] result is made drawable with {!draw}

    {[
      let%hegel_test with_printer_example tc =
        let doubled = map (fun x -> x * 2) (integers ~min_value:0 ~max_value:9 ()) in
        let n = draw tc (with_printer Core.Int.sexp_of_t doubled) in
        assert (n >= 0)
      ;;
    ]} *)
val with_printer : ('a -> Core.Sexp.t) -> ('a, 'p) generator -> ('a, printable) generator

(** [printer gen] is the printer carried by the printable generator [gen]. *)
val printer : ('a, printable) generator -> 'a -> Core.Sexp.t

(** [pool_values ~pool_id ~values ~consume] builds a generator that picks a value
    from the engine pool [pool_id], resolving the drawn id against the local
    [values] table. When [consume], the picked value is removed from the pool.
    Carries no printer, so it is {!unprintable}. *)
val pool_values
  :  pool_id:int
  -> values:(int, 'a) Core.Hashtbl.t
  -> consume:bool
  -> ('a, unprintable) generator

(**/**)

(** {2 Generator combinators} *)

(** [composite generate_fn] builds a generator from an imperative [generate_fn]
    that draws sub-values from the test case and assembles a result — the
    OCaml/imperative counterpart to the schema-driven combinators, useful when a
    value is easiest to describe by drawing its parts in sequence (this is also
    the form [@@deriving hegel_generator] emits). Carries no printer (the output type
    is the caller's), so it is {!unprintable}: draw it with {!Hegel.draw_silent},
    or {!Hegel.with_printer} it to draw with {!Hegel.draw}.

    {[
      let point =
        composite (fun tc ->
          let x = draw_silent tc (integers ~min_value:0 ~max_value:9 ()) in
          let y = draw_silent tc (integers ~min_value:0 ~max_value:9 ()) in
          x, y)
      ;;
    ]} *)
val composite : (Client.test_case -> 'a) -> ('a, unprintable) generator

(** [map f gen] transforms values from [gen] using [f]. The result carries no
    printer (the output type is the user's); use {!Hegel.with_printer} to draw it
    with {!Hegel.draw}.

    When [gen]'s core is [Basic], the schema is preserved and transforms are
    composed; otherwise a mapped core is created.

    {[
      let%hegel_test map_example tc =
        let even = draw_silent tc (map (fun x -> x * 2) (integers ~min_value:0 ~max_value:9 ())) in
        assert (even mod 2 = 0)
      ;;
    ]} *)
val map : ('a -> 'b) -> ('a, 'p) generator -> ('b, unprintable) generator

(** [flat_map f gen] creates a dependent generator. [f] receives the generated
    value and returns a generator whose value is the final result. The result
    carries no printer; use {!Hegel.with_printer} to draw it with {!Hegel.draw}.

    {[
      let%hegel_test flat_map_example tc =
        let len_gen = integers ~min_value:0 ~max_value:5 () in
        let xs =
          draw_silent tc
            (flat_map (fun n -> lists (integers ~min_value:0 ~max_value:9 ()) ~min_size:n ~max_size:n ()) len_gen)
        in
        assert (List.length xs <= 5)
      ;;
    ]} *)
val flat_map
  :  ('a -> ('b, 'q) generator)
  -> ('a, 'p) generator
  -> ('b, unprintable) generator

(** [filter predicate gen] filters values from [gen] using [predicate], keeping
    [gen]'s printability. Tries multiple times; calls [assume false] if all
    attempts fail.

    {[
      let%hegel_test filter_example tc =
        let even = draw tc (filter (fun x -> x mod 2 = 0) (integers ~min_value:0 ~max_value:100 ())) in
        assert (even mod 2 = 0)
      ;;
    ]} *)
val filter : ('a -> bool) -> ('a, 'p) generator -> ('a, 'p) generator

(**/**)

(** [schema gen] returns the schema for a [Basic]-core generator, or [None]. *)
val schema : ('a, 'p) generator -> Cbor.t option

(** [is_basic gen] returns [true] if [gen] has a [Basic] core. *)
val is_basic : ('a, 'p) generator -> bool

(** [as_basic gen] returns [Some (schema, transform)] if [gen] has a [Basic]
    core, or [None] otherwise. *)
val as_basic : ('a, 'p) generator -> (Cbor.t * (Cbor.t -> 'a)) option

(** [basic_unique_safe gen] returns [true] iff [gen] has a [Basic] core whose
    transform is known to preserve distinctness over the schema's value space.
    Used to decide whether [lists ~unique:true] can take the engine-side fast
    path or must fall back to client-side dedup. *)
val basic_unique_safe : ('a, 'p) generator -> bool

(**/**)

(** {2 Primitive generators} *)

(** [booleans ()] creates a generator for boolean values.

    {[
      let%hegel_test booleans_example tc =
        let b = draw tc (booleans ()) in
        assert (b = true || b = false)
      ;;
    ]} *)
val booleans : unit -> (bool, printable) generator

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. When a bound is omitted it defaults to the corresponding
    OCaml native [int] limit.

    {[
      let%hegel_test integers_example tc =
        let n = draw tc (integers ~min_value:1 ~max_value:6 ()) in
        assert (n >= 1 && n <= 6)
      ;;
    ]} *)
val integers : ?min_value:int -> ?max_value:int -> unit -> (int, printable) generator

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    Uses schema type ["float"] as required by the Hegel engine. The fields
    [allow_nan], [allow_infinity], [exclude_min], [exclude_max], and [width] are
    always sent (required by the engine). Defaults follow Hypothesis:
    - [allow_nan]: [true] only when no bounds are set
    - [allow_infinity]: [true] when at most one bound is set

    {[
      let%hegel_test floats_example tc =
        let f = draw tc (floats ~min_value:0.0 ~max_value:1.0 ()) in
        assert (Float.compare f 0.0 >= 0)
      ;;
    ]} *)
val floats
  :  ?min_value:float
  -> ?max_value:float
  -> ?exclude_min:bool
  -> ?exclude_max:bool
  -> ?allow_nan:bool
  -> ?allow_infinity:bool
  -> unit
  -> (float, printable) generator

(** [text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ?alphabet ()]
    creates a generator for Unicode text strings.

    Character filtering options restrict which characters may appear:
    - [codec]: restrict to characters encodable in this codec (e.g. ["ascii"],
      ["utf-8"], ["latin-1"])
    - [min_codepoint], [max_codepoint]: restrict Unicode codepoint range
    - [categories]: whitelist of Unicode general categories (e.g.
      [["L"; "Nd"]]). Mutually exclusive with [exclude_categories].
    - [exclude_categories]: blacklist of Unicode general categories. Mutually
      exclusive with [categories].
    - [include_characters]: always include these characters even if excluded by
      other filters
    - [exclude_characters]: always exclude these characters
    - [alphabet]: fixed set of allowed characters. Mutually exclusive with all
      individual character filtering parameters.

    Surrogate codepoints (category Cs) are always excluded since OCaml strings
    are conventionally UTF-8.

    {[
      let%hegel_test text_example tc =
        let s = draw tc (text ~min_size:1 ~max_size:8 ~codec:"ascii" ()) in
        assert (String.length s >= 1)
      ;;
    ]} *)
val text
  :  ?min_size:int
  -> ?max_size:int
  -> ?codec:string
  -> ?min_codepoint:int
  -> ?max_codepoint:int
  -> ?categories:string list
  -> ?exclude_categories:string list
  -> ?include_characters:string
  -> ?exclude_characters:string
  -> ?alphabet:string
  -> unit
  -> (string, printable) generator

(** [characters ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] creates a
    generator for single Unicode characters (as single-character UTF-8 strings).

    Accepts the same character filtering options as {!text} except [min_size],
    [max_size], and [alphabet]. Surrogate codepoints (category Cs) are always
    excluded since OCaml strings are conventionally UTF-8.

    {[
      let%hegel_test characters_example tc =
        let c = draw tc (characters ~codec:"ascii" ()) in
        assert (String.length c >= 1)
      ;;
    ]} *)
val characters
  :  ?codec:string
  -> ?min_codepoint:int
  -> ?max_codepoint:int
  -> ?categories:string list
  -> ?exclude_categories:string list
  -> ?include_characters:string
  -> ?exclude_characters:string
  -> unit
  -> (string, printable) generator

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.

    {[
      let%hegel_test binary_example tc =
        let bytes = draw tc (binary ~min_size:0 ~max_size:16 ()) in
        assert (String.length bytes <= 16)
      ;;
    ]} *)
val binary : ?min_size:int -> ?max_size:int -> unit -> (string, printable) generator

(** [just value] creates a generator that always produces [value]. The output
    type is the caller's, so the result carries no printer.

    {[
      let%hegel_test just_example tc =
        let x = draw_silent tc (just 42) in
        assert (x = 42)
      ;;
    ]} *)
val just : 'a -> ('a, unprintable) generator

(** {2 Collection generators} *)

(** [lists elements ?min_size ?max_size ?unique ()] creates a generator for
    lists of [elements]. When [unique] is [true], elements will be
    distinct.

    {[
      let%hegel_test lists_example tc =
        let xs = draw tc (lists (integers ~min_value:0 ~max_value:9 ()) ~max_size:10 ()) in
        assert (List.length xs <= 10)
      ;;
    ]} *)
val lists
  :  ('a, printable) generator
  -> ?min_size:int
  -> ?max_size:int
  -> ?unique:bool
  -> unit
  -> ('a list, printable) generator

(** [hashmaps keys values ?min_size ?max_size ()] creates a generator for
    dictionaries (hash maps) over printable [keys] and [values]. When both are
    basic generators, uses the engine-side dict schema; when either is
    non-basic, falls back to the collection protocol.

    {[
      let%hegel_test hashmaps_example tc =
        let m =
          draw tc
            (hashmaps (text ~max_size:4 ()) (integers ~min_value:0 ~max_value:9 ()) ~max_size:5 ())
        in
        assert (List.length m <= 5)
      ;;
    ]} *)
val hashmaps
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> (('a * 'b) list, printable) generator

(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values. The output type is the caller's, so the result
    carries no printer.

    {[
      let%hegel_test sampled_from_example tc =
        let color = draw_silent tc (sampled_from [ `Red; `Green; `Blue ]) in
        ignore color
      ;;
    ]} *)
val sampled_from : 'a list -> ('a, unprintable) generator

(** [one_of generators] creates a generator that picks from one of the given
    printable [generators]. Requires at least one generator.

    {[
      let%hegel_test one_of_example tc =
        let n =
          draw tc
            (one_of [ integers ~min_value:0 ~max_value:9 (); integers ~min_value:90 ~max_value:99 () ])
        in
        assert (n >= 0)
      ;;
    ]} *)
val one_of : ('a, printable) generator list -> ('a, printable) generator

(** [optional element] creates a generator that produces either [None] or
    [Some value] from the printable [element].

    {[
      let%hegel_test optional_example tc =
        let o = draw tc (optional (integers ~min_value:0 ~max_value:9 ())) in
        match o with
        | None -> ()
        | Some n -> assert (n >= 0)
      ;;
    ]} *)
val optional : ('a, printable) generator -> ('a option, printable) generator

(** {2 Tuple generators} *)

(** [tuples2 g1 g2] creates a generator for 2-element tuples of printable
    components.

    {[
      let%hegel_test tuples2_example tc =
        let n, b = draw tc (tuples2 (integers ~min_value:0 ~max_value:9 ()) (booleans ())) in
        assert (n >= 0 && (b || not b))
      ;;
    ]} *)
val tuples2
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ('a * 'b, printable) generator

(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples of printable
    components.

    {[
      let%hegel_test tuples3_example tc =
        let a, b, c =
          draw tc
            (tuples3 (integers ~min_value:0 ~max_value:9 ()) (booleans ()) (text ~max_size:4 ()))
        in
        assert (a >= 0 && (b || not b) && String.length c >= 0)
      ;;
    ]} *)
val tuples3
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ('c, printable) generator
  -> ('a * 'b * 'c, printable) generator

(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples of printable
    components.

    {[
      let%hegel_test tuples4_example tc =
        let a, b, c, d =
          draw tc
            (tuples4
               (integers ~min_value:0 ~max_value:9 ())
               (booleans ())
               (text ~max_size:4 ())
               (floats ~min_value:0.0 ~max_value:1.0 ()))
        in
        assert (a >= 0 && (b || not b) && String.length c >= 0 && Float.compare d 0.0 >= 0)
      ;;
    ]} *)
val tuples4
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ('c, printable) generator
  -> ('d, printable) generator
  -> ('a * 'b * 'c * 'd, printable) generator

(** {2 Format generators} *)

(** [emails ()] creates a generator for valid email address strings.

    {[
      let%hegel_test emails_example tc =
        let e = draw tc (emails ()) in
        assert (String.contains e '@')
      ;;
    ]} *)
val emails : unit -> (string, printable) generator

(** [urls ()] creates a generator for valid URL strings.

    {[
      let%hegel_test urls_example tc =
        let u = draw tc (urls ()) in
        assert (String.length u > 0)
      ;;
    ]} *)
val urls : unit -> (string, printable) generator

(** [domains ?max_length ()] creates a generator for domain name strings.

    {[
      let%hegel_test domains_example tc =
        let d = draw tc (domains ~max_length:64 ()) in
        assert (String.length d <= 64)
      ;;
    ]} *)
val domains : ?max_length:int -> unit -> (string, printable) generator

(** [dates ()] creates a generator for ISO 8601 date strings (YYYY-MM-DD).

    {[
      let%hegel_test dates_example tc =
        let d = draw tc (dates ()) in
        assert (String.length d = 10)
      ;;
    ]} *)
val dates : unit -> (string, printable) generator

(** [times ()] creates a generator for time strings.

    {[
      let%hegel_test times_example tc =
        let t = draw tc (times ()) in
        assert (String.length t > 0)
      ;;
    ]} *)
val times : unit -> (string, printable) generator

(** [datetimes ()] creates a generator for ISO 8601 datetime strings.

    {[
      let%hegel_test datetimes_example tc =
        let dt = draw tc (datetimes ()) in
        assert (String.length dt > 0)
      ;;
    ]} *)
val datetimes : unit -> (string, printable) generator

(** [ip_addresses ?version ()] creates a generator for IP address strings.

    {[
      let%hegel_test ip_addresses_example tc =
        let ip = draw tc (ip_addresses ~version:4 ()) in
        assert (String.contains ip '.')
      ;;
    ]} *)
val ip_addresses : ?version:int -> unit -> (string, printable) generator

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern].

    {[
      let%hegel_test from_regex_example tc =
        let s = draw tc (from_regex "[a-z]+" ()) in
        assert (String.length s >= 1)
      ;;
    ]} *)
val from_regex : string -> ?fullmatch:bool -> unit -> (string, printable) generator
