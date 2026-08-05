(** This module provides a composable generator API for property-based testing.
    Generators produce typed OCaml values.

    All examples in this documentation assume [open Hegel], and refer to
    generators as [Generators.foo].

    The usual way to build a compound value is to {!Hegel.draw} its parts in
    sequence:

    {[
      type point = { x : int; y : int }

      let%hegel_test points_stay_in_range tc =
        let x = draw tc (Generators.integers ~min_value:0 ~max_value:100 ()) in
        let y = draw tc (Generators.integers ~min_value:0 ~max_value:100 ()) in
        let p = { x; y } in
        require tc ~msg:"point coordinates stay in range"
          (p.x <= 100 && p.y <= 100)
      ;;
    ]}

    Generator combinators are higher-order functions that take generators and/or
    functions as input and assemble them into a single generator:

    - {!map} transforms every generated value with a function.
    - {!flat_map} builds a {e dependent} generator, where a later generator is
      chosen from an earlier value. For example, draw a length [n], then a list
      of exactly [n] elements.
    - {!filter} keeps only values satisfying a predicate.
    - {!one_of} and {!sampled_from} choose among alternatives.

    Every generator carries a phantom type ['p] recording whether it holds a printer.
    Primitive generators are {!printable} and may be drawn with {!Hegel.draw},
    which prints the drawn value on a failing replay. *)

(** A generator producing values of type ['a]. The phantom ['p] is {!printable}
    when the generator carries a printer (and so may be drawn with {!Hegel.draw})
    and {!unprintable} otherwise. *)
type ('a, 'p) generator

(** Phantom witness that a generator carries a printer; see {!generator}. *)
type printable

(** Phantom witness that a generator carries no printer; see {!generator}.

    A generator is [unprintable] whenever the result type is the caller's rather
    than the engine's, so Hegel has no printer to attach. This covers {!map},
    {!flat_map}, {!sampled_from}, {!just}, {!composite}, and the generators
    produced by [\[@@deriving hegel_generator\]]. An [unprintable] generator
    cannot be drawn with {!Hegel.draw}. There are two ways to use an [unprintable]
    generator:

    - Draw it with {!Hegel.draw_silent}, which produces the value but records
      nothing for the failing-replay output.
    - Attach a printer with {!Hegel.with_printer} to obtain a {!printable}
      generator that can be drawn from with {!Hegel.draw} *)
type unprintable

(**/**)

(** [draw ?label tc gen] produces a typed value from the printable generator
    [gen] using test case [tc].

    On the final replay of a failing test (or on every case under verbose
    output), an outermost draw prints its value through [Internal.note] as
    [name = value]. The [name] is [label] when given, else ["draw"]; an unlabeled
    draw is numbered ([draw_1], [draw_2], …) while a [label] is printed bare.
    Draws nested inside a span (e.g. composite elements) are suppressed so only
    the outermost value shows. To draw a generator with no printer, use
    {!draw_silent} or attach a printer with {!with_printer}.

    Inside a [let%hegel_test], the PPX supplies the binding name as the label, so
    [let x = draw tc gen] prints its value as [x = value]. You rarely pass
    [?label] by hand. When the same name is shadowed or drawn in a loop, its
    draws are numbered [x_1], [x_2], … in draw order; pass [?label] to override
    the name (e.g. [draw ~label:"y" tc gen]).

    {[
      let%hegel_test draw_example tc =
        let n = draw tc (Generators.integers ~min_value:0 ~max_value:100 ()) in
        assert (n >= 0)
      ;;
    ]} *)
val draw : ?label:string -> Internal.test_case -> ('a, printable) generator -> 'a

(** [draw_named ~label ~repeatable tc gen] is the naming-aware draw the
    [let%hegel_test] PPX rewrites bindings to; not intended for direct use
    (prefer {!draw}). It prints [label = value] (bare), or [label_1 = value],
    [label_2 = value], … when [repeatable] is set. The PPX sets [repeatable = true]  
    for a binding name that is reused or drawn in a loop. *)
val draw_named
  :  label:string
  -> repeatable:bool
  -> Internal.test_case
  -> ('a, printable) generator
  -> 'a

(** [draw_silent tc gen] produces a typed value from any generator without
    recording it for the final-replay output. Use it for draws whose value is
    not a useful part of the printed counterexample, or for generators that
    carry no printer.

    {[
      let%hegel_test draw_silent_example tc =
        let n =
          draw_silent
            tc
            (Generators.map (fun x -> x * 2) (Generators.integers ~min_value:0 ~max_value:9 ()))
        in
        assert (n >= 0)
      ;;
    ]} *)
val draw_silent : Internal.test_case -> ('a, 'p) generator -> 'a

(** [draw_silent_named ~name tc gen] is {!draw_silent} threading the draw-site
    [name] into a function generator ({!Generators.functions}). Not intended for 
    direct use (prefer {!draw_silent}). *)
val draw_silent_named : name:string -> Internal.test_case -> ('a, 'p) generator -> 'a

(** [with_printer sexp_of gen] attaches (or replaces) [gen]'s printer, yielding
    a printable generator that {!draw} accepts. This is how a [map]/[flat_map]/
    [sampled_from]/[just] result is made drawable with {!draw}.

    The printer is any ['a -> Sexplib0.Sexp.t].

    With [ppx_sexp_conv] in your preprocessor, [\[%sexp_of: t\]] is the idiomatic
    shorthand for that function

    {[
      let%hegel_test with_printer_example tc =
        let doubled =
          Generators.map (fun x -> x * 2) (Generators.integers ~min_value:0 ~max_value:9 ())
        in
        (* [%sexp_of: int] is shorthand for the int printer. *)
        let n = draw tc (Generators.with_printer [%sexp_of: int] doubled) in
        assert (n >= 0)
      ;;
    ]} *)
val with_printer
  :  ('a -> Sexplib0.Sexp.t)
  -> ('a, 'p) generator
  -> ('a, printable) generator

(** [printer gen] is the printer carried by the printable generator [gen]. *)
val printer : ('a, printable) generator -> 'a -> Sexplib0.Sexp.t

(**/**)

(** {2 Primitive generators} *)

(** [booleans ()] creates a generator for boolean values.

    {[
      let%hegel_test booleans_example tc =
        let b = draw tc (Generators.booleans ()) in
        assert (b = true || b = false)
      ;;
    ]} *)
val booleans : unit -> (bool, printable) generator

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds.

    Defaults:
    - [min_value]: OCaml native int min
    - [max_value]: OCaml native int max

    {[
      let%hegel_test integers_example tc =
        let n = draw tc (Generators.integers ~min_value:1 ~max_value:6 ()) in
        assert (n >= 1 && n <= 6)
      ;;
    ]} *)
val integers : ?min_value:int -> ?max_value:int -> unit -> (int, printable) generator

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    Defaults:
    - [min_value]: 64-bit float min (only when both [allow_nan] and
      [allow_infinity] are [false])
    - [max_value]: 64-bit float max (only when both [allow_nan] and
      [allow_infinity] are [false])
    - [exclude_min]: [false]
    - [exclude_max]: [false]
    - [allow_nan]: [true] only when no bounds are set
    - [allow_infinity]: [true] when at most one bound is set

    {[
      let%hegel_test floats_example tc =
        let f = draw tc (Generators.floats ~min_value:0.0 ~max_value:1.0 ()) in
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
        let s = draw tc (Generators.text ~min_size:1 ~max_size:8 ~codec:"ascii" ()) in
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
        let c = draw tc (Generators.characters ~codec:"ascii" ()) in
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
        let bytes = draw tc (Generators.binary ~min_size:0 ~max_size:16 ()) in
        assert (String.length bytes <= 16)
      ;;
    ]} *)
val binary : ?min_size:int -> ?max_size:int -> unit -> (string, printable) generator

(** [just value] creates a generator that always produces [value]. The output
    type is the caller's, so the result carries no printer.

    {[
      let%hegel_test just_example tc =
        let x = draw_silent tc (Generators.just 42) in
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
        let xs =
          draw tc (Generators.lists (Generators.integers ~min_value:0 ~max_value:9 ()) ~max_size:10 ())
        in
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

(** [assoc_lists keys values ?min_size ?max_size ()] creates a
    generator for association lists over printable [keys] and [values]:
    [(key, value)] pairs, in generation order, whose keys are unique.

    {[
      let%hegel_test assoc_lists_example tc =
        let m =
          draw tc
            (Generators.assoc_lists
               (Generators.text ~max_size:4 ())
               (Generators.integers ~min_value:0 ~max_value:9 ())
               ~max_size:5
               ())
        in
        assert (List.length m <= 5)
      ;;
    ]} *)
val assoc_lists
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> (('a * 'b) list, printable) generator

(** [hash_tables_core ~of_pairs ~sexp_of_t keys values ?min_size ?max_size ()]
    builds a hash-table generator over any table type ['t]. *)
val hash_tables_core
  :  of_pairs:(('a * 'b) list -> 't)
  -> sexp_of_t:
       (('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> 't -> Sexplib0.Sexp.t)
  -> ('a, printable) generator
  -> ('b, printable) generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> ('t, printable) generator

(** [hash_tables keys values ?min_size ?max_size ()] creates a generator for
    [Stdlib.Hashtbl.t] tables over printable [keys] and [values]

    {[
      let%hegel_test hash_tables_example tc =
        let m =
          draw tc
            (Generators.hash_tables
               (Generators.text ~max_size:4 ())
               (Generators.integers ~min_value:0 ~max_value:9 ())
               ~max_size:5
               ())
        in
        assert (Hashtbl.length m <= 5)
      ;;
    ]} *)
val hash_tables
  :  ('a, printable) generator
  -> ('b, printable) generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> (('a, 'b) Stdlib.Hashtbl.t, printable) generator

(** [sampled_from options] creates a generator that samples from a non-empty
    list of values. Sampling is {e not} uniform: the engine's bounded-integer
    draw deliberately over-weights boundary and "interesting" indices, so the
    first element (and, to a lesser extent, the last) is drawn noticeably more
    often than the middle ones, matching the sibling Hegel client libraries.
    The output type is the caller's, so the result carries no printer.

    {[
      let%hegel_test sampled_from_example tc =
        let color = draw_silent tc (Generators.sampled_from [ `Red; `Green; `Blue ]) in
        ignore color
      ;;
    ]} *)
val sampled_from : 'a list -> ('a, unprintable) generator

(** [one_of generators] creates a generator that picks from one of the given
    printable [generators]. Requires at least one generator. On a failing
    replay, the drawn value prints through the printer of the branch it was
    drawn from. The recorded branch is per generator value: if one [one_of]
    generator is drawn several times before printing (e.g. as the element
    generator of {!lists}), every value prints through the most recently drawn
    branch's printer.

    {[
      let%hegel_test one_of_example tc =
        let n =
          draw tc
            (Generators.one_of
               [ Generators.integers ~min_value:0 ~max_value:9 ()
               ; Generators.integers ~min_value:90 ~max_value:99 ()
               ])
        in
        assert (n >= 0)
      ;;
    ]} *)
val one_of : ('a, printable) generator list -> ('a, printable) generator

(** [optional gen] creates a generator that produces either [None] or
    [Some value] from the printable [gen].

    {[
      let%hegel_test optional_example tc =
        let o = draw tc (Generators.optional (Generators.integers ~min_value:0 ~max_value:9 ())) in
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
        let n, b =
          draw tc
            (Generators.tuples2 (Generators.integers ~min_value:0 ~max_value:9 ()) (Generators.booleans ()))
        in
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
            (Generators.tuples3
               (Generators.integers ~min_value:0 ~max_value:9 ())
               (Generators.booleans ())
               (Generators.text ~max_size:4 ()))
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
            (Generators.tuples4
               (Generators.integers ~min_value:0 ~max_value:9 ())
               (Generators.booleans ())
               (Generators.text ~max_size:4 ())
               (Generators.floats ~min_value:0.0 ~max_value:1.0 ()))
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

(** {2 Function generators} *)

(** [functions ?name ?sexp_of_arg ~returns ()] creates a generator for functions
    ['a -> 'b] whose results are drawn from [returns].

    The result carries no printer (its output type is a function), so draw it
    with {!Hegel.draw_silent}. Applying the drawn function to an argument draws a
    result from [returns] the first time that argument is seen and memoizes it.

    On the failing final replay each top-level application prints as
    [name arg = result] (applications nested inside a span are suppressed).
    [name] is [?name] when you pass it. Otherwise, the draw-site binding name 
    inside a [let%hegel_test], else ["function"].

    The memo table keys on the argument itself (structural hash/equality), so
    distinct arguments always get independent results. [sexp_of_arg] only renders
    the argument in the shown pair. An omitted [sexp_of_arg] or a non-printable
    [returns] shows [<opaque>].

    {[
      let%hegel_test map_length_preserved tc =
        let f_gen =
          Generators.functions ~sexp_of_arg:[%sexp_of: int] ~returns:(Generators.integers ()) ()
        in
        let f = draw_silent tc f_gen in
        let xs = draw tc (Generators.lists (Generators.integers ()) ()) in
        assert (List.length (List.map f xs) = List.length xs)
      ;;
    ]} *)
val functions
  :  ?name:string
  -> ?sexp_of_arg:('a -> Sexplib0.Sexp.t)
  -> returns:('b, _) generator
  -> unit
  -> ('a -> 'b, unprintable) generator

(** [functions2 ?name ?sexp_of_arg1 ?sexp_of_arg2 ~returns ()] creates a
    generator for curried two-argument functions ['a -> 'b -> 'c].

    Sugar over {!functions} keyed on the argument pair: the two arguments form
    one memo key and are shown uncurried as [name (arg1 arg2) = result]. Draw it
    with {!draw_silent}. *)
val functions2
  :  ?name:string
  -> ?sexp_of_arg1:('a -> Sexplib0.Sexp.t)
  -> ?sexp_of_arg2:('b -> Sexplib0.Sexp.t)
  -> returns:('c, _) generator
  -> unit
  -> ('a -> 'b -> 'c, unprintable) generator

(** [functions3 ?name ?sexp_of_arg1 ?sexp_of_arg2 ?sexp_of_arg3 ~returns ()]
    creates a generator for curried three-argument functions
    ['a -> 'b -> 'c -> 'd].

    Like {!functions2}, keyed on the argument triple and shown uncurried as
    [name (arg1 arg2 arg3) = result]. Draw it with {!draw_silent}. *)
val functions3
  :  ?name:string
  -> ?sexp_of_arg1:('a -> Sexplib0.Sexp.t)
  -> ?sexp_of_arg2:('b -> Sexplib0.Sexp.t)
  -> ?sexp_of_arg3:('c -> Sexplib0.Sexp.t)
  -> returns:('d, _) generator
  -> unit
  -> ('a -> 'b -> 'c -> 'd, unprintable) generator

(** {2 Format generators} *)

(** [emails ()] creates a generator for valid email address strings.

    Addresses follow RFC 5321/5322: a local part of 1 to 64 characters from the
    RFC 5322 [atext] set, an [@], and a domain from {!domains}, with the overall
    address length capped at 254 octets (RFC 5321 §4.5.3.1.3).

    {[
      let%hegel_test emails_example tc =
        let e = draw tc (Generators.emails ()) in
        assert (String.contains e '@')
      ;;
    ]} *)
val emails : unit -> (string, printable) generator

(** [urls ()] creates a generator for valid URL strings.

    URLs follow RFC 3986, of the form
    [scheme://domain\[:port\]/path\[#fragment\]] with [scheme] one of
    [http]/[https], the domain drawn from {!domains} (up to 255 characters), an
    optional port in [1, 65535], zero or more [/]-separated path segments of up
    to 100 characters each, and an optional fragment of up to 100 characters.
    Path and fragment characters are percent-encoded.

    {[
      let%hegel_test urls_example tc =
        let u = draw tc (Generators.urls ()) in
        assert (String.length u > 0)
      ;;
    ]} *)
val urls : unit -> (string, printable) generator

(** [domains ?max_length ()] creates a generator for domain name strings.

    Domains are RFC 1035 fully-qualified domain names: a top-level domain
    sampled from the IANA TLD list followed by up to 126 dot-separated labels,
    each 1 to 63 characters matching
    [\[a-zA-Z\](\[a-zA-Z0-9-\]{0,61}\[a-zA-Z0-9\])?] (punycode [xn--] labels
    reserved by RFC 5890 are excluded). Generated domains never exceed
    [max_length] (default 255, per RFC 1035 §2.3.4); when provided, [max_length]
    must be in [4, 255].

    {[
      let%hegel_test domains_example tc =
        let d = draw tc (Generators.domains ~max_length:64 ()) in
        assert (String.length d <= 64)
      ;;
    ]} *)
val domains : ?max_length:int -> unit -> (string, printable) generator

(** [dates_core ~of_parts ~sexp_of ()] builds a date generator over any date
    representation.

    {[
      let core_dates =
        Generators.dates_core
          ~of_parts:(fun ~year ~month ~day ->
            Core.Date.create_exn ~y:year ~m:(Core.Month.of_int_exn month) ~d:day)
          ~sexp_of:Core.Date.sexp_of_t
          ()
    ]} *)
val dates_core
  :  of_parts:(year:int -> month:int -> day:int -> 'a)
  -> sexp_of:('a -> Sexplib0.Sexp.t)
  -> unit
  -> ('a, printable) generator

(** [times_core ~of_parts ~sexp_of ()] builds a time-of-day generator over any
    time representation. *)
val times_core
  :  of_parts:(hour:int -> minute:int -> second:int -> microsecond:int -> 'a)
  -> sexp_of:('a -> Sexplib0.Sexp.t)
  -> unit
  -> ('a, printable) generator

(** [datetimes_core ~of_parts ~sexp_of ()] builds a naive-datetime generator
    over any representation. *)
val datetimes_core
  :  of_parts:
       (year:int
        -> month:int
        -> day:int
        -> hour:int
        -> minute:int
        -> second:int
        -> microsecond:int
        -> 'a)
  -> sexp_of:('a -> Sexplib0.Sexp.t)
  -> unit
  -> ('a, printable) generator

(** [dates ()] creates a generator for calendar dates as ISO 8601 [YYYY-MM-DD]
    strings, with year in [\[1, 9999\]] and calendar-valid month/day.

    {[
      let%hegel_test dates_example tc =
        let d = draw tc (Generators.dates ()) in
        assert (String.length d = 10)
      ;;
    ]} *)
val dates : unit -> (string, printable) generator

(** [times ()] creates a generator for times of day as ISO 8601 [HH:MM:SS.ffffff] 
    strings.

    {[
      let%hegel_test times_example tc =
        let t = draw tc (Generators.times ()) in
        assert (String.length t = 15)
      ;;
    ]} *)
val times : unit -> (string, printable) generator

(** [datetimes ()] creates a generator for naive datetimes as ISO 8601
    [YYYY-MM-DDTHH:MM:SS.ffffff] strings.

    {[
      let%hegel_test datetimes_example tc =
        let dt = draw tc (Generators.datetimes ()) in
        assert (String.contains dt 'T')
      ;;
    ]} *)
val datetimes : unit -> (string, printable) generator

(** [ip_addresses ?version ()] creates a generator for typed [Ipaddr.t] IP
    addresses. [version] selects IPv4 ([`V4], RFC 791) or IPv6 ([`V6],
    RFC 4291); when omitted, either version is generated. Render a drawn
    address with [Ipaddr.to_string] (RFC 5952 canonical form for v6).

    {[
      let%hegel_test ip_addresses_example tc =
        match draw tc (Generators.ip_addresses ~version:`V4 ()) with
        | Ipaddr.V4 _ as ip -> assert (String.contains (Ipaddr.to_string ip) '.')
        | Ipaddr.V6 _ -> assert false
      ;;
    ]} *)
val ip_addresses : ?version:[ `V4 | `V6 ] -> unit -> (Ipaddr.t, printable) generator

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern], written in the syntax of Python's [re]
    module. When [fullmatch] is [true] (the default) the whole string must match
    [pattern]; otherwise a match anywhere suffices.

    {[
      let%hegel_test from_regex_example tc =
        let s = draw tc (Generators.from_regex "[a-z]+" ()) in
        assert (String.length s >= 1)
      ;;
    ]} *)
val from_regex : string -> ?fullmatch:bool -> unit -> (string, printable) generator

(** {2 Generator combinators} *)

(** [composite generate_fn] builds a generator from an imperative [generate_fn]
    that draws sub-values from the test case and assembles a result, useful when
    a value is easiest to describe by drawing its parts in sequence.

    {[
      let point =
        Generators.composite (fun tc ->
          let x = draw_silent tc (Generators.integers ~min_value:0 ~max_value:9 ()) in
          let y = draw_silent tc (Generators.integers ~min_value:0 ~max_value:9 ()) in
          x, y)
      ;;
    ]} *)
val composite : (Internal.test_case -> 'a) -> ('a, unprintable) generator

(** [map f gen] transforms values from [gen] using [f].

    {[
      let%hegel_test map_example tc =
        let even =
          draw_silent tc (Generators.map (fun x -> x * 2) (Generators.integers ~min_value:0 ~max_value:9 ()))
        in
        assert (even mod 2 = 0)
      ;;
    ]} *)
val map : ('a -> 'b) -> ('a, 'p) generator -> ('b, unprintable) generator

(** [flat_map f gen] creates a dependent generator. [f] receives the generated
    value and returns a generator whose value is the final result.

    {[
      let%hegel_test flat_map_example tc =
        let len_gen = Generators.integers ~min_value:0 ~max_value:5 () in
        let xs =
          draw_silent tc
            (Generators.flat_map
               (fun n ->
                  Generators.lists
                    (Generators.integers ~min_value:0 ~max_value:9 ())
                    ~min_size:n
                    ~max_size:n
                    ())
               len_gen)
        in
        assert (List.length xs <= 5)
      ;;
    ]} *)
val flat_map
  :  ('a -> ('b, 'q) generator)
  -> ('a, 'p) generator
  -> ('b, unprintable) generator

(** [filter predicate gen] filters values from [gen] using [predicate], keeping
    [gen]'s printability. Tries up to three times and rejects the test case if all
    attempts fail.

    {[
      let%hegel_test filter_example tc =
        let even =
          draw tc (Generators.filter (fun x -> x mod 2 = 0) (Generators.integers ~min_value:0 ~max_value:100 ()))
        in
        assert (even mod 2 = 0)
      ;;
    ]} *)
val filter : ('a -> bool) -> ('a, 'p) generator -> ('a, 'p) generator

(** {2 Pool generators}

    The engine-pool generator ([Stateful.Pool]) resolves engine-drawn variable
    ids against the client-side pool.*)

(** Defined for convenience *)
module Int_table : Stdlib.Hashtbl.S with type key = int

(** [Make_pool (Tbl)] specializes the pool-drawing machinery to the concrete
    int-keyed hashtable module [Tbl]. *)
module Make_pool (Tbl : Stdlib.Hashtbl.S with type key = int) : sig
  val resolve_draw : 'a Tbl.t -> consume:bool -> int -> 'a

  val pool_values
    :  pool_id:int
    -> values:'a Tbl.t
    -> consume:bool
    -> ('a, unprintable) generator
end

(**/**)

(** Internal plumbing consumed by the [ppx_hegel_generator] deriver, the
    library's own modules, and white-box tests. Not for direct use — no
    stability guarantees. *)
module Ppx_internal : sig
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

  (** Maximum number of filter attempts before calling [assume false]. *)
  val max_filter_attempts : int

  (** [group label data f] runs [f ()] inside a span with the given [label]. The
      span is stopped with [discard:false] regardless of whether [f] raises. *)
  val group : int -> Internal.test_case -> (unit -> 'a) -> 'a

  (** [discardable_group label data f] runs [f ()] inside a span with [label].
      If [f] raises, the span is stopped with [discard:true]; otherwise
      [discard:false]. *)
  val discardable_group : int -> Internal.test_case -> (unit -> 'a) -> 'a

  (** [resolve_draw_core ~find ~remove ~consume id] resolves a drawn pool [id]
      via the [find] closure. When [consume], the picked value is removed. *)
  val resolve_draw_core
    :  find:(int -> 'a option)
    -> remove:(int -> unit)
    -> consume:bool
    -> int
    -> 'a

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
    -> Internal.test_case
    -> unit
    -> collection

  (** [collection_more coll data] returns [true] if more elements should be
      generated, [false] when the collection is complete. *)
  val collection_more : collection -> Internal.test_case -> bool

  (** [collection_reject coll data] rejects the last element of the collection.
      Raises [Internal.Data_exhausted] on StopTest. *)
  val collection_reject : collection -> Internal.test_case -> unit

  (** [values_core ~pool_id ~find ~remove ~is_empty ~consume] is a
      generator that picks a value from the engine pool [pool_id]. When [consume],
      the picked value is removed from the pool. *)
  val values_core
    :  pool_id:int
    -> find:(int -> 'a option)
    -> remove:(int -> unit)
    -> is_empty:(unit -> bool)
    -> consume:bool
    -> ('a, unprintable) generator

  (** [composite_with_label ~label generate_fn] is {!composite} but tags the
      span with [label] instead of the fixed struct/record label. Used by the
      derive PPX to tag composites (e.g. {!Labels.enum_variant}). *)
  val composite_with_label
    :  label:int
    -> (Internal.test_case -> 'a)
    -> ('a, unprintable) generator
end

(**/**)
