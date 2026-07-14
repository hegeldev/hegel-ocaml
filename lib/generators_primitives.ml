open! Core
open Generators_core

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. When a bound is omitted it defaults to the corresponding
    OCaml native [int] limit. *)
let integers ?(min_value = Int.min_value) ?(max_value = Int.max_value) () =
  if min_value > max_value
  then
    raise
      (Invalid_argument
         (sprintf "Cannot have max_value=%d < min_value=%d" max_value min_value));
  leaf
    ~draw:(fun tc -> Internal.generate_integer tc ~min_value ~max_value)
    ~sexp_of:sexp_of_int
;;

(** [booleans ()] creates a generator for boolean values. *)
let booleans () =
  leaf ~draw:(fun tc -> Internal.generate_boolean tc 0.5 None) ~sexp_of:sexp_of_bool
;;

(** The smallest positive (subnormal) 64-bit float. Passed as
    [smallest_nonzero_magnitude] so the engine places no magnitude restriction on
    drawn floats. *)
let smallest_nonzero_magnitude = 5e-324

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    Unbounded ends are sent to the engine as [neg_infinity] / [infinity].

    Defaults:
    - [exclude_min]: [false]
    - [exclude_max]: [false]
    - [allow_nan]: [true] only when no bounds are set
    - [allow_infinity]: [true] when at most one bound is set *)
let floats
      ?min_value
      ?max_value
      ?(exclude_min = false)
      ?(exclude_max = false)
      ?allow_nan
      ?allow_infinity
      ()
  =
  let has_min = Option.is_some min_value in
  let has_max = Option.is_some max_value in
  let eff_allow_nan =
    match allow_nan with
    | Some v -> v
    | None -> (not has_min) && not has_max
  in
  let eff_allow_infinity =
    match allow_infinity with
    | Some v -> v
    | None -> (not has_min) || not has_max
  in
  if eff_allow_nan && (has_min || has_max)
  then raise (Invalid_argument "Cannot have allow_nan=true with min_value or max_value");
  (match min_value, max_value with
   | Some min, Some max when Float.( > ) min max ->
     raise
       (Invalid_argument
          (sprintf "There are no floats between min_value=%g and max_value=%g" min max))
   | _ -> ());
  if eff_allow_infinity && has_min && has_max
  then
    raise
      (Invalid_argument
         "Cannot have allow_infinity=true with both min_value and max_value");
  let min_value = Option.value min_value ~default:Float.neg_infinity in
  let max_value = Option.value max_value ~default:Float.infinity in
  leaf
    ~draw:(fun tc ->
      Internal.generate_float
        tc
        ~min_value
        ~max_value
        ~allow_nan:eff_allow_nan
        ~allow_infinity:eff_allow_infinity
        ~exclude_min
        ~exclude_max
        ~smallest_nonzero_magnitude)
    ~sexp_of:sexp_of_float
;;

(** Unicode general categories that include surrogate codepoints. OCaml strings
    are conventionally UTF-8, and surrogates in UTF-8 are ill-formed. *)
let surrogate_categories = [ "Cs"; "C" ]

(** [effective_categories ?categories ?exclude_categories ()] applies the
    character-category validation and surrogate auto-exclusion shared by {!text}
    and {!characters}, returning the effective [(categories, exclude_categories)]
    options to pass to the engine. *)
let effective_categories ?categories ?exclude_categories () =
  (match categories, exclude_categories with
   | Some _, Some _ ->
     raise (Invalid_argument "categories and exclude_categories are mutually exclusive")
   | _ -> ());
  (* Surrogate auto-exclusion *)
  (match categories with
   | Some cats ->
     List.iter cats ~f:(fun cat ->
       if List.mem surrogate_categories cat ~equal:String.equal
       then
         raise
           (Invalid_argument
              (sprintf
                 "Category %S includes surrogate codepoints (Cs), which OCaml UTF-8 \
                  strings cannot represent"
                 cat)))
   | None -> ());
  let effective_exclude_categories =
    match categories with
    | Some _ -> exclude_categories
    | None ->
      let excl = Option.value exclude_categories ~default:[] in
      if List.mem excl "Cs" ~equal:String.equal then Some excl else Some (excl @ [ "Cs" ])
  in
  categories, effective_exclude_categories
;;

(** [text_generator ~min_size ~max_size ...] is the shared {!Leaf} builder behind
    {!text} and {!characters}: it resolves the effective character categories and
    draws a text string over the described alphabet. *)
let text_generator
      ~min_size
      ~max_size
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ()
  =
  let categories, exclude_categories =
    effective_categories ?categories ?exclude_categories ()
  in
  leaf
    ~draw:(fun tc ->
      Internal.generate_text
        tc
        ~min_size
        ~max_size
        ~codec
        ~min_codepoint:(Option.value min_codepoint ~default:0)
        ~max_codepoint:(Option.value max_codepoint ~default:0xFFFFFFFF)
        ~categories
        ~exclude_categories
        ~include_characters
        ~exclude_characters)
    ~sexp_of:sexp_of_string
;;

(** [text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ?alphabet ()]
    creates a generator for Unicode text strings.

    Character filtering options restrict which characters may appear. The
    [alphabet] parameter is mutually exclusive with all individual character
    filtering parameters. Surrogate codepoints (category Cs) are always excluded
    since OCaml strings are conventionally UTF-8. *)
let text
      ?(min_size = 0)
      ?max_size
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ?alphabet
      ()
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
  let has_char_param =
    Option.is_some codec
    || Option.is_some min_codepoint
    || Option.is_some max_codepoint
    || Option.is_some categories
    || Option.is_some exclude_categories
    || Option.is_some include_characters
    || Option.is_some exclude_characters
  in
  (match alphabet with
   | Some _ when has_char_param ->
     raise
       (Invalid_argument
          "alphabet is mutually exclusive with individual character filtering parameters")
   | _ -> ());
  match alphabet with
  | Some alph ->
    (* An explicit alphabet: an empty base category set plus exactly these
       characters unioned in. *)
    text_generator ~min_size ~max_size ~categories:[] ~include_characters:alph ()
  | None ->
    text_generator
      ~min_size
      ~max_size
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ()
;;

(** [characters ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] creates a
    generator for single Unicode characters (as single-character UTF-8 strings).

    Character filtering options restrict which characters may appear. Surrogate
    codepoints (category Cs) are always excluded since OCaml strings are
    conventionally UTF-8. *)
let characters
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ()
  =
  text_generator
    ~min_size:1
    ~max_size:(Some 1)
    ?codec
    ?min_codepoint
    ?max_codepoint
    ?categories
    ?exclude_categories
    ?include_characters
    ?exclude_characters
    ()
;;

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)
let binary ?(min_size = 0) ?max_size () =
  if min_size < 0
  then raise (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
   | _ -> ());
  leaf
    ~draw:(fun tc -> Internal.generate_bytes tc ~min_size ~max_size)
    ~sexp_of:sexp_of_string
;;

(** [just value] creates a generator that always produces [value].

    The output type is chosen by the caller, so no printer is carried. *)
let just value = leaf_silent ~draw:(fun _ -> value)

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern], written in the syntax of Python's [re]
    module. When [fullmatch] is [true] (the default) the whole string must match
    [pattern]; otherwise a match anywhere suffices. *)
let from_regex pattern ?(fullmatch = true) () =
  leaf
    ~draw:(fun tc -> Internal.generate_regex tc ~pattern ~fullmatch)
    ~sexp_of:sexp_of_string
;;

(** [emails ()] creates a generator for valid email address strings.

    Addresses follow RFC 5321/5322: a local part of 1 to 64 characters from the
    RFC 5322 [atext] set, an [@], and a domain from {!domains}, with the overall
    address length capped at 254 octets (RFC 5321 §4.5.3.1.3). *)
let emails () = leaf ~draw:Internal.generate_email ~sexp_of:sexp_of_string

(** [urls ()] creates a generator for valid URL strings.

    URLs follow RFC 3986, of the form
    [scheme://domain\[:port\]/path\[#fragment\]] with [scheme] one of
    [http]/[https], the domain drawn from {!domains} (up to 255 characters), an
    optional port in [1, 65535], zero or more [/]-separated path segments of up
    to 100 characters each, and an optional fragment of up to 100 characters.
    Path and fragment characters are percent-encoded. *)
let urls () = leaf ~draw:Internal.generate_url ~sexp_of:sexp_of_string

(** [domains ?max_length ()] creates a generator for domain name strings.

    Domains are RFC 1035 fully-qualified domain names: a top-level domain
    sampled from the IANA TLD list followed by up to 126 dot-separated labels,
    each 1 to 63 characters matching
    [\[a-zA-Z\](\[a-zA-Z0-9-\]{0,61}\[a-zA-Z0-9\])?] (punycode [xn--] labels
    reserved by RFC 5890 are excluded). Generated domains never exceed
    [max_length] (default 255, per RFC 1035 §2.3.4); when provided, [max_length]
    must be in [4, 255]. *)
let domains ?max_length () =
  (match max_length with
   | Some ml when ml < 4 || ml > 255 ->
     raise (Invalid_argument (sprintf "max_length=%d must be between 4 and 255" ml))
   | _ -> ());
  let max_length = Option.value max_length ~default:255 in
  leaf ~draw:(fun tc -> Internal.generate_domain tc ~max_length) ~sexp_of:sexp_of_string
;;

(* [date_of_parts (year, month, day)] converts an engine-drawn Gregorian date
   into a [Core.Date.t]. The engine only emits calendar-valid dates, so the
   conversion cannot raise. *)
let date_of_parts (year, month, day) =
  Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day
;;

(* [time_of_parts (hour, minute, second, microsecond)] converts an engine-drawn
   time of day into a [Core.Time_ns.Ofday.t]. The engine only emits valid times,
   so the conversion cannot raise. *)
let time_of_parts (hour, minute, second, microsecond) =
  Time_ns.Ofday.create ~hr:hour ~min:minute ~sec:second ~us:microsecond ()
;;

(** [dates ()] creates a generator for [Core.Date.t] values, with year in
    [\[1, 9999\]] and calendar-valid month/day. *)
let dates () =
  leaf ~draw:(fun tc -> date_of_parts (Internal.generate_date tc)) ~sexp_of:Date.sexp_of_t
;;

(** [times ()] creates a generator for [Core.Time_ns.Ofday.t] times of day with
    microsecond precision. *)
let times () =
  leaf
    ~draw:(fun tc -> time_of_parts (Internal.generate_time tc))
    ~sexp_of:Time_ns.Ofday.sexp_of_t
;;

(** [datetimes ()] creates a generator for naive datetimes as
    [(Core.Date.t, Core.Time_ns.Ofday.t)] pairs, combining {!dates} and
    {!times}. *)
let datetimes () =
  leaf
    ~draw:(fun tc ->
      let date, time = Internal.generate_datetime tc in
      date_of_parts date, time_of_parts time)
    ~sexp_of:(fun (date, time) ->
      Sexp.List [ Date.sexp_of_t date; Time_ns.Ofday.sexp_of_t time ])
;;
