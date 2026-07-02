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
  let pairs =
    [ `Text "type", `Text "integer"
    ; `Text "min_value", `Int min_value
    ; `Text "max_value", `Int max_value
    ]
  in
  basic ~schema:(`Map pairs) ~transform:Cbor_helpers.extract_int ~sexp_of:sexp_of_int ()
;;

(** [booleans ()] creates a generator for boolean values. *)
let booleans () =
  basic
    ~schema:(`Map [ `Text "type", `Text "boolean" ])
    ~transform:Cbor_helpers.extract_bool
    ~sexp_of:sexp_of_bool
    ()
;;

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    The fields [allow_nan], [allow_infinity], [exclude_min], [exclude_max], and
    [width] are always sent.

    Defaults:
    - [min_value]: 64-bit float min (only when both [allow_nan] and
      [allow_infinity] are [false])
    - [max_value]: 64-bit float max (only when both [allow_nan] and
      [allow_infinity] are [false])
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
  let pairs =
    [ `Text "type", `Text "float"
    ; `Text "allow_nan", `Bool eff_allow_nan
    ; `Text "allow_infinity", `Bool eff_allow_infinity
    ; `Text "exclude_min", `Bool exclude_min
    ; `Text "exclude_max", `Bool exclude_max
    ; `Text "width", `Int 64
    ]
  in
  let pairs =
    pairs
    @ List.filter_opt
        [ Option.map min_value ~f:(fun v -> `Text "min_value", `Float v)
        ; Option.map max_value ~f:(fun v -> `Text "max_value", `Float v)
        ]
  in
  basic
    ~schema:(`Map pairs)
    ~transform:Cbor_helpers.extract_float
    ~sexp_of:sexp_of_float
    ()
;;

(** Unicode general categories that include surrogate codepoints. OCaml strings
    are conventionally UTF-8, and surrogates in UTF-8 are ill-formed. *)
let surrogate_categories = [ "Cs"; "C" ]

(** [char_filter_schema_pairs ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] builds the
    CBOR key-value pairs for character filtering options, including surrogate
    auto-exclusion. Used by both {!text} and {!characters}. *)
let char_filter_schema_pairs
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ()
  =
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
  List.filter_opt
    [ Option.map codec ~f:(fun c -> `Text "codec", `Text c)
    ; Option.map min_codepoint ~f:(fun cp -> `Text "min_codepoint", `Int cp)
    ; Option.map max_codepoint ~f:(fun cp -> `Text "max_codepoint", `Int cp)
    ; Option.map categories ~f:(fun cats ->
        `Text "categories", `Array (List.map cats ~f:(fun c -> `Text c)))
    ; Option.map effective_exclude_categories ~f:(fun cats ->
        `Text "exclude_categories", `Array (List.map cats ~f:(fun c -> `Text c)))
    ; Option.map include_characters ~f:(fun s -> `Text "include_characters", `Text s)
    ; Option.map exclude_characters ~f:(fun s -> `Text "exclude_characters", `Text s)
    ]
;;

(** [text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ?alphabet ()]
    creates a generator for Unicode text strings.

    Uses schema type ["string"] as required by the Hegel engine. Character
    filtering options restrict which characters may appear. The [alphabet]
    parameter is mutually exclusive with all individual character filtering
    parameters. Surrogate codepoints (category Cs) are always excluded since
    OCaml strings are conventionally UTF-8. *)
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
  let char_pairs =
    match alphabet with
    | Some alph ->
      [ `Text "categories", `Array []; `Text "include_characters", `Text alph ]
    | None ->
      char_filter_schema_pairs
        ?codec
        ?min_codepoint
        ?max_codepoint
        ?categories
        ?exclude_categories
        ?include_characters
        ?exclude_characters
        ()
  in
  let pairs =
    List.filter_opt
      [ Some (`Text "type", `Text "string")
      ; Some (`Text "min_size", `Int min_size)
      ; Option.map max_size ~f:(fun ms -> `Text "max_size", `Int ms)
      ]
    @ char_pairs
  in
  basic
    ~schema:(`Map pairs)
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [characters ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] creates a
    generator for single Unicode characters (as single-character UTF-8 strings).

    Uses a string schema with [min_size: 1] and [max_size: 1]. Character
    filtering options restrict which characters may appear. Surrogate codepoints
    (category Cs) are always excluded since OCaml strings are conventionally
    UTF-8. *)
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
  let char_pairs =
    char_filter_schema_pairs
      ?codec
      ?min_codepoint
      ?max_codepoint
      ?categories
      ?exclude_categories
      ?include_characters
      ?exclude_characters
      ()
  in
  let pairs =
    [ `Text "type", `Text "string"; `Text "min_size", `Int 1; `Text "max_size", `Int 1 ]
    @ char_pairs
  in
  basic
    ~schema:(`Map pairs)
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
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
  let pairs =
    List.filter_opt
      [ Some (`Text "type", `Text "binary")
      ; Some (`Text "min_size", `Int min_size)
      ; Option.map max_size ~f:(fun ms -> `Text "max_size", `Int ms)
      ]
  in
  basic
    ~schema:(`Map pairs)
    ~transform:Cbor_helpers.extract_bytes
    ~sexp_of:sexp_of_string
    ()
;;

(** [just value] creates a generator that always produces [value].

    The schema uses [{"constant": null}] and the transform ignores the engine
    result, returning the constant [value]. The output type is chosen by the
    caller, so no printer is carried. *)
let just value =
  basic_silent
    ~schema:(`Map [ `Text "type", `Text "constant"; `Text "value", `Null ])
    ~transform:(fun _ -> value)
    ()
;;

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern], written in the syntax of Python's [re]
    module. When [fullmatch] is [true] (the default) the whole string must match
    [pattern]; otherwise a match anywhere suffices. *)
let from_regex pattern ?(fullmatch = true) () =
  basic
    ~schema:
      (`Map
          [ `Text "type", `Text "regex"
          ; `Text "pattern", `Text pattern
          ; `Text "fullmatch", `Bool fullmatch
          ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [emails ()] creates a generator for valid email address strings.

    Addresses follow RFC 5321/5322: a local part of 1 to 64 characters from the
    RFC 5322 [atext] set, an [@], and a domain from {!domains}, with the overall
    address length capped at 254 octets (RFC 5321 §4.5.3.1.3). *)
let emails () =
  basic
    ~schema:(`Map [ `Text "type", `Text "email" ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [urls ()] creates a generator for valid URL strings.

    URLs follow RFC 3986, of the form
    [scheme://domain\[:port\]/path\[#fragment\]] with [scheme] one of
    [http]/[https], the domain drawn from {!domains} (up to 255 characters), an
    optional port in [1, 65535], zero or more [/]-separated path segments of up
    to 100 characters each, and an optional fragment of up to 100 characters.
    Path and fragment characters are percent-encoded. *)
let urls () =
  basic
    ~schema:(`Map [ `Text "type", `Text "url" ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

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
  let pairs =
    List.filter_opt
      [ Some (`Text "type", `Text "domain")
      ; Option.map max_length ~f:(fun ml -> `Text "max_length", `Int ml)
      ]
  in
  basic
    ~schema:(`Map pairs)
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [dates ()] creates a generator for ISO 8601 date strings ([YYYY-MM-DD]),
    with year in [\[1, 9999\]] and calendar-valid month/day. *)
let dates () =
  basic
    ~schema:(`Map [ `Text "type", `Text "date" ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [times ()] creates a generator for ISO 8601 time strings ([HH:MM:SS] or
    [HH:MM:SS.ffffff], the fractional part present only when microseconds are
    non-zero). *)
let times () =
  basic
    ~schema:(`Map [ `Text "type", `Text "time" ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;

(** [datetimes ()] creates a generator for ISO 8601 datetime strings
    ([YYYY-MM-DDTHH:MM:SS\[.ffffff\]]), combining {!dates} and {!times}. *)
let datetimes () =
  basic
    ~schema:(`Map [ `Text "type", `Text "datetime" ])
    ~transform:Cbor_helpers.extract_string
    ~sexp_of:sexp_of_string
    ()
;;
