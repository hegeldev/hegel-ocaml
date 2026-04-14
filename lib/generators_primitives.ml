open! Core
open Generators_core

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
let integers ?min_value ?max_value () =
  (match (min_value, max_value) with
  | Some min, Some max when min > max ->
      raise
        (Invalid_argument
           (sprintf "Cannot have max_value=%d < min_value=%d" max min))
  | _ -> ());
  let pairs =
    List.filter_opt
      [
        Some (`Text "type", `Text "integer");
        Option.map min_value ~f:(fun v -> (`Text "min_value", `Int v));
        Option.map max_value ~f:(fun v -> (`Text "max_value", `Int v));
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_int }

(** [booleans ()] creates a generator for boolean values. *)
let booleans () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "boolean") ];
      transform = Cbor_helpers.extract_bool;
    }

(** [floats ?min_value ?max_value ?exclude_min ?exclude_max ?allow_nan
     ?allow_infinity ()] creates a generator for floating-point values.

    Uses schema type ["float"] as required by the Hegel server. The fields
    [allow_nan], [allow_infinity], [exclude_min], [exclude_max], and [width] are
    always sent (required by the server). Defaults follow Hypothesis:
    - [allow_nan]: [true] only when no bounds are set
    - [allow_infinity]: [true] when at most one bound is set *)
let floats ?min_value ?max_value ?(exclude_min = false) ?(exclude_max = false)
    ?allow_nan ?allow_infinity () =
  let has_min = Option.is_some min_value in
  let has_max = Option.is_some max_value in
  let eff_allow_nan =
    match allow_nan with Some v -> v | None -> (not has_min) && not has_max
  in
  let eff_allow_infinity =
    match allow_infinity with
    | Some v -> v
    | None -> (not has_min) || not has_max
  in
  if eff_allow_nan && (has_min || has_max) then
    raise
      (Invalid_argument "Cannot have allow_nan=true with min_value or max_value");
  (match (min_value, max_value) with
  | Some min, Some max when Float.( > ) min max ->
      raise
        (Invalid_argument
           (sprintf "There are no floats between min_value=%g and max_value=%g"
              min max))
  | _ -> ());
  if eff_allow_infinity && has_min && has_max then
    raise
      (Invalid_argument
         "Cannot have allow_infinity=true with both min_value and max_value");
  let pairs =
    [
      (`Text "type", `Text "float");
      (`Text "allow_nan", `Bool eff_allow_nan);
      (`Text "allow_infinity", `Bool eff_allow_infinity);
      (`Text "exclude_min", `Bool exclude_min);
      (`Text "exclude_max", `Bool exclude_max);
      (`Text "width", `Int 64);
    ]
  in
  let pairs =
    pairs
    @ List.filter_opt
        [
          Option.map min_value ~f:(fun v -> (`Text "min_value", `Float v));
          Option.map max_value ~f:(fun v -> (`Text "max_value", `Float v));
        ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_float }

(** Unicode general categories that include surrogate codepoints. OCaml strings
    are conventionally UTF-8, and surrogates in UTF-8 are ill-formed. *)
let surrogate_categories = [ "Cs"; "C" ]

(** [char_filter_schema_pairs ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] builds the
    CBOR key-value pairs for character filtering options, including surrogate
    auto-exclusion. Used by both {!text} and {!characters}. *)
let char_filter_schema_pairs ?codec ?min_codepoint ?max_codepoint ?categories
    ?exclude_categories ?include_characters ?exclude_characters () =
  (match (categories, exclude_categories) with
  | Some _, Some _ ->
      raise
        (Invalid_argument
           "categories and exclude_categories are mutually exclusive")
  | _ -> ());
  (* Surrogate auto-exclusion *)
  (match categories with
  | Some cats ->
      List.iter cats ~f:(fun cat ->
          if List.mem surrogate_categories cat ~equal:String.equal then
            raise
              (Invalid_argument
                 (sprintf
                    "Category %S includes surrogate codepoints (Cs), which \
                     OCaml UTF-8 strings cannot represent"
                    cat)))
  | None -> ());
  let effective_exclude_categories =
    match categories with
    | Some _ -> exclude_categories
    | None ->
        let excl = Option.value exclude_categories ~default:[] in
        if List.mem excl "Cs" ~equal:String.equal then Some excl
        else Some (excl @ [ "Cs" ])
  in
  List.filter_opt
    [
      Option.map codec ~f:(fun c -> (`Text "codec", `Text c));
      Option.map min_codepoint ~f:(fun cp -> (`Text "min_codepoint", `Int cp));
      Option.map max_codepoint ~f:(fun cp -> (`Text "max_codepoint", `Int cp));
      Option.map categories ~f:(fun cats ->
          (`Text "categories", `Array (List.map cats ~f:(fun c -> `Text c))));
      Option.map effective_exclude_categories ~f:(fun cats ->
          ( `Text "exclude_categories",
            `Array (List.map cats ~f:(fun c -> `Text c)) ));
      Option.map include_characters ~f:(fun s ->
          (`Text "include_characters", `Text s));
      Option.map exclude_characters ~f:(fun s ->
          (`Text "exclude_characters", `Text s));
    ]

(** [text ?min_size ?max_size ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ?alphabet ()]
    creates a generator for Unicode text strings.

    Uses schema type ["string"] as required by the Hegel server. Character
    filtering options restrict which characters may appear. The [alphabet]
    parameter is mutually exclusive with all individual character filtering
    parameters. Surrogate codepoints (category Cs) are always excluded since
    OCaml strings are conventionally UTF-8. *)
let text ?(min_size = 0) ?max_size ?codec ?min_codepoint ?max_codepoint
    ?categories ?exclude_categories ?include_characters ?exclude_characters
    ?alphabet () =
  if min_size < 0 then
    raise
      (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
  | Some ms when ms < 0 ->
      raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
  | Some ms when min_size > ms ->
      raise
        (Invalid_argument
           (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
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
           "alphabet is mutually exclusive with individual character filtering \
            parameters")
  | _ -> ());
  let char_pairs =
    match alphabet with
    | Some alph ->
        [
          (`Text "categories", `Array []);
          (`Text "include_characters", `Text alph);
        ]
    | None ->
        char_filter_schema_pairs ?codec ?min_codepoint ?max_codepoint
          ?categories ?exclude_categories ?include_characters
          ?exclude_characters ()
  in
  let pairs =
    List.filter_opt
      [
        Some (`Text "type", `Text "string");
        Some (`Text "min_size", `Int min_size);
        Option.map max_size ~f:(fun ms -> (`Text "max_size", `Int ms));
      ]
    @ char_pairs
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [characters ?codec ?min_codepoint ?max_codepoint ?categories
     ?exclude_categories ?include_characters ?exclude_characters ()] creates a
    generator for single Unicode characters (as single-character UTF-8 strings).

    Uses a string schema with [min_size: 1] and [max_size: 1]. Character
    filtering options restrict which characters may appear. Surrogate codepoints
    (category Cs) are always excluded since OCaml strings are conventionally
    UTF-8. *)
let characters ?codec ?min_codepoint ?max_codepoint ?categories
    ?exclude_categories ?include_characters ?exclude_characters () =
  let char_pairs =
    char_filter_schema_pairs ?codec ?min_codepoint ?max_codepoint ?categories
      ?exclude_categories ?include_characters ?exclude_characters ()
  in
  let pairs =
    [
      (`Text "type", `Text "string");
      (`Text "min_size", `Int 1);
      (`Text "max_size", `Int 1);
    ]
    @ char_pairs
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)
let binary ?(min_size = 0) ?max_size () =
  if min_size < 0 then
    raise
      (Invalid_argument (sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
  | Some ms when ms < 0 ->
      raise (Invalid_argument (sprintf "max_size=%d must be non-negative" ms))
  | Some ms when min_size > ms ->
      raise
        (Invalid_argument
           (sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
  | _ -> ());
  let pairs =
    List.filter_opt
      [
        Some (`Text "type", `Text "binary");
        Some (`Text "min_size", `Int min_size);
        Option.map max_size ~f:(fun ms -> (`Text "max_size", `Int ms));
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_bytes }

(** [just value] creates a generator that always produces [value].

    The schema uses [{"constant": null}] and the transform ignores the server
    result, returning the constant [value]. *)
let just value =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "constant"); (`Text "value", `Null) ];
      transform = (fun _ -> value);
    }

(** [from_regex pattern ?fullmatch ()] creates a generator for strings matching
    a regular expression [pattern].

    When [fullmatch] is [true] (the default), the entire string must match the
    pattern. When [false], a substring match suffices. *)
let from_regex pattern ?(fullmatch = true) () =
  Basic
    {
      schema =
        `Map
          [
            (`Text "type", `Text "regex");
            (`Text "pattern", `Text pattern);
            (`Text "fullmatch", `Bool fullmatch);
          ];
      transform = Cbor_helpers.extract_string;
    }

(** [emails ()] creates a generator for valid email address strings. *)
let emails () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "email") ];
      transform = Cbor_helpers.extract_string;
    }

(** [urls ()] creates a generator for valid URL strings. *)
let urls () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "url") ];
      transform = Cbor_helpers.extract_string;
    }

(** [domains ?max_length ()] creates a generator for domain name strings.

    If [max_length] is provided, generated domains will not exceed that length.
*)
let domains ?max_length () =
  (match max_length with
  | Some ml when ml < 4 || ml > 255 ->
      raise
        (Invalid_argument (sprintf "max_length=%d must be between 4 and 255" ml))
  | _ -> ());
  let pairs =
    List.filter_opt
      [
        Some (`Text "type", `Text "domain");
        Option.map max_length ~f:(fun ml -> (`Text "max_length", `Int ml));
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [dates ()] creates a generator for ISO 8601 date strings (YYYY-MM-DD). *)
let dates () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "date") ];
      transform = Cbor_helpers.extract_string;
    }

(** [times ()] creates a generator for time strings. *)
let times () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "time") ];
      transform = Cbor_helpers.extract_string;
    }

(** [datetimes ()] creates a generator for ISO 8601 datetime strings. *)
let datetimes () =
  Basic
    {
      schema = `Map [ (`Text "type", `Text "datetime") ];
      transform = Cbor_helpers.extract_string;
    }
