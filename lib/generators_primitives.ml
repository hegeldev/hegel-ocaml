open Generators_core

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. *)
let integers ?min_value ?max_value () =
  (match (min_value, max_value) with
  | Some min, Some max when min > max ->
      raise
        (Invalid_argument
           (Printf.sprintf "Cannot have max_value=%d < min_value=%d" max min))
  | _ -> ());
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "integer");
        Option.map (fun v -> (`Text "min_value", `Int v)) min_value;
        Option.map (fun v -> (`Text "max_value", `Int v)) max_value;
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
  | Some min, Some max when min > max ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "There are no floats between min_value=%g and max_value=%g" min
              max))
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
    @ List.filter_map Fun.id
        [
          Option.map (fun v -> (`Text "min_value", `Float v)) min_value;
          Option.map (fun v -> (`Text "max_value", `Float v)) max_value;
        ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_float }

(** [text ?min_size ?max_size ()] creates a generator for Unicode text strings.

    Uses schema type ["string"] as required by the Hegel server. *)
let text ?(min_size = 0) ?max_size () =
  if min_size < 0 then
    raise
      (Invalid_argument
         (Printf.sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
  | Some ms when ms < 0 ->
      raise
        (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
  | Some ms when min_size > ms ->
      raise
        (Invalid_argument
           (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
  | _ -> ());
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "string");
        Some (`Text "min_size", `Int min_size);
        Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_string }

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)
let binary ?(min_size = 0) ?max_size () =
  if min_size < 0 then
    raise
      (Invalid_argument
         (Printf.sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
  | Some ms when ms < 0 ->
      raise
        (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
  | Some ms when min_size > ms ->
      raise
        (Invalid_argument
           (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
  | _ -> ());
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "binary");
        Some (`Text "min_size", `Int min_size);
        Option.map (fun ms -> (`Text "max_size", `Int ms)) max_size;
      ]
  in
  Basic { schema = `Map pairs; transform = Cbor_helpers.extract_bytes }

(** [just value] creates a generator that always produces [value].

    The schema uses [{"const": null}] and the transform ignores the server
    result, returning the constant [value]. *)
let just value =
  Basic
    { schema = `Map [ (`Text "const", `Null) ]; transform = (fun _ -> value) }

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
        (Invalid_argument
           (Printf.sprintf "max_length=%d must be between 4 and 255" ml))
  | _ -> ());
  let pairs =
    List.filter_map Fun.id
      [
        Some (`Text "type", `Text "domain");
        Option.map (fun ml -> (`Text "max_length", `Int ml)) max_length;
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
