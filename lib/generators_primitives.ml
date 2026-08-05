open Sexplib0.Sexp_conv
open Generators_core

(** [integers ?min_value ?max_value ()] creates a generator for integers within
    the given bounds. When a bound is omitted it defaults to the corresponding
    OCaml native [int] limit. *)
let integers ?(min_value = Int.min_int) ?(max_value = Int.max_int) () =
  if min_value > max_value
  then
    raise
      (Invalid_argument
         (Printf.sprintf "Cannot have max_value=%d < min_value=%d" max_value min_value));
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
   | Some min, Some max when min > max ->
     raise
       (Invalid_argument
          (Printf.sprintf
             "There are no floats between min_value=%g and max_value=%g"
             min
             max))
   | _ -> ());
  if eff_allow_infinity && has_min && has_max
  then
    raise
      (Invalid_argument
         "Cannot have allow_infinity=true with both min_value and max_value");
  let min_value = Option.value min_value ~default:neg_infinity in
  let max_value = Option.value max_value ~default:infinity in
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
     List.iter
       (fun cat ->
          if List.mem cat surrogate_categories
          then
            raise
              (Invalid_argument
                 (Printf.sprintf
                    "Category %S includes surrogate codepoints (Cs), which OCaml UTF-8 \
                     strings cannot represent"
                    cat)))
       cats
   | None -> ());
  let effective_exclude_categories =
    match categories with
    | Some _ -> exclude_categories
    | None ->
      let excl = Option.value exclude_categories ~default:[] in
      if List.mem "Cs" excl then Some excl else Some (excl @ [ "Cs" ])
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
  then
    raise (Invalid_argument (Printf.sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument
          (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
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

(** [make_characters ~of_char ~sexp_of ()] builds a generator for single
    characters over any representation ['a], covering the full native [char]
    range (codepoints 0-255, i.e. Latin-1). *)
let make_characters ~of_char ~sexp_of () =
  leaf
    ~draw:(fun tc ->
      let s =
        Internal.generate_text
          tc
          ~min_size:1
          ~max_size:(Some 1)
          ~codec:None
          ~min_codepoint:0
          ~max_codepoint:0xFF
          ~categories:None
          ~exclude_categories:None
          ~include_characters:None
          ~exclude_characters:None
      in
      let decoded = String.get_utf_8_uchar s 0 in
      of_char (Char.chr (Uchar.to_int (Uchar.utf_decode_uchar decoded))))
    ~sexp_of
;;

(** [char ()] creates a generator for single characters (codepoints 0-255,
    i.e. Latin-1) as native [char] values. *)
let char () = make_characters ~of_char:Fun.id ~sexp_of:sexp_of_char ()

(** [binary ?min_size ?max_size ()] creates a generator for binary byte strings.
*)
let binary ?(min_size = 0) ?max_size () =
  if min_size < 0
  then
    raise (Invalid_argument (Printf.sprintf "min_size=%d must be non-negative" min_size));
  (match max_size with
   | Some ms when ms < 0 ->
     raise (Invalid_argument (Printf.sprintf "max_size=%d must be non-negative" ms))
   | Some ms when min_size > ms ->
     raise
       (Invalid_argument
          (Printf.sprintf "Cannot have max_size=%d < min_size=%d" ms min_size))
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
     raise
       (Invalid_argument (Printf.sprintf "max_length=%d must be between 4 and 255" ml))
   | _ -> ());
  let max_length = Option.value max_length ~default:255 in
  leaf ~draw:(fun tc -> Internal.generate_domain tc ~max_length) ~sexp_of:sexp_of_string
;;

(** [make_dates ~of_parts ~sexp_of ()] builds a date generator over any date
    representation. [of_parts] converts the generated date data to the desired
    date representation. *)
let make_dates ~of_parts ~sexp_of () =
  leaf
    ~draw:(fun tc ->
      let year, month, day = Internal.generate_date tc in
      of_parts ~year ~month ~day)
    ~sexp_of
;;

(** [make_times ~of_parts ~sexp_of ()] builds a time-of-day generator over any
    time representation. [of_parts] converts the generated time data to the 
    desired time representation. *)
let make_times ~of_parts ~sexp_of () =
  leaf
    ~draw:(fun tc ->
      let hour, minute, second, microsecond = Internal.generate_time tc in
      of_parts ~hour ~minute ~second ~microsecond)
    ~sexp_of
;;

(** [make_datetimes ~of_parts ~sexp_of ()] builds a naive-datetime generator
    over any representation. [of_parts] converts the generated datetime data
    to the desired representation. *)
let make_datetimes ~of_parts ~sexp_of () =
  leaf
    ~draw:(fun tc ->
      let (year, month, day), (hour, minute, second, microsecond) =
        Internal.generate_datetime tc
      in
      of_parts ~year ~month ~day ~hour ~minute ~second ~microsecond)
    ~sexp_of
;;

(* [format_date]/[format_time] render engine-drawn parts as ISO 8601 strings. *)
let format_date ~year ~month ~day = Printf.sprintf "%04d-%02d-%02d" year month day

let format_time ~hour ~minute ~second ~microsecond =
  Printf.sprintf "%02d:%02d:%02d.%06d" hour minute second microsecond
;;

(** [dates ()] is a generator for ISO 8601 [YYYY-MM-DD] date strings. *)
let dates () = make_dates ~of_parts:format_date ~sexp_of:sexp_of_string ()

(** [times ()] is a generator for ISO 8601 [HH:MM:SS.ffffff] time-of-day
    strings. *)
let times () = make_times ~of_parts:format_time ~sexp_of:sexp_of_string ()

(** [datetimes ()] is a generator for naive ISO 8601 [YYYY-MM-DDTHH:MM:SS.ffffff] 
    datetime strings. *)
let datetimes () =
  make_datetimes
    ~of_parts:(fun ~year ~month ~day ~hour ~minute ~second ~microsecond ->
      format_date ~year ~month ~day ^ "T" ^ format_time ~hour ~minute ~second ~microsecond)
    ~sexp_of:sexp_of_string
    ()
;;
