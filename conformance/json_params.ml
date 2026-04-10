(** JSON params parser for conformance binaries — backed by Yojson. *)

(** [parse json_str] parses a JSON object string into an association list of
    key-value pairs. Raises [Failure] if the input is not a JSON object. *)
let parse json_str =
  match Yojson.Safe.from_string json_str with
  | `Assoc pairs -> pairs
  | _ -> failwith "Expected JSON object"

(** [get_int pairs key default] returns the integer value for [key] in [pairs],
    or [default] if the key is absent or not an integer. *)
let get_int pairs key default =
  match List.assoc_opt key pairs with
  | Some (`Int n) -> n
  | Some _ | None -> default

(** [get_int_opt pairs key] returns [Some n] if [key] maps to an integer in
    [pairs], or [None] if absent or null. *)
let get_int_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Int n) -> Some n
  | Some `Null | None -> None
  | Some _ -> None

(** [get_float_opt pairs key] returns [Some f] if [key] maps to a number in
    [pairs] (integers are promoted to float), or [None] if absent or null. *)
let get_float_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Float f) -> Some f
  | Some (`Int n) -> Some (float_of_int n)
  | Some `Null | None -> None
  | Some _ -> None

(** [get_bool_opt pairs key] returns [Some b] if [key] maps to a boolean in
    [pairs], or [None] if absent or null. *)
let get_bool_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Bool b) -> Some b
  | Some `Null | None -> None
  | Some _ -> None

(** [get_bool pairs key default] returns the boolean value for [key] in [pairs],
    or [default] if absent or not a boolean. *)
let get_bool pairs key default =
  match List.assoc_opt key pairs with
  | Some (`Bool b) -> b
  | Some _ | None -> default

(** [get_string pairs key default] returns the string value for [key] in
    [pairs], or [default] if absent or not a string. *)
let get_string pairs key default =
  match List.assoc_opt key pairs with
  | Some (`String s) -> s
  | Some _ | None -> default

(** [get_int_array pairs key] returns the list of integers for [key] in [pairs].
    Float values are truncated to int. Non-numeric items are skipped. Returns
    [[]] if the key is absent or not a list. *)
let get_int_array pairs key =
  match List.assoc_opt key pairs with
  | Some (`List items) ->
      List.filter_map
        (function
          | `Int n -> Some n | `Float f -> Some (int_of_float f) | _ -> None)
        items
  | _ -> []

(** [get_string_opt pairs key] returns [Some s] if [key] maps to a string in
    [pairs], or [None] if absent or null. *)
let get_string_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`String s) -> Some s
  | Some `Null | None -> None
  | Some _ -> None

(** [get_string_list pairs key] returns the list of strings for [key] in
    [pairs]. Non-string items are skipped. Returns [[]] if the key is absent or
    not a list. *)
let get_string_list pairs key =
  match List.assoc_opt key pairs with
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(** [int_opt_to_json v] serializes an optional int to JSON: [Some n] becomes the
    string representation of [n], [None] becomes ["null"]. *)
let int_opt_to_json = function None -> "null" | Some n -> string_of_int n
