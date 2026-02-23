(** Simple JSON params parser for conformance binaries.

    Parses the small JSON objects passed as argv[1] to conformance binaries.
    Only supports the types needed by the conformance framework: integers,
    floats, booleans, null, strings, and arrays of integers. *)

type value =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of value list

(** [skip_ws s i] skips whitespace starting at position [i]. *)
let rec skip_ws s i =
  if i >= String.length s then i
  else match s.[i] with ' ' | '\t' | '\n' | '\r' -> skip_ws s (i + 1) | _ -> i

(** [parse_string s i] parses a JSON string starting at position [i] (after the
    opening quote). Returns (string, next_pos). *)
let parse_string s i =
  let buf = Buffer.create 16 in
  let rec go i =
    if i >= String.length s then failwith "Unterminated JSON string"
    else
      match s.[i] with
      | '"' -> (Buffer.contents buf, i + 1)
      | '\\' ->
          if i + 1 >= String.length s then failwith "Bad escape"
          else (
            (match s.[i + 1] with
            | '"' -> Buffer.add_char buf '"'
            | '\\' -> Buffer.add_char buf '\\'
            | 'n' -> Buffer.add_char buf '\n'
            | 'r' -> Buffer.add_char buf '\r'
            | 't' -> Buffer.add_char buf '\t'
            | c -> Buffer.add_char buf c);
            go (i + 2))
      | c ->
          Buffer.add_char buf c;
          go (i + 1)
  in
  go i

(** [parse_number s i] parses a JSON number starting at position [i]. Returns
    (value, next_pos). *)
let parse_number s i =
  let n = String.length s in
  let j = ref i in
  if !j < n && s.[!j] = '-' then incr j;
  while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do
    incr j
  done;
  let is_float = ref false in
  if !j < n && s.[!j] = '.' then begin
    is_float := true;
    incr j;
    while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do
      incr j
    done
  end;
  if !j < n && (s.[!j] = 'e' || s.[!j] = 'E') then begin
    is_float := true;
    incr j;
    if !j < n && (s.[!j] = '+' || s.[!j] = '-') then incr j;
    while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do
      incr j
    done
  end;
  let num_str = String.sub s i (!j - i) in
  let value =
    if !is_float then Float (float_of_string num_str)
    else
      match int_of_string_opt num_str with
      | Some n -> Int n
      | None -> Float (float_of_string num_str)
  in
  (value, !j)

(** [parse_value s i] parses a JSON value starting at position [i]. Returns
    (value, next_pos). *)
let rec parse_value s i =
  let i = skip_ws s i in
  if i >= String.length s then failwith "Unexpected end of JSON"
  else
    match s.[i] with
    | 'n' ->
        if i + 3 < String.length s && String.sub s i 4 = "null" then
          (Null, i + 4)
        else failwith "Expected null"
    | 't' ->
        if i + 3 < String.length s && String.sub s i 4 = "true" then
          (Bool true, i + 4)
        else failwith "Expected true"
    | 'f' ->
        if i + 4 < String.length s && String.sub s i 5 = "false" then
          (Bool false, i + 5)
        else failwith "Expected false"
    | '"' ->
        let str, j = parse_string s (i + 1) in
        (String str, j)
    | '[' -> parse_array s (i + 1)
    | '{' -> parse_object s (i + 1)
    | '-' | '0' .. '9' -> parse_number s i
    | c -> failwith (Printf.sprintf "Unexpected char %c at %d" c i)

(** [parse_array s i] parses a JSON array starting after the opening bracket. *)
and parse_array s i =
  let i = skip_ws s i in
  if i < String.length s && s.[i] = ']' then (Array [], i + 1)
  else
    let rec go i acc =
      let v, i = parse_value s i in
      let acc = v :: acc in
      let i = skip_ws s i in
      if i >= String.length s then failwith "Unterminated array"
      else
        match s.[i] with
        | ']' -> (Array (List.rev acc), i + 1)
        | ',' -> go (i + 1) acc
        | c -> failwith (Printf.sprintf "Unexpected char %c in array" c)
    in
    go i []

(** [parse_object s i] parses a JSON object starting after the opening brace.
    Returns an [Array] of key-value pair arrays. The top-level {!parse} function
    converts this to an association list. *)
and parse_object s i =
  let i = skip_ws s i in
  if i < String.length s && s.[i] = '}' then (Array [], i + 1)
  else
    let rec go i acc =
      let i = skip_ws s i in
      (* parse key *)
      if i >= String.length s || s.[i] <> '"' then
        failwith "Expected string key in object";
      let key, i = parse_string s (i + 1) in
      let i = skip_ws s i in
      if i >= String.length s || s.[i] <> ':' then
        failwith "Expected ':' in object";
      let v, i = parse_value s (i + 1) in
      let acc = (key, v) :: acc in
      let i = skip_ws s i in
      if i >= String.length s then failwith "Unterminated object"
      else
        match s.[i] with
        | '}' -> (List.rev acc, i + 1)
        | ',' -> go (i + 1) acc
        | c -> failwith (Printf.sprintf "Unexpected char %c in object" c)
    in
    let pairs, i = go i [] in
    (Array (List.map (fun (k, v) -> Array [ String k; v ]) pairs), i)

(** [parse json_str] parses a JSON string and returns (key*value) pairs as an
    association list. *)
let parse json_str =
  let v, _ = parse_value json_str 0 in
  match v with
  | Array pairs ->
      List.map
        (fun p ->
          match p with
          | Array [ String k; v ] -> (k, v)
          | _ -> failwith "parse_object returned unexpected structure")
        pairs
  | _ -> failwith "Expected JSON object"

(** [get_int pairs key default] gets an int from parsed params or returns
    default. *)
let get_int pairs key default =
  match List.assoc_opt key pairs with
  | Some (Int n) -> n
  | Some _ | None -> default

(** [get_int_opt pairs key] gets an optional int from parsed params. *)
let get_int_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (Int n) -> Some n
  | Some Null | None -> None
  | Some _ -> None

(** [get_float_opt pairs key] gets an optional float from parsed params. *)
let get_float_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (Float f) -> Some f
  | Some (Int n) -> Some (float_of_int n)
  | Some Null | None -> None
  | Some _ -> None

(** [get_bool_opt pairs key] gets an optional bool from parsed params. *)
let get_bool_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (Bool b) -> Some b
  | Some Null | None -> None
  | Some _ -> None

(** [get_bool pairs key default] gets a bool from parsed params or returns
    default. *)
let get_bool pairs key default =
  match List.assoc_opt key pairs with
  | Some (Bool b) -> b
  | Some _ | None -> default

(** [get_string pairs key default] gets a string from parsed params or returns
    default. *)
let get_string pairs key default =
  match List.assoc_opt key pairs with
  | Some (String s) -> s
  | Some _ | None -> default

(** [get_int_array pairs key] gets an array of integers from parsed params. *)
let get_int_array pairs key =
  match List.assoc_opt key pairs with
  | Some (Array items) ->
      List.filter_map
        (function
          | Int n -> Some n | Float f -> Some (int_of_float f) | _ -> None)
        items
  | _ -> []
