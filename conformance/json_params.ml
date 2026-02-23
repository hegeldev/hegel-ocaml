(** JSON params parser for conformance binaries — backed by Yojson. *)

let parse json_str =
  match Yojson.Safe.from_string json_str with
  | `Assoc pairs -> pairs
  | _ -> failwith "Expected JSON object"

let get_int pairs key default =
  match List.assoc_opt key pairs with
  | Some (`Int n) -> n
  | Some _ | None -> default

let get_int_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Int n) -> Some n
  | Some `Null | None -> None
  | Some _ -> None

let get_float_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Float f) -> Some f
  | Some (`Int n) -> Some (float_of_int n)
  | Some `Null | None -> None
  | Some _ -> None

let get_bool_opt pairs key =
  match List.assoc_opt key pairs with
  | Some (`Bool b) -> Some b
  | Some `Null | None -> None
  | Some _ -> None

let get_bool pairs key default =
  match List.assoc_opt key pairs with
  | Some (`Bool b) -> b
  | Some _ | None -> default

let get_string pairs key default =
  match List.assoc_opt key pairs with
  | Some (`String s) -> s
  | Some _ | None -> default

let get_int_array pairs key =
  match List.assoc_opt key pairs with
  | Some (`List items) ->
    List.filter_map
      (function
        | `Int n -> Some n
        | `Float f -> Some (int_of_float f)
        | _ -> None)
      items
  | _ -> []
