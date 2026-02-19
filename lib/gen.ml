(* ================================================================ *)
(* Span Labels                                                      *)
(* ================================================================ *)

module Labels = struct
  let list = 1
  let list_element = 2
  let set = 3
  let set_element = 4
  let map = 5
  let map_entry = 6
  let tuple = 7
  let one_of = 8
  let optional = 9
  let fixed_dict = 10
  let flat_map = 11
  let filter = 12
  let mapped = 13
  let sampled_from = 14
end

(* ================================================================ *)
(* Exception for assume(false)                                      *)
(* ================================================================ *)

exception Assume_rejected

(* ================================================================ *)
(* Core types                                                       *)
(* ================================================================ *)

type 'a basic = { schema : Cbor.t; parse : Cbor.t -> 'a }
type 'a t = { generate : unit -> 'a; as_basic : unit -> 'a basic option }

(* ================================================================ *)
(* Protocol helpers                                                 *)
(* ================================================================ *)

let is_debug () = Sys.getenv_opt "HEGEL_PROTOCOL_DEBUG" <> None

let send_request command payload =
  let entries =
    (Cbor.Text "command", Cbor.Text command)
    :: (match payload with Cbor.Map pairs -> pairs | _ -> assert false)
  in
  let request = Cbor.Map entries in
  if is_debug () then
    Printf.eprintf "REQUEST: %s\n%!" (Cbor.to_diagnostic request);
  let ch = State.get_channel () in
  let result = Protocol.Channel.request_cbor ch request in
  if is_debug () then
    Printf.eprintf "RESPONSE: %s\n%!" (Cbor.to_diagnostic result);
  result

let string_contains haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec check i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else check (i + 1)
    in
    check 0

let send_request_no_fail command payload =
  try send_request command payload
  with Failure msg ->
    if
      string_contains msg "StopTest"
      || string_contains msg "overflow"
      || string_contains msg "Overrun"
    then (
      State.set_test_aborted true;
      raise Assume_rejected)
    else (
      Printf.eprintf "Failed to communicate with Hegel: %s\n%!" msg;
      exit 134)

let generate_raw schema =
  if State.get_test_aborted () then raise Assume_rejected;
  let result =
    send_request_no_fail "generate" (Cbor.Map [ (Cbor.Text "schema", schema) ])
  in
  (if State.get_last_run () then
     let s = Printf.sprintf "Generated: %s" (Cbor.to_diagnostic result) in
     State.buffer_generated_value s);
  result

let start_span label =
  if State.get_test_aborted () then raise Assume_rejected;
  State.increment_span_depth ();
  try
    let _ =
      send_request_no_fail "start_span"
        (Cbor.Map [ (Cbor.Text "label", Cbor.Unsigned label) ])
    in
    ()
  with Assume_rejected ->
    State.decrement_span_depth ();
    raise Assume_rejected

let stop_span discard =
  if State.get_test_aborted () then (
    State.decrement_span_depth ();
    ())
  else (
    State.decrement_span_depth ();
    let _ =
      try
        send_request "stop_span"
          (Cbor.Map [ (Cbor.Text "discard", Cbor.Bool discard) ])
      with _ -> Cbor.Null
    in
    ())

let note message = if State.get_last_run () then Printf.eprintf "%s\n%!" message

let target ?(label = "") value =
  let _ =
    send_request_no_fail "target"
      (Cbor.Map
         [
           (Cbor.Text "value", Cbor.Float value);
           (Cbor.Text "label", Cbor.Text label);
         ])
  in
  ()

(* ================================================================ *)
(* Span grouping helpers                                            *)
(* ================================================================ *)

let group label f =
  start_span label;
  Fun.protect ~finally:(fun () -> stop_span false) f

(* ================================================================ *)
(* Server-managed collections                                       *)
(* ================================================================ *)

type collection = {
  base_name : string;
  min_size : int;
  max_size : int option;
  mutable server_name : string option;
  mutable finished : bool;
}

let new_collection name min_size max_size =
  { base_name = name; min_size; max_size; server_name = None; finished = false }

let ensure_initialized coll =
  match coll.server_name with
  | Some name -> name
  | None ->
      let pairs =
        [
          (Cbor.Text "name", Cbor.Text coll.base_name);
          (Cbor.Text "min_size", Cbor.Unsigned coll.min_size);
        ]
        @
        match coll.max_size with
        | Some max -> [ (Cbor.Text "max_size", Cbor.Unsigned max) ]
        | None -> []
      in
      let response = send_request_no_fail "new_collection" (Cbor.Map pairs) in
      let name =
        match response with
        | Cbor.Text s -> s
        | _ -> failwith "Expected text response from new_collection"
      in
      coll.server_name <- Some name;
      name

let collection_more coll =
  (* Only called from while loop condition, which exits on false *)
  assert (not coll.finished);
  let name = ensure_initialized coll in
  let response =
    send_request_no_fail "collection_more"
      (Cbor.Map [ (Cbor.Text "collection", Cbor.Text name) ])
  in
  let result =
    match response with
    | Cbor.Bool b -> b
    | _ -> failwith "Expected bool from collection_more"
  in
  if not result then coll.finished <- true;
  result

(* ================================================================ *)
(* Base64 decode (for binary generator)                             *)
(* ================================================================ *)

let base64_char_value c =
  match c with
  | 'A' .. 'Z' -> Char.code c - Char.code 'A'
  | 'a' .. 'z' -> Char.code c - Char.code 'a' + 26
  | '0' .. '9' -> Char.code c - Char.code '0' + 52
  | '+' -> 62
  | '/' -> 63
  | '=' -> 0
  | _ -> failwith (Printf.sprintf "Invalid base64 character: %c" c)

let base64_decode input =
  let len = String.length input in
  if len = 0 then ""
  else (
    assert (len mod 4 = 0);
    let buf = Buffer.create (len * 3 / 4) in
    let i = ref 0 in
    while !i < len do
      let a = base64_char_value input.[!i] in
      let b = base64_char_value input.[!i + 1] in
      let c = base64_char_value input.[!i + 2] in
      let d = base64_char_value input.[!i + 3] in
      Buffer.add_char buf (Char.chr ((a lsl 2) lor (b lsr 4) land 0xff));
      if input.[!i + 2] <> '=' then
        Buffer.add_char buf
          (Char.chr (((b land 0x0f) lsl 4) lor (c lsr 2) land 0xff));
      if input.[!i + 3] <> '=' then
        Buffer.add_char buf (Char.chr (((c land 0x03) lsl 6) lor d land 0xff));
      i := !i + 4
    done;
    Buffer.contents buf)

(* ================================================================ *)
(* CBOR value parsing helpers                                       *)
(* ================================================================ *)

let cbor_int_value = function
  | Cbor.Unsigned n -> n
  | Cbor.Negative n -> n
  | Cbor.Float f when Float.is_integer f -> Float.to_int f
  | v ->
      failwith
        (Printf.sprintf "Expected integer, got %s" (Cbor.to_diagnostic v))

let cbor_float_value = function
  | Cbor.Float f -> f
  | Cbor.Unsigned n -> Float.of_int n
  | Cbor.Negative n -> Float.of_int n
  | v ->
      failwith (Printf.sprintf "Expected float, got %s" (Cbor.to_diagnostic v))

let cbor_text_value = function
  | Cbor.Text s -> s
  | v ->
      failwith (Printf.sprintf "Expected text, got %s" (Cbor.to_diagnostic v))

let cbor_bool_value = function
  | Cbor.Bool b -> b
  | v ->
      failwith (Printf.sprintf "Expected bool, got %s" (Cbor.to_diagnostic v))

let cbor_array_value = function
  | Cbor.Array a -> a
  | Cbor.Tag (258, Cbor.Array a) -> a (* CBOR set tag *)
  | v ->
      failwith (Printf.sprintf "Expected array, got %s" (Cbor.to_diagnostic v))

(* ================================================================ *)
(* Primitives                                                       *)
(* ================================================================ *)

let unit () =
  let schema = Cbor.Map [ (Cbor.Text "const", Cbor.Null) ] in
  {
    generate =
      (fun () ->
        let _ = generate_raw schema in
        ());
    as_basic = (fun () -> Some { schema; parse = (fun _ -> ()) });
  }

let bool () =
  let schema = Cbor.Map [ (Cbor.Text "type", Cbor.Text "boolean") ] in
  {
    generate = (fun () -> cbor_bool_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_bool_value });
  }

let just value =
  let schema = Cbor.Map [ (Cbor.Text "const", Cbor.Null) ] in
  {
    generate =
      (fun () ->
        let _ = generate_raw schema in
        value);
    as_basic = (fun () -> Some { schema; parse = (fun _ -> value) });
  }

(* ================================================================ *)
(* Numeric                                                          *)
(* ================================================================ *)

let int ?(min = min_int) ?(max = max_int) () =
  let schema =
    Cbor.Map
      [
        (Cbor.Text "type", Cbor.Text "integer");
        ( Cbor.Text "min_value",
          if min >= 0 then Cbor.Unsigned min else Cbor.Negative min );
        ( Cbor.Text "max_value",
          if max >= 0 then Cbor.Unsigned max else Cbor.Negative max );
      ]
  in
  {
    generate = (fun () -> cbor_int_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_int_value });
  }

let int32 ?(min = Int32.min_int) ?(max = Int32.max_int) () =
  let min_i = Int32.to_int min in
  let max_i = Int32.to_int max in
  let schema =
    Cbor.Map
      [
        (Cbor.Text "type", Cbor.Text "integer");
        ( Cbor.Text "min_value",
          if min_i >= 0 then Cbor.Unsigned min_i else Cbor.Negative min_i );
        ( Cbor.Text "max_value",
          if max_i >= 0 then Cbor.Unsigned max_i else Cbor.Negative max_i );
      ]
  in
  {
    generate = (fun () -> Int32.of_int (cbor_int_value (generate_raw schema)));
    as_basic =
      (fun () ->
        Some { schema; parse = (fun v -> Int32.of_int (cbor_int_value v)) });
  }

let int64 ?(min = Int64.of_int min_int) ?(max = Int64.of_int max_int) () =
  let min_i = Int64.to_int min in
  let max_i = Int64.to_int max in
  let schema =
    Cbor.Map
      [
        (Cbor.Text "type", Cbor.Text "integer");
        ( Cbor.Text "min_value",
          if min_i >= 0 then Cbor.Unsigned min_i else Cbor.Negative min_i );
        ( Cbor.Text "max_value",
          if max_i >= 0 then Cbor.Unsigned max_i else Cbor.Negative max_i );
      ]
  in
  {
    generate = (fun () -> Int64.of_int (cbor_int_value (generate_raw schema)));
    as_basic =
      (fun () ->
        Some { schema; parse = (fun v -> Int64.of_int (cbor_int_value v)) });
  }

let float ?(min = neg_infinity) ?(max = infinity) ?allow_nan ?allow_infinity () =
  let has_min = Float.is_finite min in
  let has_max = Float.is_finite max in
  let allow_nan =
    match allow_nan with
    | Some v -> v
    | None -> (not has_min) && not has_max
  in
  let allow_infinity =
    match allow_infinity with
    | Some v -> v
    | None -> (not has_min) || not has_max
  in
  let pairs =
    [
      (Cbor.Text "type", Cbor.Text "number");
      (Cbor.Text "exclude_min", Cbor.Bool false);
      (Cbor.Text "exclude_max", Cbor.Bool false);
      (Cbor.Text "allow_nan", Cbor.Bool allow_nan);
      (Cbor.Text "allow_infinity", Cbor.Bool allow_infinity);
      (Cbor.Text "width", Cbor.Unsigned 64);
    ]
    @ (if has_min || ((not allow_infinity) && not allow_nan) then
         [ (Cbor.Text "min_value", Cbor.Float min) ]
       else [])
    @
    if has_max || ((not allow_infinity) && not allow_nan) then
      [ (Cbor.Text "max_value", Cbor.Float max) ]
    else []
  in
  let schema = Cbor.Map pairs in
  {
    generate = (fun () -> cbor_float_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_float_value });
  }

(* ================================================================ *)
(* Strings                                                          *)
(* ================================================================ *)

let string ?(min_size = 0) ?max_size () =
  let pairs =
    [
      (Cbor.Text "type", Cbor.Text "string");
      (Cbor.Text "min_size", Cbor.Unsigned min_size);
    ]
    @
    match max_size with
    | Some ms -> [ (Cbor.Text "max_size", Cbor.Unsigned ms) ]
    | None -> []
  in
  let schema = Cbor.Map pairs in
  {
    generate = (fun () -> cbor_text_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_text_value });
  }

let from_regex ?(fullmatch = false) pattern =
  let schema =
    Cbor.Map
      [
        (Cbor.Text "type", Cbor.Text "regex");
        (Cbor.Text "pattern", Cbor.Text pattern);
        (Cbor.Text "fullmatch", Cbor.Bool fullmatch);
      ]
  in
  {
    generate = (fun () -> cbor_text_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_text_value });
  }

(* ================================================================ *)
(* Binary                                                           *)
(* ================================================================ *)

let binary ?(min_size = 0) ?max_size () =
  let pairs =
    [
      (Cbor.Text "type", Cbor.Text "binary");
      (Cbor.Text "min_size", Cbor.Unsigned min_size);
    ]
    @
    match max_size with
    | Some ms -> [ (Cbor.Text "max_size", Cbor.Unsigned ms) ]
    | None -> []
  in
  let schema = Cbor.Map pairs in
  let parse_binary v = base64_decode (cbor_text_value v) in
  {
    generate = (fun () -> parse_binary (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = parse_binary });
  }

(* ================================================================ *)
(* Collections                                                      *)
(* ================================================================ *)

let list ?(min_size = 0) ?max_size gen =
  {
    generate =
      (fun () ->
        match gen.as_basic () with
        | Some basic ->
            let pairs =
              [
                (Cbor.Text "type", Cbor.Text "list");
                (Cbor.Text "elements", basic.schema);
                (Cbor.Text "min_size", Cbor.Unsigned min_size);
              ]
              @
              match max_size with
              | Some ms -> [ (Cbor.Text "max_size", Cbor.Unsigned ms) ]
              | None -> []
            in
            let schema = Cbor.Map pairs in
            let raw = generate_raw schema in
            List.map basic.parse (cbor_array_value raw)
        | None ->
            group Labels.list (fun () ->
                let coll = new_collection "composite_list" min_size max_size in
                let result = ref [] in
                while collection_more coll do
                  result := gen.generate () :: !result
                done;
                List.rev !result));
    as_basic =
      (fun () ->
        match gen.as_basic () with
        | Some basic ->
            let pairs =
              [
                (Cbor.Text "type", Cbor.Text "list");
                (Cbor.Text "elements", basic.schema);
                (Cbor.Text "min_size", Cbor.Unsigned min_size);
              ]
              @
              match max_size with
              | Some ms -> [ (Cbor.Text "max_size", Cbor.Unsigned ms) ]
              | None -> []
            in
            let schema = Cbor.Map pairs in
            Some
              {
                schema;
                parse = (fun raw -> List.map basic.parse (cbor_array_value raw));
              }
        | None -> None);
  }

let array ?min_size ?max_size gen =
  let list_gen = list ?min_size ?max_size gen in
  {
    generate = (fun () -> Array.of_list (list_gen.generate ()));
    as_basic =
      (fun () ->
        match list_gen.as_basic () with
        | Some basic ->
            Some
              {
                schema = basic.schema;
                parse = (fun raw -> Array.of_list (basic.parse raw));
              }
        | None -> None);
  }

(* ================================================================ *)
(* Tuples                                                           *)
(* ================================================================ *)

let pair gen1 gen2 =
  {
    generate =
      (fun () ->
        match (gen1.as_basic (), gen2.as_basic ()) with
        | Some b1, Some b2 ->
            let schema =
              Cbor.Map
                [
                  (Cbor.Text "type", Cbor.Text "tuple");
                  (Cbor.Text "elements", Cbor.Array [ b1.schema; b2.schema ]);
                ]
            in
            let raw = generate_raw schema in
            let arr = cbor_array_value raw in
            (b1.parse (List.nth arr 0), b2.parse (List.nth arr 1))
        | _ ->
            group Labels.tuple (fun () ->
                let v1 = gen1.generate () in
                let v2 = gen2.generate () in
                (v1, v2)));
    as_basic =
      (fun () ->
        match (gen1.as_basic (), gen2.as_basic ()) with
        | Some b1, Some b2 ->
            let schema =
              Cbor.Map
                [
                  (Cbor.Text "type", Cbor.Text "tuple");
                  (Cbor.Text "elements", Cbor.Array [ b1.schema; b2.schema ]);
                ]
            in
            Some
              {
                schema;
                parse =
                  (fun raw ->
                    let arr = cbor_array_value raw in
                    (b1.parse (List.nth arr 0), b2.parse (List.nth arr 1)));
              }
        | _ -> None);
  }

let triple gen1 gen2 gen3 =
  {
    generate =
      (fun () ->
        match (gen1.as_basic (), gen2.as_basic (), gen3.as_basic ()) with
        | Some b1, Some b2, Some b3 ->
            let schema =
              Cbor.Map
                [
                  (Cbor.Text "type", Cbor.Text "tuple");
                  ( Cbor.Text "elements",
                    Cbor.Array [ b1.schema; b2.schema; b3.schema ] );
                ]
            in
            let raw = generate_raw schema in
            let arr = cbor_array_value raw in
            ( b1.parse (List.nth arr 0),
              b2.parse (List.nth arr 1),
              b3.parse (List.nth arr 2) )
        | _ ->
            group Labels.tuple (fun () ->
                let v1 = gen1.generate () in
                let v2 = gen2.generate () in
                let v3 = gen3.generate () in
                (v1, v2, v3)));
    as_basic =
      (fun () ->
        match (gen1.as_basic (), gen2.as_basic (), gen3.as_basic ()) with
        | Some b1, Some b2, Some b3 ->
            let schema =
              Cbor.Map
                [
                  (Cbor.Text "type", Cbor.Text "tuple");
                  ( Cbor.Text "elements",
                    Cbor.Array [ b1.schema; b2.schema; b3.schema ] );
                ]
            in
            Some
              {
                schema;
                parse =
                  (fun raw ->
                    let arr = cbor_array_value raw in
                    ( b1.parse (List.nth arr 0),
                      b2.parse (List.nth arr 1),
                      b3.parse (List.nth arr 2) ));
              }
        | _ -> None);
  }

(* ================================================================ *)
(* Combinators                                                      *)
(* ================================================================ *)

let map f gen =
  {
    generate =
      (fun () ->
        match gen.as_basic () with
        | Some basic -> f (basic.parse (generate_raw basic.schema))
        | None -> group Labels.mapped (fun () -> f (gen.generate ())));
    as_basic =
      (fun () ->
        match gen.as_basic () with
        | Some basic ->
            Some
              {
                schema = basic.schema;
                parse = (fun raw -> f (basic.parse raw));
              }
        | None -> None);
  }

let filter predicate gen =
  {
    generate =
      (fun () ->
        let rec try_n n =
          if n <= 0 then raise Assume_rejected
          else (
            start_span Labels.filter;
            let value = gen.generate () in
            if predicate value then (
              stop_span false;
              value)
            else (
              stop_span true;
              try_n (n - 1)))
        in
        try_n 3);
    as_basic = (fun () -> None);
  }

let flat_map f gen =
  {
    generate =
      (fun () ->
        group Labels.flat_map (fun () ->
            let intermediate = gen.generate () in
            let next_gen = f intermediate in
            next_gen.generate ()));
    as_basic = (fun () -> None);
  }

let one_of gens =
  match gens with
  | [] -> failwith "one_of: empty list"
  | _ ->
      let n = List.length gens in
      {
        generate =
          (fun () ->
            let basics = List.map (fun g -> g.as_basic ()) gens in
            if List.for_all Option.is_some basics then
              let basics = List.map Option.get basics in
              let tagged_schemas =
                List.mapi
                  (fun i b ->
                    Cbor.Map
                      [
                        (Cbor.Text "type", Cbor.Text "tuple");
                        ( Cbor.Text "elements",
                          Cbor.Array
                            [
                              Cbor.Map [ (Cbor.Text "const", Cbor.Unsigned i) ];
                              b.schema;
                            ] );
                      ])
                  basics
              in
              let schema =
                Cbor.Map [ (Cbor.Text "one_of", Cbor.Array tagged_schemas) ]
              in
              let raw = generate_raw schema in
              let arr = cbor_array_value raw in
              let tag = cbor_int_value (List.nth arr 0) in
              let value = List.nth arr 1 in
              (List.nth basics tag).parse value
            else
              group Labels.one_of (fun () ->
                  let idx = (int ~min:0 ~max:(n - 1) ()).generate () in
                  (List.nth gens idx).generate ()));
        as_basic =
          (fun () ->
            let basics = List.map (fun g -> g.as_basic ()) gens in
            if List.for_all Option.is_some basics then
              let basics = List.map Option.get basics in
              let tagged_schemas =
                List.mapi
                  (fun i b ->
                    Cbor.Map
                      [
                        (Cbor.Text "type", Cbor.Text "tuple");
                        ( Cbor.Text "elements",
                          Cbor.Array
                            [
                              Cbor.Map [ (Cbor.Text "const", Cbor.Unsigned i) ];
                              b.schema;
                            ] );
                      ])
                  basics
              in
              let schema =
                Cbor.Map [ (Cbor.Text "one_of", Cbor.Array tagged_schemas) ]
              in
              Some
                {
                  schema;
                  parse =
                    (fun raw ->
                      let arr = cbor_array_value raw in
                      let tag = cbor_int_value (List.nth arr 0) in
                      let value = List.nth arr 1 in
                      (List.nth basics tag).parse value);
                }
            else None);
      }

let optional gen =
  let just_none = just None in
  let some_gen = map Option.some gen in
  one_of [ just_none; some_gen ]

let sampled_from elements =
  match elements with
  | [] -> failwith "sampled_from: empty list"
  | _ ->
      let n = List.length elements in
      let schema =
        Cbor.Map
          [
            (Cbor.Text "type", Cbor.Text "integer");
            (Cbor.Text "min_value", Cbor.Unsigned 0);
            (Cbor.Text "max_value", Cbor.Unsigned (n - 1));
          ]
      in
      let parse raw =
        let idx = cbor_int_value raw in
        List.nth elements idx
      in
      {
        generate = (fun () -> parse (generate_raw schema));
        as_basic = (fun () -> Some { schema; parse });
      }

(* ================================================================ *)
(* Formats                                                          *)
(* ================================================================ *)

let simple_format type_name =
  let schema = Cbor.Map [ (Cbor.Text "type", Cbor.Text type_name) ] in
  {
    generate = (fun () -> cbor_text_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_text_value });
  }

let email () = simple_format "email"
let url () = simple_format "url"
let date () = simple_format "date"
let time () = simple_format "time"
let datetime () = simple_format "datetime"

let domain ?(max_length = 255) () =
  let schema =
    Cbor.Map
      [
        (Cbor.Text "type", Cbor.Text "domain");
        (Cbor.Text "max_length", Cbor.Unsigned max_length);
      ]
  in
  {
    generate = (fun () -> cbor_text_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_text_value });
  }

let ip_address ?version () =
  let schema =
    match version with
    | Some `V4 -> Cbor.Map [ (Cbor.Text "type", Cbor.Text "ipv4") ]
    | Some `V6 -> Cbor.Map [ (Cbor.Text "type", Cbor.Text "ipv6") ]
    | None ->
        Cbor.Map
          [
            ( Cbor.Text "one_of",
              Cbor.Array
                [
                  Cbor.Map [ (Cbor.Text "type", Cbor.Text "ipv4") ];
                  Cbor.Map [ (Cbor.Text "type", Cbor.Text "ipv6") ];
                ] );
          ]
  in
  {
    generate = (fun () -> cbor_text_value (generate_raw schema));
    as_basic = (fun () -> Some { schema; parse = cbor_text_value });
  }
