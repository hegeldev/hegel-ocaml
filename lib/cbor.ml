type t =
  | Unsigned of int
  | Negative of int
  | Text of string
  | Bytes of string
  | Array of t list
  | Map of (t * t) list
  | Bool of bool
  | Null
  | Float of float
  | Tag of int * t

(* ---- Encoding ---- *)

let encode_u8 buf v = Buffer.add_char buf (Char.chr (v land 0xff))

let encode_u16_be buf v =
  encode_u8 buf (v lsr 8);
  encode_u8 buf v

let encode_u32_be buf v =
  encode_u16_be buf (v lsr 16);
  encode_u16_be buf v

let encode_u64_be buf v =
  encode_u32_be buf (v lsr 32);
  encode_u32_be buf v

let encode_head buf major value =
  let major_shifted = major lsl 5 in
  if value < 24 then encode_u8 buf (major_shifted lor value)
  else if value < 0x100 then (
    encode_u8 buf (major_shifted lor 24);
    encode_u8 buf value)
  else if value < 0x10000 then (
    encode_u8 buf (major_shifted lor 25);
    encode_u16_be buf value)
  else if value < 0x100000000 then (
    encode_u8 buf (major_shifted lor 26);
    encode_u32_be buf value)
  else (
    encode_u8 buf (major_shifted lor 27);
    encode_u64_be buf value)

let encode_float64 buf f =
  let bits = Int64.bits_of_float f in
  encode_u8 buf ((7 lsl 5) lor 27);
  for i = 7 downto 0 do
    encode_u8 buf
      (Int64.to_int (Int64.shift_right_logical bits (i * 8)) land 0xff)
  done

let rec encode buf = function
  | Unsigned n -> encode_head buf 0 n
  | Negative n ->
      (* CBOR negative: -1-n stored as major type 1, value n *)
      encode_head buf 1 (-1 - n)
  | Bytes s ->
      encode_head buf 2 (String.length s);
      Buffer.add_string buf s
  | Text s ->
      encode_head buf 3 (String.length s);
      Buffer.add_string buf s
  | Array items ->
      encode_head buf 4 (List.length items);
      List.iter (encode buf) items
  | Map pairs ->
      encode_head buf 5 (List.length pairs);
      List.iter
        (fun (k, v) ->
          encode buf k;
          encode buf v)
        pairs
  | Tag (tag, value) ->
      encode_head buf 6 tag;
      encode buf value
  | Bool true -> encode_u8 buf ((7 lsl 5) lor 21)
  | Bool false -> encode_u8 buf ((7 lsl 5) lor 20)
  | Null -> encode_u8 buf ((7 lsl 5) lor 22)
  | Float f -> encode_float64 buf f

let encode_to_string v =
  let buf = Buffer.create 64 in
  encode buf v;
  Buffer.contents buf

(* ---- Decoding ---- *)

let get_u8 s off = Char.code (String.get s off)
let get_u16_be s off = (get_u8 s off lsl 8) lor get_u8 s (off + 1)

let get_u32_be s off =
  (get_u8 s off lsl 24)
  lor (get_u8 s (off + 1) lsl 16)
  lor (get_u8 s (off + 2) lsl 8)
  lor get_u8 s (off + 3)

let get_u64_be s off =
  (get_u8 s off lsl 56)
  lor (get_u8 s (off + 1) lsl 48)
  lor (get_u8 s (off + 2) lsl 40)
  lor (get_u8 s (off + 3) lsl 32)
  lor (get_u8 s (off + 4) lsl 24)
  lor (get_u8 s (off + 5) lsl 16)
  lor (get_u8 s (off + 6) lsl 8)
  lor get_u8 s (off + 7)

let get_float64_be s off =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits :=
      Int64.logor (Int64.shift_left !bits 8) (Int64.of_int (get_u8 s (off + i)))
  done;
  Int64.float_of_bits !bits

let decode_head s off =
  let b = get_u8 s off in
  let major = b lsr 5 in
  let additional = b land 0x1f in
  if additional < 24 then (major, additional, off + 1)
  else if additional = 24 then (major, get_u8 s (off + 1), off + 2)
  else if additional = 25 then (major, get_u16_be s (off + 1), off + 3)
  else if additional = 26 then (major, get_u32_be s (off + 1), off + 5)
  else if additional = 27 then (major, get_u64_be s (off + 1), off + 9)
  else
    failwith (Printf.sprintf "CBOR: unsupported additional info %d" additional)

let rec decode s off =
  let b = get_u8 s off in
  let major = b lsr 5 in
  let additional = b land 0x1f in
  match major with
  | 0 ->
      let _, value, off' = decode_head s off in
      (Unsigned value, off')
  | 1 ->
      let _, value, off' = decode_head s off in
      (Negative (-1 - value), off')
  | 2 ->
      let _, len, off' = decode_head s off in
      (Bytes (String.sub s off' len), off' + len)
  | 3 ->
      let _, len, off' = decode_head s off in
      (Text (String.sub s off' len), off' + len)
  | 4 ->
      let _, count, off' = decode_head s off in
      let items = ref [] in
      let pos = ref off' in
      for _ = 1 to count do
        let item, next = decode s !pos in
        items := item :: !items;
        pos := next
      done;
      (Array (List.rev !items), !pos)
  | 5 ->
      let _, count, off' = decode_head s off in
      let pairs = ref [] in
      let pos = ref off' in
      for _ = 1 to count do
        let k, next_k = decode s !pos in
        let v, next_v = decode s next_k in
        pairs := (k, v) :: !pairs;
        pos := next_v
      done;
      (Map (List.rev !pairs), !pos)
  | 6 ->
      let _, tag, off' = decode_head s off in
      let value, off'' = decode s off' in
      (Tag (tag, value), off'')
  | 7 ->
      if additional = 20 then (Bool false, off + 1)
      else if additional = 21 then (Bool true, off + 1)
      else if additional = 22 then (Null, off + 1)
      else if additional = 27 then
        let f = get_float64_be s (off + 1) in
        (Float f, off + 9)
      else if additional = 25 then
        (* half-precision float *)
        let half = get_u16_be s (off + 1) in
        let sign = (half lsr 15) land 1 in
        let exp = (half lsr 10) land 0x1f in
        let mant = half land 0x3ff in
        let f =
          if exp = 0 then
            (* subnormal *)
            let v = Float.ldexp (Float.of_int mant) (-24) in
            if sign = 1 then -.v else v
          else if exp = 31 then
            if mant = 0 then
              if sign = 1 then Float.neg_infinity else Float.infinity
            else Float.nan
          else
            let v = Float.ldexp (Float.of_int (mant + 1024)) (exp - 25) in
            if sign = 1 then -.v else v
        in
        (Float f, off + 3)
      else if additional = 26 then
        (* single-precision float *)
        let bits = Int32.of_int (get_u32_be s (off + 1)) in
        let f = Int32.float_of_bits bits in
        (Float f, off + 5)
      else
        failwith (Printf.sprintf "CBOR: unsupported simple value %d" additional)
  | _ ->
      (* major = (byte lsr 5) land 7 is always 0-7, all handled above *)
      assert false

let decode_string s =
  let v, _ = decode s 0 in
  v

(* ---- Helpers ---- *)

let map_get cbor key =
  match cbor with
  | Map pairs ->
      let rec find = function
        | [] -> None
        | (Text k, v) :: _ when k = key -> Some v
        | _ :: rest -> find rest
      in
      find pairs
  | _ -> None

let map_get_exn cbor key =
  match map_get cbor key with
  | Some v -> v
  | None -> failwith (Printf.sprintf "CBOR map: missing key %S" key)

let as_text = function Text s -> Some s | _ -> None
let as_bool = function Bool b -> Some b | _ -> None

let as_float = function
  | Float f -> Some f
  | Unsigned n -> Some (Float.of_int n)
  | Negative n -> Some (Float.of_int n)
  | _ -> None

let as_array = function Array l -> Some l | _ -> None
let as_map = function Map l -> Some l | _ -> None

let as_int = function
  | Unsigned n -> Some n
  | Negative n -> Some n
  | Float f when Float.is_integer f -> Some (Float.to_int f)
  | _ -> None

let rec to_diagnostic = function
  | Unsigned n -> string_of_int n
  | Negative n -> string_of_int n
  | Text s -> Printf.sprintf "%S" s
  | Bytes s ->
      Printf.sprintf "h'%s'"
        (String.concat ""
           (List.init (String.length s) (fun i ->
                Printf.sprintf "%02x" (Char.code s.[i]))))
  | Array items -> "[" ^ String.concat ", " (List.map to_diagnostic items) ^ "]"
  | Map pairs ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (k, v) -> to_diagnostic k ^ ": " ^ to_diagnostic v)
             pairs)
      ^ "}"
  | Bool true -> "true"
  | Bool false -> "false"
  | Null -> "null"
  | Float f -> string_of_float f
  | Tag (tag, value) -> Printf.sprintf "%d(%s)" tag (to_diagnostic value)
