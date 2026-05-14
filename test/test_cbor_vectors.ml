(** Each JSON entry becomes one Alcotest case that:

    1. Hex-decodes the [hex] field to obtain the wire-format CBOR.
    2. Decodes with [Cbor.Simple.decode] and checks the diagnostic string
       against the expected [diagnostic], or the JSON-converted value against
       the expected [decoded] JSON.
*)

module Simple = Cbor.Simple

type result =
  | Decoded of Yojson.Safe.t
  | Diagnostic of string

type entry =
  { hex : string
  ; result : result
  }

let of_hex s =
  assert (String.length s mod 2 = 0);
  let n = String.length s / 2 in
  let r = Bytes.create n in
  for i = 0 to pred n do
    Bytes.set r i (Char.chr (int_of_string ("0x" ^ String.sub s (i * 2) 2)))
  done;
  Bytes.to_string r
;;

let parse_entries json =
  Yojson.Safe.from_string json
  |> Yojson.Safe.Util.to_list
  |> List.map (function
    | `Assoc a ->
      let hex = Yojson.Safe.Util.to_string (List.assoc "hex" a) in
      let result =
        match List.assoc_opt "diagnostic" a with
        | Some d -> Diagnostic (Yojson.Safe.Util.to_string d)
        | None -> Decoded (List.assoc "decoded" a)
      in
      { hex; result }
    | _ -> assert false)
;;

let rec json_of_cbor : Simple.t -> Yojson.Safe.t = function
  | (`Null | `Bool _ | `Int _ | `Float _) as x -> x
  | `Undefined | `Simple _ -> `Null
  | `Bytes x -> `String x
  | `Text x -> `String x
  | `Array x -> `List (List.map json_of_cbor x)
  | `Map x ->
    `Assoc
      (List.map
         (fun (k, v) ->
            match k with
            | `Text s -> s, json_of_cbor v
            | _ -> failwith "json_of_cbor: expected string key")
         x)
  | `Tag (t, v) ->
    `String (Printf.sprintf "%d(%s)" t (Yojson.Safe.to_string (json_of_cbor v)))
;;

(** Vectors whose [decoded] value falls outside what [Cbor.Simple] can
    represent — 64-bit unsigned literals and bignum tags. The upstream test
    has the same exclusion list. *)
let appendix_a_ignored = [ 10; 11; 12; 13; 71 ]

let run_entry path idx entry =
  let cbor = Simple.decode (of_hex entry.hex) in
  let diag = Simple.to_diagnostic cbor in
  match entry.result with
  | Diagnostic s ->
    if not (String.equal s diag)
    then Alcotest.failf "%s[%d]: expected diagnostic %s, got %s" path idx s diag
  | Decoded json ->
    let actual = json_of_cbor cbor in
    if json <> actual
    then
      Alcotest.failf
        "%s[%d]: expected %s, got %s"
        path
        idx
        (Yojson.Safe.to_string json)
        (Yojson.Safe.to_string actual)
;;

let cases_of label entries ignored =
  List.mapi
    (fun idx entry ->
       let name = Printf.sprintf "%s [%d] %s" label idx entry.hex in
       let test () =
         if List.mem idx ignored
         then ()
         else (
           try run_entry label idx entry with
           | exn ->
             let msg =
               match exn with
               | Failure s -> s
               | _ -> Printexc.to_string exn
             in
             Alcotest.failf "%s[%d]: %s" label idx msg)
       in
       Alcotest.test_case name `Quick test)
    entries
;;

let tests =
  cases_of
    "cbor_appendix_a.json"
    (parse_entries Cbor_appendix_a_data.json)
    appendix_a_ignored
;;
