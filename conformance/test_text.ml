(** Conformance binary: generates text values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators

(** [utf8_codepoints s] returns the list of Unicode codepoint values from a
    UTF-8 encoded string. Each multi-byte sequence is decoded into its codepoint
    integer value. *)
let utf8_codepoints s =
  let n = String.length s in
  let rec collect i acc =
    if i >= n then List.rev acc
    else
      let b0 = Char.code s.[i] in
      if b0 land 0x80 = 0 then collect (i + 1) (b0 :: acc)
      else if b0 land 0xE0 = 0xC0 then
        let b1 = Char.code s.[i + 1] in
        let cp = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F) in
        collect (i + 2) (cp :: acc)
      else if b0 land 0xF0 = 0xE0 then
        let b1 = Char.code s.[i + 1] in
        let b2 = Char.code s.[i + 2] in
        let cp =
          ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
        in
        collect (i + 3) (cp :: acc)
      else
        let b1 = Char.code s.[i + 1] in
        let b2 = Char.code s.[i + 2] in
        let b3 = Char.code s.[i + 3] in
        let cp =
          ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F)
        in
        collect (i + 4) (cp :: acc)
  in
  collect 0 []

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let codec = Json_params.get_string_opt params "codec" in
  let min_codepoint = Json_params.get_int_opt params "min_codepoint" in
  let max_codepoint = Json_params.get_int_opt params "max_codepoint" in
  let categories =
    match List.assoc_opt "categories" params with
    | Some (`List _) -> Some (Json_params.get_string_list params "categories")
    | _ -> None
  in
  let exclude_categories =
    match List.assoc_opt "exclude_categories" params with
    | Some (`List _) ->
        Some (Json_params.get_string_list params "exclude_categories")
    | _ -> None
  in
  let include_characters =
    Json_params.get_string_opt params "include_characters"
  in
  let exclude_characters =
    Json_params.get_string_opt params "exclude_characters"
  in
  let test_cases = get_test_cases () in
  Hegel.run_hegel_test ~settings:(Hegel.Client.settings ~test_cases ())
    (fun tc ->
      let s =
        Hegel.draw tc
          (text ~min_size ?max_size ?codec ?min_codepoint ?max_codepoint
             ?categories ?exclude_categories ?include_characters
             ?exclude_characters ())
      in
      let codepoints = utf8_codepoints s in
      let codepoints_json =
        "[" ^ String.concat ", " (List.map string_of_int codepoints) ^ "]"
      in
      write_metrics [ ("codepoints", codepoints_json) ])
