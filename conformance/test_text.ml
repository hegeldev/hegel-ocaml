(** Conformance binary: generates text values and writes metrics. *)

open Hegel.Conformance
open Hegel.Generators
open Hegel.Cbor_helpers

(** [utf8_length s] counts the number of Unicode codepoints in a UTF-8 string.
    Each codepoint can be 1-4 bytes. *)
let utf8_length s =
  let n = String.length s in
  let count = ref 0 in
  let i = ref 0 in
  while !i < n do
    let b = Char.code s.[!i] in
    let skip =
      if b land 0x80 = 0 then 1
      else if b land 0xE0 = 0xC0 then 2
      else if b land 0xF0 = 0xE0 then 3
      else 4
    in
    i := !i + skip;
    incr count
  done;
  !count

let () =
  let params_str = if Array.length Sys.argv > 1 then Sys.argv.(1) else "{}" in
  let params = Json_params.parse params_str in
  let min_size = Json_params.get_int params "min_size" 0 in
  let max_size = Json_params.get_int_opt params "max_size" in
  let test_cases = get_test_cases () in
  Hegel.Session.run_hegel_test ~name:"test_text" ~test_cases (fun () ->
      let v = generate (text ~min_size ?max_size ()) in
      let s = extract_string v in
      let length = utf8_length s in
      write_metrics [ ("length", string_of_int length) ])
