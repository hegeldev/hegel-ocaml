(** Shared scrubbing helper for expect-test snapshots. *)

let scrub_numeric_after ~marker ~placeholder s =
  let is_digit c = c >= '0' && c <= '9' in
  let ml = String.length marker in
  let sl = String.length s in
  let buf = Buffer.create sl in
  let rec go i =
    if i >= sl
    then ()
    else if i + ml <= sl && String.equal (String.sub s i ml) marker
    then (
      Buffer.add_string buf marker;
      let j = ref (i + ml) in
      while !j < sl && is_digit s.[!j] do
        incr j
      done;
      Buffer.add_string buf placeholder;
      go !j)
    else (
      Buffer.add_char buf s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents buf
;;

let scrub_blobs s =
  let marker = "failure_blobs" in
  let ml = String.length marker in
  let sl = String.length s in
  let buf = Buffer.create sl in
  let rec go i =
    if i >= sl
    then ()
    else if i + ml <= sl && String.equal (String.sub s i ml) marker
    then (
      Buffer.add_string buf marker;
      let q1 = ref (i + ml) in
      while !q1 < sl && not (Char.equal s.[!q1] '"') do
        incr q1
      done;
      Buffer.add_string buf (String.sub s (i + ml) (!q1 - (i + ml)));
      if !q1 >= sl
      then go !q1
      else (
        Buffer.add_char buf '"';
        let q2 = ref (!q1 + 1) in
        while !q2 < sl && not (Char.equal s.[!q2] '"') do
          incr q2
        done;
        Buffer.add_string buf "<BLOB>";
        if !q2 >= sl
        then go !q2
        else (
          Buffer.add_char buf '"';
          go (!q2 + 1))))
    else (
      Buffer.add_char buf s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents buf
;;

(* OxCaml automatically adds draw positions. Keep general value-printing
   snapshots shared across compilers; position tests inspect the raw output. *)
let scrub_draw_positions s =
  let open Core in
  String.split s ~on:'\n'
  |> List.map ~f:(fun line ->
    match
      String.substr_index line ~pattern:" @ ", String.substr_index line ~pattern:" = "
    with
    | Some start, Some stop when start < stop ->
      String.prefix line start ^ String.drop_prefix line stop
    | _ -> line)
  |> String.concat ~sep:"\n"
;;

let scrub_report ?(hide_draw_positions = true) s =
  (if hide_draw_positions then scrub_draw_positions s else s)
  |> Expect_test_helpers_core.hide_positions_in_string
  |> scrub_numeric_after ~marker:".ml:" ~placeholder:"<LINE>"
  |> scrub_blobs
;;
