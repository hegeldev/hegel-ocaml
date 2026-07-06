(* Build-time helper: picks the ppxlib AST-compatibility variant for
   [ppx_compat.ml] and writes it to stdout. This is a portable (stdlib-only)
   replacement for the former [(bash ...)] rule so the PPX builds on Windows,
   which has no [bash]/[grep]/[cut].

   Invoked by the rule in [dune] as:
     select_ppx_compat <ocaml_version> <ppxlib-META> \
       <oxcaml.ml> <post-53.ml> <pre-53.ml>

   Selection (mirrors the old shell logic):
   - an OxCaml compiler ([+ox] in the version) -> the OxCaml variant;
   - otherwise ppxlib >= 0.36 (the OCaml 5.3 labeled-tuple AST) -> the post-5.3
     variant, and older ppxlib -> the pre-5.3 variant. *)

let contains s sub =
  let n = String.length s
  and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  m = 0 || at 0
;;

let starts_with prefix s =
  let p = String.length prefix in
  String.length s >= p && String.sub s 0 p = prefix
;;

(* Emit a variant file's contents verbatim to stdout. *)
let cat path =
  let ic = open_in_bin path in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  print_string contents
;;

(* [(major, minor)] of ppxlib's own version, read from its META. The version
   sits on a top-level [version = "X.Y.Z"] line (column 0); sub-package version
   lines are indented, so matching the raw line prefix skips them. *)
let ppxlib_major_minor meta_path =
  let ic = open_in meta_path in
  let found = ref None in
  (try
     while !found = None do
       let line = input_line ic in
       if starts_with "version" line
       then (
         match String.split_on_char '"' line with
         | _ :: v :: _ -> found := Some v
         | _ -> ())
     done
   with
   | End_of_file -> ());
  close_in ic;
  match !found with
  | None -> failwith "select_ppx_compat: no version field in ppxlib META"
  | Some v ->
    (match String.split_on_char '.' v with
     | major :: minor :: _ -> int_of_string major, int_of_string minor
     | [ major ] -> int_of_string major, 0
     | [] -> failwith "select_ppx_compat: empty ppxlib version")
;;

let () =
  let ocaml_version = Sys.argv.(1) in
  let meta = Sys.argv.(2) in
  let oxcaml = Sys.argv.(3) in
  let post_53 = Sys.argv.(4) in
  let pre_53 = Sys.argv.(5) in
  if contains ocaml_version "+ox"
  then cat oxcaml
  else (
    let major, minor = ppxlib_major_minor meta in
    if major > 0 || minor >= 36 then cat post_53 else cat pre_53)
;;
