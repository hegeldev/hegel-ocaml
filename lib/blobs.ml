(** Source-file splicing for [[@@blobs]] failure-blob recording. See the
    accompanying [.mli] for the public API. *)

open! Core

type t =
  { recorded : string list
  ; file : string
  ; payload_start : int
  ; payload_end : int
  }

(** [render_list blobs] returns an OCaml-source list literal whose elements
    are the strings in [blobs], escaped with [String.escaped]. Empty input
    yields the canonical empty-list literal. *)
let render_list blobs =
  match blobs with
  | [] -> "[]"
  | _ ->
    let items = List.map blobs ~f:(fun b -> Printf.sprintf "\"%s\"" (String.escaped b)) in
    "[ " ^ String.concat ~sep:"; " items ^ " ]"
;;

(** [resolve_path file] returns the absolute path under which to read the
    source and write the [.corrected] sibling. The PPX supplies
    [pos_fname] as a workspace-relative path; the test binary's cwd is
    the workspace root, so joining them lands the [.corrected] file next
    to the original source file in the source tree (which is also where
    [dune promote] looks). Absolute inputs are passed through. *)
let resolve_path file =
  if Filename.is_relative file then Filename.concat (Stdlib.Sys.getcwd ()) file else file
;;

let write_corrected t ~blobs =
  let path = resolve_path t.file in
  let original = In_channel.read_all path in
  let prefix = String.sub original ~pos:0 ~len:t.payload_start in
  let suffix =
    String.sub original ~pos:t.payload_end ~len:(String.length original - t.payload_end)
  in
  let corrected = prefix ^ render_list blobs ^ suffix in
  Out_channel.write_all (path ^ ".corrected") ~data:corrected
;;
