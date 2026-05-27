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
    [pos_fname] as a workspace-relative path; cwd differs by harness —
    Alcotest tests run from the workspace root, while inline-tests
    runners run from the per-library directory inside dune's sandbox
    (which mirrors the source tree by hardlinks). We try the full
    relative path first (workspace-root case), then fall back to just
    the basename (sandbox per-library case). Absolute inputs are passed
    through. *)
let resolve_path file =
  if not (Filename.is_relative file)
  then file
  else (
    let cwd = Stdlib.Sys.getcwd () in
    let with_full = Filename.concat cwd file in
    if Stdlib.Sys.file_exists with_full
    then with_full
    else Filename.concat cwd (Filename.basename file))
;;

(** Flag flipped to [true] by {!write_corrected}; read by the expect
    backend's evaluator to decide between an exit-0 (so dune's [(diff?)]
    action runs) and a failure exit. *)
let corrected_written = ref false

let any_corrected_written () = !corrected_written

let write_corrected t ~blobs =
  let path = resolve_path t.file in
  let original = In_channel.read_all path in
  let prefix = String.sub original ~pos:0 ~len:t.payload_start in
  let suffix =
    String.sub original ~pos:t.payload_end ~len:(String.length original - t.payload_end)
  in
  let corrected = prefix ^ render_list blobs ^ suffix in
  Out_channel.write_all (path ^ ".corrected") ~data:corrected;
  corrected_written := true
;;
