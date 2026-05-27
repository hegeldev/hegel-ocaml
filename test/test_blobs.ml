(** Unit tests for {!Hegel.Blobs}. *)

open! Core
open Hegel

let with_tempdir = Test_helpers.with_tempdir

(** Write [contents] to [path/name] and return the absolute path. *)
let write_fixture ~dir ~name ~contents =
  let path = Filename.concat dir name in
  Out_channel.write_all path ~data:contents;
  path
;;

let read_corrected ~dir ~name =
  let path = Filename.concat dir (name ^ ".corrected") in
  In_channel.read_all path
;;

let test_empty_payload () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-empty-" ~f:(fun dir ->
    let prefix = "let%hegel_test foo tc = ()\n[@@blobs " in
    let suffix = "]\n;;\n" in
    let contents = prefix ^ "[]" ^ suffix in
    let payload_start = String.length prefix in
    let payload_end = payload_start + 2 in
    let path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let t = { Blobs.recorded = []; file = path; payload_start; payload_end } in
    Blobs.write_corrected t ~blobs:[];
    Alcotest.(check string)
      "empty list payload unchanged"
      contents
      (read_corrected ~dir ~name:"foo.ml"))
;;

let test_records_blobs () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-record-" ~f:(fun dir ->
    let prefix = "let%hegel_test foo tc = ()\n[@@blobs " in
    let suffix = "]\n;;\n" in
    let contents = prefix ^ "[]" ^ suffix in
    let payload_start = String.length prefix in
    let payload_end = payload_start + 2 in
    let path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let t = { Blobs.recorded = []; file = path; payload_start; payload_end } in
    Blobs.write_corrected t ~blobs:[ "AAAA"; "BBBB" ];
    let corrected = read_corrected ~dir ~name:"foo.ml" in
    let expected = prefix ^ "[ \"AAAA\"; \"BBBB\" ]" ^ suffix in
    Alcotest.(check string) "blobs spliced into payload range" expected corrected)
;;

let test_relative_path_resolves_against_cwd () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-relpath-" ~f:(fun dir ->
    let prefix = "[@@blobs " in
    let suffix = "]" in
    let contents = prefix ^ "[]" ^ suffix in
    let payload_start = String.length prefix in
    let payload_end = payload_start + 2 in
    let _path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let prev_cwd = Stdlib.Sys.getcwd () in
    Exn.protect
      ~finally:(fun () -> Stdlib.Sys.chdir prev_cwd)
      ~f:(fun () ->
        Stdlib.Sys.chdir dir;
        let t = { Blobs.recorded = []; file = "foo.ml"; payload_start; payload_end } in
        Blobs.write_corrected t ~blobs:[ "ZZ" ]);
    let corrected = read_corrected ~dir ~name:"foo.ml" in
    let expected = prefix ^ "[ \"ZZ\" ]" ^ suffix in
    Alcotest.(check string) "relative path resolved" expected corrected)
;;

let test_basename_fallback () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-fallback-" ~f:(fun dir ->
    let prefix = "[@@blobs " in
    let suffix = "]" in
    let contents = prefix ^ "[]" ^ suffix in
    let payload_start = String.length prefix in
    let payload_end = payload_start + 2 in
    let _path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let prev_cwd = Stdlib.Sys.getcwd () in
    Exn.protect
      ~finally:(fun () -> Stdlib.Sys.chdir prev_cwd)
      ~f:(fun () ->
        Stdlib.Sys.chdir dir;
        (* [file] is a workspace-relative path; from cwd it doesn't
           resolve directly (no [subdir/] subdir here), so resolve_path
           should fall back to the basename. *)
        let t =
          { Blobs.recorded = []; file = "subdir/foo.ml"; payload_start; payload_end }
        in
        Blobs.write_corrected t ~blobs:[ "Z" ]);
    let corrected = read_corrected ~dir ~name:"foo.ml" in
    let expected = prefix ^ "[ \"Z\" ]" ^ suffix in
    Alcotest.(check string) "basename fallback used" expected corrected)
;;

let test_any_corrected_written_flag () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-flag-" ~f:(fun dir ->
    let contents = "[@@blobs []]" in
    let payload_start = String.length "[@@blobs " in
    let payload_end = payload_start + 2 in
    let path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let t = { Blobs.recorded = []; file = path; payload_start; payload_end } in
    (* The ref is global per process, so the most we can assert is that
       [any_corrected_written] is [true] after a successful write. *)
    Blobs.write_corrected t ~blobs:[ "x" ];
    Alcotest.(check bool) "flag set after write" true (Blobs.any_corrected_written ()))
;;

let test_escaped_blob () =
  with_tempdir ~prefix:"/tmp/hegel-blobs-escape-" ~f:(fun dir ->
    let prefix = "[@@blobs " in
    let suffix = "]" in
    let contents = prefix ^ "[]" ^ suffix in
    let payload_start = String.length prefix in
    let payload_end = payload_start + 2 in
    let path = write_fixture ~dir ~name:"foo.ml" ~contents in
    let t = { Blobs.recorded = []; file = path; payload_start; payload_end } in
    Blobs.write_corrected t ~blobs:[ "a\"b\\c" ];
    let corrected = read_corrected ~dir ~name:"foo.ml" in
    let expected = prefix ^ "[ \"a\\\"b\\\\c\" ]" ^ suffix in
    Alcotest.(check string) "blob is OCaml-escaped" expected corrected)
;;

let suite =
  [ Alcotest.test_case "empty payload preserved" `Quick test_empty_payload
  ; Alcotest.test_case "records into payload range" `Quick test_records_blobs
  ; Alcotest.test_case
      "relative path resolved against cwd"
      `Quick
      test_relative_path_resolves_against_cwd
  ; Alcotest.test_case "blob string is escaped" `Quick test_escaped_blob
  ; Alcotest.test_case "resolve_path basename fallback" `Quick test_basename_fallback
  ; Alcotest.test_case
      "any_corrected_written tracks writes"
      `Quick
      test_any_corrected_written_flag
  ]
;;
