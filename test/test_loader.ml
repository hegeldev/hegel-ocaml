(** Tests for the libhegel download protocol ([Hegel_ffi.Loader]).

    The protocol is exercised against a stubbed [curl] (a shell script placed
    first on [PATH]) that writes a fixed payload in two chunks with a pause
    between them, widening the window of any race between concurrent
    downloaders. The tests supply their own expected checksum, so no network
    access or real libhegel artifact is involved. *)

open! Core
module Loader = Hegel_ffi.Loader

let payload = "first half of the payload second half of the payload"

(* A [curl] stand-in: honors [-o <dest>] and writes {!payload} to it in two
   chunks with a pause in between. [FAKE_CURL_EXIT] forces a failure exit
   code (simulating a network error) without writing anything. *)
let fake_curl_script =
  {|#!/bin/sh
if [ -n "${FAKE_CURL_EXIT:-}" ]; then exit "$FAKE_CURL_EXIT"; fi
out=
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then out=$2; shift 2; else shift 1; fi
done
printf 'first half of the payload ' > "$out"
sleep 0.4
printf 'second half of the payload' >> "$out"
|}
;;

(* Set up a tempdir containing the stubbed [curl] and pass its path, the
   PATH value that puts the stub first, and the payload's checksum to [f].
   The checksum is computed with the loader's own [sha256_of_file] so the
   test agrees with the implementation about hashing. *)
let with_download_fixture ~f =
  Test_helpers.with_tempdir ~prefix:"hegel-loader-test-" ~f:(fun dir ->
    let curl_path = Filename.concat dir "curl" in
    Out_channel.write_all curl_path ~data:fake_curl_script;
    Core_unix.chmod curl_path ~perm:0o755;
    let reference = Filename.concat dir "reference-payload" in
    Out_channel.write_all reference ~data:payload;
    let expected = Loader.sha256_of_file reference in
    let stub_path =
      dir ^ ":" ^ Option.value (Sys.getenv "PATH") ~default:"/usr/bin:/bin"
    in
    f ~dir ~stub_path ~expected)
;;

(* Run [f] with PATH set to [path], restoring the previous value on exit. *)
let with_path path ~f =
  let prev = Sys.getenv "PATH" in
  Core_unix.putenv ~key:"PATH" ~data:path;
  Exn.protect
    ~finally:(fun () ->
      match prev with
      | Some v -> Core_unix.putenv ~key:"PATH" ~data:v
      | None -> Test_helpers.unsetenv "PATH")
    ~f
;;

let leftover_tmp_files dir =
  Stdlib.Sys.readdir dir
  |> Array.to_list
  |> List.filter ~f:(fun name -> Test_helpers.contains_substring name ".tmp")
;;

(* Regression test for the cold-cache download race: multiple processes (e.g.
   several test binaries starting concurrently on a fresh machine) download
   libhegel at the same time. Every downloader must end up with a verified
   library, and the cache file must not be corrupted. Before the fix all
   processes shared a single temporary path ([<cache>.tmp]), so the first
   process to finish renamed the file the others were still writing out from
   under them and the losers crashed. *)
let test_concurrent_downloads_all_succeed () =
  with_download_fixture ~f:(fun ~dir ~stub_path ~expected ->
    let cache_path = Filename.concat dir "libhegel.so" in
    let n = 4 in
    let children =
      List.init n ~f:(fun i ->
        match Core_unix.fork () with
        | `In_the_parent pid -> pid
        | `In_the_child ->
          let status =
            try
              Core_unix.putenv ~key:"PATH" ~data:stub_path;
              (* Stagger the children so their downloads overlap at
                 different phases rather than starting in lockstep. *)
              ignore (Core_unix.nanosleep (Float.of_int i *. 0.15));
              let got =
                Loader.download_verified
                  ~url:"http://example.invalid/libhegel.so"
                  ~expected
                  ~cache_path
              in
              if String.equal (In_channel.read_all got) payload then 0 else 1
            with
            | _ -> 1
          in
          Core_unix.exit_immediately status)
    in
    let failures =
      List.count children ~f:(fun pid -> Result.is_error (Core_unix.waitpid pid))
    in
    Alcotest.(check int) "all concurrent downloaders succeed" 0 failures;
    Alcotest.(check string)
      "cache file holds the verified payload"
      payload
      (In_channel.read_all cache_path);
    Alcotest.(check (list string))
      "no leftover temporary files"
      []
      (leftover_tmp_files dir))
;;

(* A stale temporary file left by a killed downloader (partial download) must
   not break, or leak into, a later download. *)
let test_stale_tmp_file_is_harmless () =
  with_download_fixture ~f:(fun ~dir ~stub_path ~expected ->
    let cache_path = Filename.concat dir "libhegel.so" in
    Out_channel.write_all (cache_path ^ ".tmp") ~data:"partial garbage";
    Out_channel.write_all (cache_path ^ ".12345.abc123.tmp") ~data:"more garbage";
    let got =
      with_path stub_path ~f:(fun () ->
        Loader.download_verified
          ~url:"http://example.invalid/libhegel.so"
          ~expected
          ~cache_path)
    in
    Alcotest.(check string) "returns the cache path" cache_path got;
    Alcotest.(check string)
      "cache file holds the verified payload"
      payload
      (In_channel.read_all cache_path))
;;

let test_checksum_mismatch_fails_and_cleans_up () =
  with_download_fixture ~f:(fun ~dir ~stub_path ~expected:_ ->
    let cache_path = Filename.concat dir "libhegel.so" in
    let raised =
      with_path stub_path ~f:(fun () ->
        try
          ignore
            (Loader.download_verified
               ~url:"http://example.invalid/libhegel.so"
               ~expected:(String.make 64 '0')
               ~cache_path);
          false
        with
        | Failure msg -> Test_helpers.contains_substring msg "SHA-256 mismatch")
    in
    Alcotest.(check bool) "raises a SHA-256 mismatch failure" true raised;
    Alcotest.(check bool)
      "does not install the corrupt file"
      false
      (Stdlib.Sys.file_exists cache_path);
    Alcotest.(check (list string))
      "no leftover temporary files"
      []
      (leftover_tmp_files dir))
;;

let test_download_failure_fails_and_cleans_up () =
  with_download_fixture ~f:(fun ~dir ~stub_path ~expected ->
    let cache_path = Filename.concat dir "libhegel.so" in
    let prev = Sys.getenv "FAKE_CURL_EXIT" in
    let raised =
      Exn.protect
        ~finally:(fun () ->
          match prev with
          | Some v -> Core_unix.putenv ~key:"FAKE_CURL_EXIT" ~data:v
          | None -> Test_helpers.unsetenv "FAKE_CURL_EXIT")
        ~f:(fun () ->
          Core_unix.putenv ~key:"FAKE_CURL_EXIT" ~data:"9";
          with_path stub_path ~f:(fun () ->
            try
              ignore
                (Loader.download_verified
                   ~url:"http://example.invalid/libhegel.so"
                   ~expected
                   ~cache_path);
              false
            with
            | Failure msg -> Test_helpers.contains_substring msg "curl exit 9"))
    in
    Alcotest.(check bool) "raises a download failure" true raised;
    Alcotest.(check bool)
      "does not install anything"
      false
      (Stdlib.Sys.file_exists cache_path);
    Alcotest.(check (list string))
      "no leftover temporary files"
      []
      (leftover_tmp_files dir))
;;

let tests =
  [ Alcotest.test_case
      "concurrent cold-cache downloads all succeed"
      `Quick
      test_concurrent_downloads_all_succeed
  ; Alcotest.test_case "stale tmp file is harmless" `Quick test_stale_tmp_file_is_harmless
  ; Alcotest.test_case
      "checksum mismatch fails and cleans up"
      `Quick
      test_checksum_mismatch_fails_and_cleans_up
  ; Alcotest.test_case
      "download failure fails and cleans up"
      `Quick
      test_download_failure_fails_and_cleans_up
  ]
;;
