(** Shared test utilities used across test modules. *)

(** [unsetenv name] removes the environment variable [name] from the process
    environment. Uses the POSIX [unsetenv(3)] function via a C stub. *)
external unsetenv : string -> unit = "caml_unsetenv"

open! Core
module Unix = Core_unix

(** [with_tempdir ~prefix ~f] creates a tempdir via [mkdtemp] under the system
    temp directory (honoring [TMPDIR]), using [prefix] as the leaf-name prefix.
    It passes the tempdir's path to [f], and removes the directory (and any flat
    files inside it) on exit — including on exception. Intended for tests whose
    tempdirs only contain top-level files; subdirectories are not recursively
    removed. *)
let with_tempdir ~prefix ~f =
  let dir =
    Core_unix.mkdtemp (Filename.concat (Stdlib.Filename.get_temp_dir_name ()) prefix)
  in
  Exn.protect
    ~finally:(fun () ->
      (try
         Stdlib.Sys.readdir dir
         |> Array.iter ~f:(fun name ->
           try Stdlib.Sys.remove (Filename.concat dir name) with
           | _ -> ())
       with
       | _ -> ());
      try Core_unix.rmdir dir with
      | _ -> ())
    ~f:(fun () -> f dir)
;;

let expect_usage_error gen substring =
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
      ignore (Hegel.draw tc gen))
  with
  | exception Hegel.Usage_error msg ->
    Alcotest.(check bool)
      (Printf.sprintf "diagnostic %S contains %S" msg substring)
      true
      (String.is_substring msg ~substring)
  | () -> Alcotest.fail "expected Usage_error"
;;

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s]. *)
let contains_substring s sub =
  let slen = String.length s
  and sublen = String.length sub in
  if sublen > slen
  then false
  else (
    let rec check i =
      if i > slen - sublen
      then false
      else if String.equal (String.sub s ~pos:i ~len:sublen) sub
      then true
      else check (i + 1)
    in
    check 0)
;;

let parallel_concurrency : Hegel.Concurrency.t =
  { spawn_join_n =
      (fun ~n ~f ->
        Stdlib.List.init n (fun i ->
          (Stdlib.Domain.spawn [@alert "-do_not_spawn_domains-unsafe_multidomain"])
            (fun () -> f i))
        |> Stdlib.List.map Stdlib.Domain.join)
  }
;;

let sequential_concurrency : Hegel.Concurrency.t =
  { spawn_join_n = (fun ~n ~f -> Stdlib.List.init n f) }
;;
