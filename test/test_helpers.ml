(** Shared test utilities used across test modules. *)

(** [unsetenv name] removes the environment variable [name] from the process
    environment. Uses the POSIX [unsetenv(3)] function via a C stub. *)
external unsetenv : string -> unit = "caml_unsetenv"

open! Core
module Unix = Core_unix

(** [with_tempdir ~prefix ~f] creates a tempdir via [mkdtemp] under the system
    temp directory (honoring [TMPDIR]), using [prefix] as the leaf-name prefix.
    It passes the tempdir's path to [f], and removes the directory (and any
    flat files inside it) on exit — including on exception. Intended for tests
    whose tempdirs only contain top-level files; subdirectories are not
    recursively removed. *)
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

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
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

(** Helper: check where a given command is *)
let find_cmd cmd =
  let inp_stream = Core_unix.open_process_in ("which " ^ cmd) in
  let output = String.strip (In_channel.input_all inp_stream) in
  let exit_code = Core_unix.close_process_in inp_stream in
  match exit_code with
  | Ok () -> output
  | Error err ->
    raise
      (Failure
         (sprintf
            "Command failed with status: %s"
            (match err with
             | `Exit_non_zero code -> Int.to_string code
             | `Signal signal -> "signaled: " ^ Signal.to_string signal)))
;;
