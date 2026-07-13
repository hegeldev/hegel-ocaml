(** Runtime registry and runner for [let%hegel_test] tests. See the
    accompanying [.mli] for the public API. *)

type test =
  { name : string
  ; file : string
  ; line : int
  ; run : unit -> unit
  }

let registry : test list ref = ref []
let registry_lock = Mutex.create ()

let register ~name ~file ~line run =
  Mutex.lock registry_lock;
  registry := { name; file; line; run } :: !registry;
  Mutex.unlock registry_lock
;;

let registered () =
  Mutex.lock registry_lock;
  let snapshot = List.rev !registry in
  Mutex.unlock registry_lock;
  snapshot
;;

let stderr_color code s =
  let color_enabled =
    match Sys.getenv_opt "HEGEL_COLOR" with
    | Some "1" -> true
    | Some "0" -> false
    | Some _ | None -> Unix.isatty Unix.stdout
  in
  if color_enabled then Printf.sprintf "\027[%sm%s\027[0m" code s else s
;;

let green = "32"
let red = "31"

let run_all () =
  let tests = registered () in
  let failures = ref 0 in
  List.iter
    (fun t ->
       match t.run () with
       | () ->
         Printf.printf
           "  %s  %s (%s:%d)\n%!"
           (stderr_color green "PASS")
           t.name
           t.file
           t.line
       | exception e ->
         incr failures;
         Printf.printf
           "  %s  %s (%s:%d)\n        %s\n%!"
           (stderr_color red "FAIL")
           t.name
           t.file
           t.line
           (Printexc.to_string e))
    tests;
  !failures
;;

let test_main () =
  let failures = run_all () in
  if failures > 0
  then (
    Printf.eprintf
      "\n%s\n%!"
      (stderr_color red (Printf.sprintf "%d test(s) failed" failures));
    Stdlib.exit 1)
  else (
    Printf.printf "\n%s\n%!" (stderr_color green "All tests passed");
    Stdlib.exit 0)
;;
