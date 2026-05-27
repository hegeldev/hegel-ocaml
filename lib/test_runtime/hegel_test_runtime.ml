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

let run_all () =
  let tests = registered () in
  let failures = ref 0 in
  List.iter
    (fun t ->
       match t.run () with
       | () -> Printf.printf "  PASS  %s (%s:%d)\n%!" t.name t.file t.line
       | exception e ->
         incr failures;
         Printf.printf
           "  FAIL  %s (%s:%d)\n        %s\n%!"
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
    Printf.eprintf "\n%d test(s) failed\n%!" failures;
    Stdlib.exit 1)
  else (
    Printf.printf "\nAll tests passed\n%!";
    Stdlib.exit 0)
;;
