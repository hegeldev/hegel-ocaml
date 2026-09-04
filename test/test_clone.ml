open! Core
open Hegel
module G = Hegel.Generators

let one_case_settings () = Hegel.settings ~test_cases:1 ()
let small_int = G.integers ~min_value:0 ~max_value:9 ()

(* A clone can be drawn from within the test body, and the parent stays usable
   alongside it. *)
let test_clone_draws () =
  let parent = ref (-1) in
  let cloned = ref (-1) in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let worker = Hegel.clone tc in
    cloned := Hegel.draw_silent worker small_int;
    parent := Hegel.draw_silent tc small_int);
  Alcotest.(check bool) "parent value in range" true (!parent >= 0 && !parent <= 9);
  Alcotest.(check bool) "clone value in range" true (!cloned >= 0 && !cloned <= 9)
;;

(* Two handles — the parent and its clone — can be driven from two threads at
   once without tripping the engine's concurrent-use guard. *)
let test_clone_concurrent () =
  let worker_value = ref (-1) in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let worker = Hegel.clone tc in
    let t =
      Caml_threads.Thread.create
        (fun () -> worker_value := Hegel.draw_silent worker small_int)
        ()
    in
    let main = Hegel.draw_silent tc small_int in
    Caml_threads.Thread.join t;
    Alcotest.(check bool) "main value in range" true (main >= 0 && main <= 9));
  Alcotest.(check bool)
    "worker value in range"
    true
    (!worker_value >= 0 && !worker_value <= 9)
;;

(* A clone's stream is deterministic: replaying the same seeded case reproduces
   both the parent's and the clone's drawn values. *)
let test_clone_reproducible () =
  let run () =
    let pair = ref (0, 0) in
    Hegel.run_hegel_test
      ~settings:(one_case_settings () |> with_seed (Some 42))
      (fun tc ->
         let p = Hegel.draw_silent tc small_int in
         let c = Hegel.draw_silent (Hegel.clone tc) small_int in
         pair := p, c);
    !pair
  in
  let a = run () in
  let b = run () in
  Alcotest.(check (pair int int)) "seeded clone draws reproduce" a b
;;

(* Cloning a clone yields a further independent stream that also draws cleanly. *)
let test_clone_of_clone () =
  let value = ref (-1) in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let c1 = Hegel.clone tc in
    let c2 = Hegel.clone c1 in
    value := Hegel.draw_silent c2 small_int);
  Alcotest.(check bool) "nested clone value in range" true (!value >= 0 && !value <= 9)
;;

(* Dropping a clone and forcing a collection runs its finaliser, which releases
   the native handle and context. *)
let test_clone_finalized () =
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    for _ = 1 to 3 do
      let (_ : int) = Hegel.draw_silent (Hegel.clone tc) small_int in
      ()
    done;
    Stdlib.Gc.full_major ());
  Alcotest.(check pass) "dropped clones finalised without error" () ()
;;

let test_clone_shares_draw_names () =
  let names = ref [] in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let n0 = Hegel.Internal.draw_display_name tc ~label:"x" ~repeatable:true in
    let n1 =
      Hegel.Internal.draw_display_name (Hegel.clone tc) ~label:"x" ~repeatable:true
    in
    let n2 = Hegel.Internal.draw_display_name tc ~label:"x" ~repeatable:true in
    names := [ n0; n1; n2 ]);
  Alcotest.(check (list string))
    "clone continues the shared counter"
    [ "x_1"; "x_2"; "x_3" ]
    !names
;;

(* A clone copies the parent's current span depth (rather than resetting to 0),
   so a clone forked inside a span treats its own draws as nested too. *)
let test_clone_copies_draw_depth () =
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    Hegel.Internal.incr_draw_depth tc;
    Hegel.Internal.incr_draw_depth tc;
    let worker = Hegel.clone tc in
    Alcotest.(check int)
      "clone inherits parent span depth"
      2
      (Hegel.Internal.draw_depth worker);
    Hegel.Internal.decr_draw_depth tc;
    Hegel.Internal.decr_draw_depth tc)
;;

let test_spawn_join_returns_value () =
  let both = ref (0, 0) in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let w = spawn tc (fun worker -> Hegel.draw_silent worker small_int) in
    let main = Hegel.draw_silent tc small_int in
    let from_worker = join w in
    both := main, from_worker);
  let main, from_worker = !both in
  Alcotest.(check bool) "main in range" true (main >= 0 && main <= 9);
  Alcotest.(check bool) "worker in range" true (from_worker >= 0 && from_worker <= 9)
;;

let test_spawn_join_reraises () =
  let raised = ref false in
  Hegel.run_hegel_test ~settings:(one_case_settings ()) (fun tc ->
    let w = spawn tc (fun _worker -> failwith "worker boom") in
    (try
       let (_ : int) = join w in
       ()
     with
     | Failure msg when String.equal msg "worker boom" -> raised := true);
    (* Parent handle is still usable after the worker's failure. *)
    let (_ : int) = Hegel.draw_silent tc small_int in
    ());
  Alcotest.(check bool) "join re-raised the worker exception" true !raised
;;

let tests =
  [ Alcotest.test_case "clone draws" `Quick test_clone_draws
  ; Alcotest.test_case "clone concurrent" `Quick test_clone_concurrent
  ; Alcotest.test_case "clone reproducible" `Quick test_clone_reproducible
  ; Alcotest.test_case "clone of clone" `Quick test_clone_of_clone
  ; Alcotest.test_case "clone finalised" `Quick test_clone_finalized
  ; Alcotest.test_case "clone shares draw names" `Quick test_clone_shares_draw_names
  ; Alcotest.test_case "clone copies draw depth" `Quick test_clone_copies_draw_depth
  ; Alcotest.test_case "spawn/join returns value" `Quick test_spawn_join_returns_value
  ; Alcotest.test_case "spawn/join re-raises" `Quick test_spawn_join_reraises
  ]
;;
