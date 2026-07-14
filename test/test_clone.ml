open! Core
open Hegel
module G = Hegel.Generators

let single_settings () = default_settings () |> with_mode Single_test_case
let small_int = G.integers ~min_value:0 ~max_value:9 ()

(* A clone can be drawn from within the test body, and the parent stays usable
   alongside it. *)
let test_clone_draws () =
  let parent = ref (-1) in
  let cloned = ref (-1) in
  Hegel.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    with_clone tc (fun worker ->
      cloned := Hegel.draw_silent worker small_int;
      parent := Hegel.draw_silent tc small_int));
  Alcotest.(check bool) "parent value in range" true (!parent >= 0 && !parent <= 9);
  Alcotest.(check bool) "clone value in range" true (!cloned >= 0 && !cloned <= 9)
;;

(* Two handles — the parent and its clone — can be driven from two threads at
   once without tripping the engine's concurrent-use guard. *)
let test_clone_concurrent () =
  let worker_value = ref (-1) in
  Hegel.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    with_clone tc (fun worker ->
      let t =
        Caml_threads.Thread.create
          (fun () -> worker_value := Hegel.draw_silent worker small_int)
          ()
      in
      let main = Hegel.draw_silent tc small_int in
      Caml_threads.Thread.join t;
      Alcotest.(check bool) "main value in range" true (main >= 0 && main <= 9)));
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
      ~settings:(single_settings () |> with_seed (Some 42))
      (fun tc ->
         let p = Hegel.draw_silent tc small_int in
         let c = with_clone tc (fun worker -> Hegel.draw_silent worker small_int) in
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
  Hegel.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    with_clone tc (fun c1 ->
      with_clone c1 (fun c2 -> value := Hegel.draw_silent c2 small_int)));
  Alcotest.(check bool) "nested clone value in range" true (!value >= 0 && !value <= 9)
;;

(* An exception raised inside [with_clone]'s body still propagates; the clone is
   released by the [finally] regardless. *)
let test_clone_frees_on_exception () =
  let raised = ref false in
  Hegel.run_hegel_test ~settings:(single_settings ()) (fun tc ->
    (try with_clone tc (fun _worker -> failwith "boom") with
     | Failure _ -> raised := true);
    (* Draw after the aborted clone to show the parent handle is still usable. *)
    let (_ : int) = Hegel.draw_silent tc small_int in
    ());
  Alcotest.(check bool) "with_clone body exception propagated" true !raised
;;

let tests =
  [ Alcotest.test_case "clone draws" `Quick test_clone_draws
  ; Alcotest.test_case "clone concurrent" `Quick test_clone_concurrent
  ; Alcotest.test_case "clone reproducible" `Quick test_clone_reproducible
  ; Alcotest.test_case "clone of clone" `Quick test_clone_of_clone
  ; Alcotest.test_case "clone frees on exception" `Quick test_clone_frees_on_exception
  ]
;;
