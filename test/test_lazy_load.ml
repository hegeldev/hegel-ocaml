(** libhegel must not be loaded until the first [Ffi.context_new]. This lives in
    its own executable because [test_hegel.exe] drives the engine, so the library
    is already loaded by the time any test in it could check. *)

open! Core
module Unix = Core_unix

let missing_library = "/nonexistent/libhegel.so"

let expect_load_failure () =
  match Hegel_ffi.Ffi.context_new () with
  | (_ : Hegel_ffi.Ffi.context) -> Alcotest.fail "context_new succeeded without libhegel"
  | exception Failure msg ->
    Alcotest.(check bool)
      "context_new fails on the missing library"
      true
      (String.is_prefix
         msg
         ~prefix:("hegel: failed to load libhegel from " ^ missing_library))
;;

let test_generators_do_not_load_libhegel () =
  (* Point the loader at a file that does not exist, and stop it falling back to
     a download, so any attempt to load the library fails loudly. *)
  Unix.putenv ~key:"HEGEL_LIBHEGEL_PATH" ~data:missing_library;
  Unix.putenv ~key:"HEGEL_LIBHEGEL_NO_DOWNLOAD" ~data:"1";
  (* Building generators is pure OCaml. *)
  let (_ : (int list, Hegel.printable) Hegel.generator) =
    Hegel.lists (Hegel.integers ()) ()
  in
  (* The engine is only reached on the first context. *)
  expect_load_failure ();
  (* The failure is memoized: a second call re-raises rather than retrying. *)
  expect_load_failure ()
;;

let () =
  Alcotest.run
    "lazy_load"
    [ ( "lazy_load"
      , [ Alcotest.test_case
            "generators do not load libhegel"
            `Quick
            test_generators_do_not_load_libhegel
        ] )
    ]
;;
