(* The dune action sets an invalid library path before process startup. Linking
   Hegel and constructing generators must succeed; only running a test should
   try to load the engine. *)
open Hegel

module Common = struct
  type t =
    { name : string
    ; scores : int list
    ; enabled : bool option
    }
  [@@deriving hegel_generator]
end

type event =
  | Empty
  | Updated of Common.t
[@@deriving hegel_generator]

let events = lists (optional hegel_generator_event) ()
let strings = one_of [ text (); from_regex "[a-z]+" (); emails (); urls (); domains () ]
let test_settings = settings ~test_cases:1 ()

let expect_load_failure () =
  match run_hegel_test ~settings:test_settings (fun _tc -> ()) with
  | () -> failwith "expected the first engine call to fail loading libhegel"
  | exception Failure msg ->
    assert (
      String.starts_with
        ~prefix:"hegel: failed to load libhegel from /hegel-test-missing/libhegel:"
        msg)
;;

let () =
  ignore (tuples2 events strings);
  (* Concurrent first use must report the loading error to every caller,
     without racing on the lazy initializer and raising [Lazy.Undefined]. *)
  let ready = Atomic.make 0 in
  let workers =
    List.init 4 (fun _ ->
      Domain.spawn (fun () ->
        Atomic.incr ready;
        while Atomic.get ready < 4 do
          Domain.cpu_relax ()
        done;
        expect_load_failure ()))
  in
  List.iter Domain.join workers;
  expect_load_failure ()
;;
