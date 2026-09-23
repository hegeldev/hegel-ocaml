open! Core
open Hegel

module Plain = struct
  type leaf = { a : int } [@@deriving hegel_generator]

  type record =
    { xs : int list
    ; o : string option
    ; l : leaf
    }
  [@@deriving hegel_generator]

  type enum =
    | E1
    | E2
  [@@deriving hegel_generator]

  type variant =
    | V1 of float
    | V2 of int * char
    | V3 of { w : bool }
  [@@deriving hegel_generator]

  type alias = int * bool [@@deriving hegel_generator]
end

module Portable = struct
  type leaf = { a : int } [@@deriving hegel_generator ~portable]

  type record =
    { xs : int list
    ; o : string option
    ; l : leaf
    }
  [@@deriving hegel_generator ~portable]

  type enum =
    | E1
    | E2
  [@@deriving hegel_generator ~portable]

  type variant =
    | V1 of float
    | V2 of int * char
    | V3 of { w : bool }
  [@@deriving hegel_generator ~portable]

  type alias = int * bool [@@deriving hegel_generator ~portable]
end

(** Test: [~portable] draws the same values as the plain derivation. *)
let test_portable_matches_plain_e2e () =
  let collect gen =
    let seen = ref [] in
    Hegel.run_hegel_test
      ~settings:(Hegel.Settings.create ~test_cases:30 ~seed:0 ())
      (fun tc ->
         seen
         := Sexp.to_string ((Hegel.Generators.printer gen) (Hegel.draw_silent tc gen))
            :: !seen);
    List.rev !seen
  in
  let same name plain portable =
    Alcotest.(check (list string)) name (collect plain) (collect portable)
  in
  same "record" Plain.hegel_generator_record Portable.hegel_generator_record;
  same "enum" Plain.hegel_generator_enum Portable.hegel_generator_enum;
  same "variant" Plain.hegel_generator_variant Portable.hegel_generator_variant;
  same "alias" Plain.hegel_generator_alias Portable.hegel_generator_alias
;;

let tests =
  [ Alcotest.test_case "portable matches plain" `Quick test_portable_matches_plain_e2e ]
;;
