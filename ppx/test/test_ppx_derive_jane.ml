(** Tests for [@@deriving hegel_generator] with the Core generators from
    [Hegel_jane.Export]. *)

open! Core
open Hegel_jane.Export

(** A record mixing primitives (resolved to the jane-shadowed leaves) with
    Core-typed fields (resolved through the wrapper modules). *)
type event =
  { id : int
  ; initial : char
  ; day : Date.t
  ; elapsed : Time_ns.Span.t
  }
[@@deriving hegel_generator]

(** Test: the [event] record derives and generates Core-typed values with the
    full Core API available on them. *)
let%hegel_test test_event_e2e tc =
  let e = Hegel.draw_silent tc hegel_generator_event in
  ignore (e.id : int);
  assert (Char.to_int e.initial >= 0 && Char.to_int e.initial <= 255);
  assert (Date.year e.day >= 1 && Date.year e.day <= 9999);
  ignore (Time_ns.Span.to_string e.elapsed : string)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

(** Test: the wrapper-module generators are ordinary values, drawable
    directly. *)
let%hegel_test test_wrapper_module_generators_e2e tc =
  let d = Hegel.draw_silent tc Date.hegel_generator in
  let span = Hegel.draw_silent tc Time_ns.Span.hegel_generator in
  assert (Date.year d >= 1 && Date.year d <= 9999);
  ignore (Time_ns.Span.to_int63_ns span : Int63.t)
[@@settings Hegel.settings ~test_cases:20 ()]
;;

let () =
  Alcotest.run
    "hegel-ppx-derive-jane"
    [ ( "ppx_derive_jane"
      , [ Alcotest.test_case "derived Core-typed record" `Quick test_event_e2e
        ; Alcotest.test_case
            "wrapper module generators"
            `Quick
            test_wrapper_module_generators_e2e
        ] )
    ]
;;
