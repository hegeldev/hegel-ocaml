(* Upstream OCaml has no [ppx_template]. This rewriter keeps each
   [let%template] and [val%template] item and removes the marker. The compiler
   ignores the [[@mode]] attributes that remain. *)

open Ppxlib

let structure_item =
  Extension.V3.declare
    "template"
    Extension.Context.structure_item
    Ast_pattern.(pstr (__ ^:: nil))
    (fun ~ctxt:_ item -> item)
;;

let signature_item =
  Extension.V3.declare
    "template"
    Extension.Context.signature_item
    Ast_pattern.(psig (__ ^:: nil))
    (fun ~ctxt:_ item -> item)
;;

let () =
  Driver.register_transformation
    "hegel_template"
    ~rules:
      [ Context_free.Rule.extension structure_item
      ; Context_free.Rule.extension signature_item
      ]
;;
