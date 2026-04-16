(** Platform compatibility shim for ppxlib types.

    On OxCaml, ppxlib's AST types differ from standard OCaml (labeled tuples,
    constructor modalities, etc.). This module abstracts over those differences
    so the PPX deriver can work on both platforms. *)

open Ppxlib

val extract_tuple_types : core_type -> core_type list option
(** [extract_tuple_types ct] returns [Some types] if [ct] is a tuple type,
    [None] otherwise. On OxCaml, strips the optional labels from labeled tuples.
*)

val extract_constr_tuple_types : constructor_arguments -> core_type list option
(** [extract_constr_tuple_types args] returns [Some types] if [args] is
    [Pcstr_tuple], [None] if [Pcstr_record]. On OxCaml, extracts the core types
    from [constructor_argument] records. *)
