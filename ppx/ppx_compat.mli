(** Platform compatibility shim for ppxlib types.

    On OxCaml, ppxlib's AST types differ from standard OCaml (labeled tuples,
    constructor modalities, etc.). This module abstracts over those differences
    so the PPX deriver can work on both platforms. *)

open Ppxlib

(** [extract_tuple_types ct] returns [Some types] if [ct] is a tuple type,
    [None] otherwise. On OxCaml, strips the optional labels from labeled tuples.
*)
val extract_tuple_types : core_type -> core_type list option

(** [extract_constr_tuple_types args] returns [Some types] if [args] is
    [Pcstr_tuple], [None] if [Pcstr_record]. On OxCaml, extracts the core types
    from [constructor_argument] records. *)
val extract_constr_tuple_types : constructor_arguments -> core_type list option

(** [extract_expr_tuple e] returns [Some es] if [e] is a tuple expression,
    [None] otherwise. On OxCaml, strips the optional labels from labeled
    tuples. *)
val extract_expr_tuple : expression -> expression list option

(** [unwrap_pattern_constraint pat] returns [Some inner] if [pat] is a
    type-constraint pattern ([(p : ty)]), [None] otherwise. On OxCaml,
    [Ppat_constraint] carries an extra modes argument. *)
val unwrap_pattern_constraint : pattern -> pattern option

(** [expr_first_param_pat e] returns [Some pat] for the pattern of the first
    value parameter of a function expression [e] ([fun pat … -> …]), or [None]
    if [e] is not a function or has no value parameter. Abstracts over the
    OxCaml [Pexp_function]/standard [Pexp_fun] split. *)
val expr_first_param_pat : expression -> pattern option

(** [peel_fun_params e] strips the leading value-parameter lambdas of [e] and
    returns the function body, or [e] unchanged when it is not such a function.
*)
val peel_fun_params : expression -> expression

(** [is_function_expr e] is [true] when [e] is a function literal ([fun … -> …]
    or [function …]). *)
val is_function_expr : expression -> bool

(** [extract_let_bindings e] returns [Some vbs] if [e] is a [let … in …]
    expression, [None] otherwise. On OxCaml, [Pexp_let] carries an extra
    [mutable_flag]. *)
val extract_let_bindings : expression -> value_binding list option

(** [map_let_value_bindings f e] rewrites the bindings of a [let … in …]
    expression [e] with [f], preserving every other field (recursion flag, body,
    and the OxCaml-only [mutable_flag]); returns [e] unchanged if it is not a
    [let]. *)
val map_let_value_bindings
  :  (value_binding list -> value_binding list)
  -> expression
  -> expression
