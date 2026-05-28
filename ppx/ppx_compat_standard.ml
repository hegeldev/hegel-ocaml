open Ppxlib

let extract_tuple_types ct =
  match ct.ptyp_desc with
  | Ptyp_tuple cts -> Some cts
  | _ -> None
;;

let extract_constr_tuple_types = function
  | Pcstr_tuple cts -> Some cts
  | Pcstr_record _ -> None
;;

let extract_expr_tuple e =
  match e.pexp_desc with
  | Pexp_tuple es -> Some es
  | _ -> None
;;
