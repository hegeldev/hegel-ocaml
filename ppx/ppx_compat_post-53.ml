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

let unwrap_pattern_constraint pat =
  match pat.ppat_desc with
  | Ppat_constraint (inner, _) -> Some inner
  | _ -> None
;;

let expr_first_param_pat e =
  match e.pexp_desc with
  | Pexp_function (params, _, _) ->
    List.find_map
      (fun param ->
         match param.pparam_desc with
         | Pparam_val (_, _, pat) -> Some pat
         | Pparam_newtype _ -> None)
      params
  | _ -> None
;;

let rec peel_fun_params e =
  match e.pexp_desc with
  | Pexp_function (_, _, Pfunction_body body) -> peel_fun_params body
  | _ -> e
;;

let is_function_expr e =
  match e.pexp_desc with
  | Pexp_function _ -> true
  | _ -> false
;;

let extract_let_bindings e =
  match e.pexp_desc with
  | Pexp_let (_, vbs, _) -> Some vbs
  | _ -> None
;;

let map_let_value_bindings f e =
  match e.pexp_desc with
  | Pexp_let (rec_flag, vbs, body) ->
    { e with pexp_desc = Pexp_let (rec_flag, f vbs, body) }
  | _ -> e
;;
