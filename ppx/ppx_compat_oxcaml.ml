open Ppxlib

let extract_tuple_types ct =
  match ct.ptyp_desc with
  | Ptyp_tuple labeled_cts -> Some (List.map snd labeled_cts)
  | _ -> None

let extract_constr_tuple_types = function
  | Pcstr_tuple args -> Some (List.map (fun arg -> arg.pca_type) args)
  | Pcstr_record _ -> None
