open Ppxlib

(* ================================================================ *)
(* AST helper shortcuts                                             *)
(* ================================================================ *)

let loc = Location.none

let evar name = Ast_builder.Default.evar ~loc name
let pvar name = Ast_builder.Default.pvar ~loc name

let eapply f args =
  List.fold_left
    (fun acc arg -> [%expr [%e acc] [%e arg]])
    f args

let gen_name_for type_name = "gen_" ^ type_name

let gen_name_for_var tv = "gen_" ^ tv

(* ================================================================ *)
(* Type mapping: OCaml core type -> Gen.t expression                *)
(* ================================================================ *)

let rec gen_for_core_type ct =
  match ct.ptyp_desc with
  | Ptyp_var tv ->
    evar (gen_name_for_var tv)

  | Ptyp_constr ({ txt = lident; _ }, args) ->
    let name = Longident.name lident in
    begin match name, args with
    | "int", [] -> [%expr Hegel.Gen.int ()]
    | "float", [] -> [%expr Hegel.Gen.float ()]
    | "bool", [] -> [%expr Hegel.Gen.bool ()]
    | "string", [] -> [%expr Hegel.Gen.string ()]
    | "unit", [] -> [%expr Hegel.Gen.unit ()]
    | "int32", [] -> [%expr Hegel.Gen.int32 ()]
    | "int64", [] -> [%expr Hegel.Gen.int64 ()]
    | "list", [a] -> [%expr Hegel.Gen.list [%e gen_for_core_type a]]
    | "array", [a] -> [%expr Hegel.Gen.array [%e gen_for_core_type a]]
    | "option", [a] -> [%expr Hegel.Gen.optional [%e gen_for_core_type a]]
    | _ ->
      let base = evar (gen_name_for name) in
      let gen_args = List.map gen_for_core_type args in
      if gen_args = [] then base
      else eapply base gen_args
    end

  | Ptyp_tuple [a; b] ->
    [%expr Hegel.Gen.pair [%e gen_for_core_type a] [%e gen_for_core_type b]]

  | Ptyp_tuple [a; b; c] ->
    [%expr Hegel.Gen.triple
      [%e gen_for_core_type a]
      [%e gen_for_core_type b]
      [%e gen_for_core_type c]]

  | Ptyp_tuple (a :: rest) ->
    let rest_type = { ct with ptyp_desc = Ptyp_tuple rest } in
    [%expr Hegel.Gen.pair [%e gen_for_core_type a] [%e gen_for_core_type rest_type]]

  | _ ->
    Location.raise_errorf ~loc:ct.ptyp_loc
      "ppx_hegel: unsupported type"

(* ================================================================ *)
(* Record generation                                                *)
(* ================================================================ *)

let gen_for_record fields =
  let field_names = List.map (fun ld -> ld.pld_name.txt) fields in
  let field_gens =
    List.map (fun ld -> (ld.pld_name.txt, gen_for_core_type ld.pld_type)) fields
  in

  let bind_field_gens body =
    List.fold_right
      (fun (fname, gen_expr) acc ->
        [%expr let [%p pvar ("__gen_" ^ fname)] = [%e gen_expr] in [%e acc]])
      field_gens body
  in

  let build_record var_prefix =
    let fields =
      List.map
        (fun fname ->
          ({ txt = Lident fname; loc }, evar (var_prefix ^ fname)))
        field_names
    in
    Ast_builder.Default.pexp_record ~loc fields None
  in

  let as_basic_expr =
    let basic_names = List.map (fun f -> "__basic_" ^ f) field_names in

    let schema_list =
      List.map
        (fun f -> [%expr [%e evar ("__basic_" ^ f)].Hegel.Gen.schema])
        field_names
    in
    let schema_array = Ast_builder.Default.elist ~loc schema_list in

    let parse_body =
      let arr_var = evar "__arr" in
      let let_bindings =
        List.mapi
          (fun i fname ->
            let parse_expr =
              [%expr [%e evar ("__basic_" ^ fname)].Hegel.Gen.parse
                (List.nth [%e arr_var] [%e Ast_builder.Default.eint ~loc i])]
            in
            (fname, parse_expr))
          field_names
      in
      List.fold_right
        (fun (fname, expr) acc ->
          [%expr let [%p pvar ("__v_" ^ fname)] = [%e expr] in [%e acc]])
        let_bindings
        (build_record "__v_")
    in

    let some_result =
      [%expr
        Some {
          Hegel.Gen.schema = Hegel.Cbor.Map [
            (Hegel.Cbor.Text "type", Hegel.Cbor.Text "tuple");
            (Hegel.Cbor.Text "elements", Hegel.Cbor.Array [%e schema_array]);
          ];
          Hegel.Gen.parse = (fun __raw ->
            let __arr = Hegel.Gen.cbor_array_value __raw in
            [%e parse_body]);
        }]
    in

    List.fold_right2
      (fun fname bname acc ->
        [%expr
          match [%e evar ("__gen_" ^ fname)].Hegel.Gen.as_basic () with
          | Some [%p pvar bname] -> [%e acc]
          | None -> None])
      field_names basic_names some_result
  in

  let generate_fallback =
    let field_bindings =
      List.map
        (fun fname ->
          (fname, [%expr [%e evar ("__gen_" ^ fname)].Hegel.Gen.generate ()]))
        field_names
    in
    List.fold_right
      (fun (fname, expr) acc ->
        [%expr let [%p pvar ("__v_" ^ fname)] = [%e expr] in [%e acc]])
      field_bindings
      (build_record "__v_")
  in

  let generate_body =
    [%expr
      match (fun () -> [%e as_basic_expr]) () with
      | Some __basic ->
        __basic.Hegel.Gen.parse (Hegel.Gen.generate_raw __basic.Hegel.Gen.schema)
      | None ->
        Hegel.Gen.group Hegel.Gen.Labels.fixed_dict (fun () -> [%e generate_fallback])]
  in

  bind_field_gens
    [%expr {
      Hegel.Gen.generate = (fun () -> [%e generate_body]);
      Hegel.Gen.as_basic = (fun () -> [%e as_basic_expr]);
    }]

(* ================================================================ *)
(* Variant generation                                               *)
(* ================================================================ *)

let gen_for_variant constructors =
  let case_gens =
    List.map
      (fun cd ->
        let cname = cd.pcd_name.txt in
        let ctor = { txt = Lident cname; loc } in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
          let value = Ast_builder.Default.pexp_construct ~loc ctor None in
          [%expr Hegel.Gen.just [%e value]]

        | Pcstr_tuple [t] ->
          let inner_gen = gen_for_core_type t in
          let mapper =
            [%expr fun __x ->
              [%e Ast_builder.Default.pexp_construct ~loc ctor (Some (evar "__x"))]]
          in
          [%expr Hegel.Gen.map [%e mapper] [%e inner_gen]]

        | Pcstr_tuple ts ->
          let n = List.length ts in
          let tuple_gen =
            match ts with
            | [a; b] ->
              [%expr Hegel.Gen.pair
                [%e gen_for_core_type a] [%e gen_for_core_type b]]
            | [a; b; c] ->
              [%expr Hegel.Gen.triple
                [%e gen_for_core_type a]
                [%e gen_for_core_type b]
                [%e gen_for_core_type c]]
            | _ ->
              let rec nest = function
                | [t] -> gen_for_core_type t
                | t :: rest ->
                  [%expr Hegel.Gen.pair [%e gen_for_core_type t] [%e nest rest]]
                | [] -> assert false
              in
              nest ts
          in
          let vars = List.mapi (fun i _ -> "__a" ^ string_of_int i) ts in
          let tuple_pat =
            match n with
            | 2 -> [%pat? ([%p pvar "__a0"], [%p pvar "__a1"])]
            | 3 -> [%pat? ([%p pvar "__a0"], [%p pvar "__a1"], [%p pvar "__a2"])]
            | _ ->
              let rec nest_pat = function
                | [v] -> pvar v
                | v :: rest ->
                  Ast_builder.Default.ppat_tuple ~loc [pvar v; nest_pat rest]
                | [] -> assert false
              in
              nest_pat vars
          in
          let ctor_args =
            Ast_builder.Default.pexp_tuple ~loc (List.map evar vars)
          in
          let ctor_expr =
            Ast_builder.Default.pexp_construct ~loc ctor (Some ctor_args)
          in
          let mapper =
            Ast_builder.Default.pexp_fun ~loc Nolabel None tuple_pat ctor_expr
          in
          [%expr Hegel.Gen.map [%e mapper] [%e tuple_gen]]

        | Pcstr_record fields ->
          let field_names = List.map (fun ld -> ld.pld_name.txt) fields in
          let field_types = List.map (fun ld -> ld.pld_type) fields in
          let n = List.length fields in
          let tuple_gen =
            match field_types with
            | [a] -> gen_for_core_type a
            | [a; b] ->
              [%expr Hegel.Gen.pair
                [%e gen_for_core_type a] [%e gen_for_core_type b]]
            | [a; b; c] ->
              [%expr Hegel.Gen.triple
                [%e gen_for_core_type a]
                [%e gen_for_core_type b]
                [%e gen_for_core_type c]]
            | _ ->
              let rec nest = function
                | [t] -> gen_for_core_type t
                | t :: rest ->
                  [%expr Hegel.Gen.pair [%e gen_for_core_type t] [%e nest rest]]
                | [] -> assert false
              in
              nest field_types
          in
          let vars = List.mapi (fun i _ -> "__a" ^ string_of_int i) fields in
          let tuple_pat =
            match n with
            | 1 -> pvar "__a0"
            | 2 -> [%pat? ([%p pvar "__a0"], [%p pvar "__a1"])]
            | 3 -> [%pat? ([%p pvar "__a0"], [%p pvar "__a1"], [%p pvar "__a2"])]
            | _ ->
              let rec nest_pat = function
                | [v] -> pvar v
                | v :: rest ->
                  Ast_builder.Default.ppat_tuple ~loc [pvar v; nest_pat rest]
                | [] -> assert false
              in
              nest_pat vars
          in
          let record_fields =
            List.map2
              (fun fname vname ->
                ({ txt = Lident fname; loc }, evar vname))
              field_names vars
          in
          let record_expr =
            Ast_builder.Default.pexp_record ~loc record_fields None
          in
          let ctor_expr =
            Ast_builder.Default.pexp_construct ~loc ctor (Some record_expr)
          in
          let mapper =
            Ast_builder.Default.pexp_fun ~loc Nolabel None tuple_pat ctor_expr
          in
          [%expr Hegel.Gen.map [%e mapper] [%e tuple_gen]])
    constructors
  in
  let gen_list = Ast_builder.Default.elist ~loc case_gens in
  [%expr Hegel.Gen.one_of [%e gen_list]]

(* ================================================================ *)
(* Build generator body for a type declaration                      *)
(* ================================================================ *)

(* Check whether a type declaration actually refers to itself *)
let type_is_self_referencing td =
  let name = td.ptype_name.txt in
  let found = ref false in
  let iterator = object
    inherit Ast_traverse.iter as super
    method! core_type ct =
      (match ct.ptyp_desc with
       | Ptyp_constr ({ txt = Lident n; _ }, _) when n = name ->
         found := true
       | _ -> ());
      super#core_type ct
  end in
  iterator#type_declaration td;
  !found

let body_for_type_decl td =
  match td.ptype_kind, td.ptype_manifest with
  | Ptype_record fields, _ -> gen_for_record fields
  | Ptype_variant constructors, _ -> gen_for_variant constructors
  | Ptype_abstract, Some ct -> gen_for_core_type ct
  | _ ->
    Location.raise_errorf ~loc:td.ptype_loc
      "ppx_hegel: unsupported type kind"

let wrap_with_params params body =
  List.fold_right
    (fun (ct, _) acc ->
      match ct.ptyp_desc with
      | Ptyp_var tv ->
        Ast_builder.Default.pexp_fun ~loc Nolabel None
          (pvar (gen_name_for_var tv))
          acc
      | _ ->
        Location.raise_errorf ~loc:ct.ptyp_loc
          "ppx_hegel: expected type variable parameter")
    params body

(* ================================================================ *)
(* Structure generator (for .ml files)                              *)
(* ================================================================ *)

let str_type_decl ~ctxt (rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  ignore loc;
  let is_rec = match rec_flag with Recursive -> true | Nonrecursive -> false in
  List.map
    (fun td ->
      let type_name = td.ptype_name.txt in
      let fname = gen_name_for type_name in
      let params = td.ptype_params in
      let body = body_for_type_decl td in
      if is_rec && params = [] && type_is_self_referencing td then begin
        (* Recursive non-parametric type: use ref + wrapper.
           The wrapper's as_basic always returns None to prevent infinite
           recursion when one_of tries to check all branches' as_basic. *)
        let ref_name = "__ref_" ^ fname in
        [%stri
          let [%p pvar fname] =
            let [%p pvar ref_name] = ref {
              Hegel.Gen.generate = (fun () -> assert false);
              Hegel.Gen.as_basic = (fun () -> None);
            } in
            let [%p pvar fname] = {
              Hegel.Gen.generate = (fun () -> (! [%e evar ref_name]).Hegel.Gen.generate ());
              Hegel.Gen.as_basic = (fun () -> None);
            } in
            ignore [%e evar fname];
            let __result = [%e body] in
            [%e evar ref_name] := __result;
            __result]
      end
      else
        let expr = wrap_with_params params body in
        [%stri let [%p pvar fname] = [%e expr]])
    tds

(* ================================================================ *)
(* Signature generator (for .mli files)                             *)
(* ================================================================ *)

let sig_type_decl ~ctxt (_rec_flag, tds) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun td ->
      let type_name = td.ptype_name.txt in
      let fname = gen_name_for type_name in
      let params = td.ptype_params in

      let result_type =
        let type_args = List.map fst params in
        let base =
          Ast_builder.Default.ptyp_constr ~loc
            { txt = Lident type_name; loc }
            type_args
        in
        Ast_builder.Default.ptyp_constr ~loc
          { txt = Ldot (Ldot (Lident "Hegel", "Gen"), "t"); loc }
          [base]
      in

      let full_type =
        List.fold_right
          (fun (ct, _) acc ->
            let param_gen_type =
              Ast_builder.Default.ptyp_constr ~loc
                { txt = Ldot (Ldot (Lident "Hegel", "Gen"), "t"); loc }
                [ct]
            in
            Ast_builder.Default.ptyp_arrow ~loc Nolabel param_gen_type acc)
          params result_type
      in

      Ast_builder.Default.psig_value ~loc
        (Ast_builder.Default.value_description ~loc
           ~name:{ txt = fname; loc }
           ~type_:full_type
           ~prim:[]))
    tds

(* ================================================================ *)
(* Register the deriver                                             *)
(* ================================================================ *)

let () =
  let str_type_decl = Deriving.Generator.V2.make_noarg str_type_decl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg sig_type_decl in
  Deriving.add "hegel"
    ~str_type_decl
    ~sig_type_decl
  |> Deriving.ignore
