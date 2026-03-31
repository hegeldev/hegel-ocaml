(** PPX deriver for [@@deriving generator].

    Reads OCaml type declarations annotated with [@@deriving generator] and
    synthesizes generator functions of type [Hegel.Client.test_case -> t].

    When called inside a Hegel test body, these functions generate a value of
    the declared type by calling [Hegel.draw] with appropriate generators and
    returning typed OCaml values directly.

    Supported types:
    - Records: generates all fields, then constructs the record
    - Variants: picks a constructor uniformly at random, then generates args
    - Type aliases: delegates to the generator for the aliased type

    Supported field/argument types:
    - [int] -> generates via [integers()]
    - [bool] -> generates via [booleans()]
    - [float] -> generates via
      [floats ~allow_nan:false ~allow_infinity:false ()]
    - [string] -> generates via [text()]
    - [t list] -> generates a list length, then generates each element
    - [t option] -> generates [Some v] or [None]
    - Named type [t] -> calls [t_generator _hegel_tc] (assumes it exists in
      scope) *)

open Ppxlib

(** [generate_expr_of_core_type ct] returns an expression of type
    [Hegel.Client.test_case -> <t>] that generates a value of core type [ct]. *)
let rec generate_expr_of_core_type (ct : core_type) : expression =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
      [%expr
        fun _hegel_tc ->
          Hegel.draw _hegel_tc
            (Hegel.Generators.integers ~min_value:Int.min_int
               ~max_value:Int.max_int ())]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
      [%expr
        fun _hegel_tc -> Hegel.draw _hegel_tc (Hegel.Generators.booleans ())]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
      [%expr
        fun _hegel_tc ->
          Hegel.draw _hegel_tc
            (Hegel.Generators.floats ~allow_nan:false ~allow_infinity:false ())]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
      [%expr fun _hegel_tc -> Hegel.draw _hegel_tc (Hegel.Generators.text ())]
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ elem_type ]) ->
      let elem_gen_fn = generate_expr_of_core_type elem_type in
      [%expr
        fun _hegel_tc -> Hegel.Derive.generate_list _hegel_tc [%e elem_gen_fn]]
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ inner_type ]) ->
      let inner_gen_fn = generate_expr_of_core_type inner_type in
      [%expr
        fun _hegel_tc ->
          Hegel.Derive.generate_option _hegel_tc [%e inner_gen_fn]]
  | Ptyp_constr ({ txt = Lident tname; _ }, []) ->
      let gen_fn =
        Ast_builder.Default.pexp_ident ~loc
          { txt = Lident (tname ^ "_generator"); loc }
      in
      [%expr fun _hegel_tc -> [%e gen_fn] _hegel_tc]
  | Ptyp_constr ({ txt = Ldot (modpath, tname); _ }, []) ->
      let rec longident_to_parts = function
        | Longident.Lident s -> [ s ]
        | Longident.Ldot (prefix, s) -> longident_to_parts prefix @ [ s ]
        | Longident.Lapply _ ->
            Location.raise_errorf ~loc
              "ppx_hegel_generator: functor application types not supported"
      in
      let parts = longident_to_parts modpath in
      let full_lid =
        List.fold_left
          (fun acc p -> Longident.Ldot (acc, p))
          (Lident (List.hd parts))
          (List.tl parts @ [ tname ^ "_generator" ])
      in
      let gen_fn =
        Ast_builder.Default.pexp_ident ~loc { txt = full_lid; loc }
      in
      [%expr fun _hegel_tc -> [%e gen_fn] _hegel_tc]
  | Ptyp_tuple components -> generate_expr_of_tuple ~loc components
  | _ ->
      Location.raise_errorf ~loc
        "ppx_hegel_generator: unsupported type in [@@deriving generator]"

(** Generate a [test_case -> (t1 * t2 * ...)] function for a tuple type. *)
and generate_expr_of_tuple ~loc components =
  let gen_fns =
    List.mapi
      (fun i ct ->
        let gen_fn = generate_expr_of_core_type ct in
        let vname = Printf.sprintf "_tup_gen_%d" i in
        (vname, gen_fn))
      components
  in
  let tuple_expr =
    Ast_builder.Default.pexp_tuple ~loc
      (List.map
         (fun (vname, _) ->
           [%expr
             [%e
               Ast_builder.Default.pexp_ident ~loc { txt = Lident vname; loc }]
               _hegel_tc])
         gen_fns)
  in
  let body =
    List.fold_right
      (fun (vname, gen_fn) acc ->
        [%expr
          let [%p Ast_builder.Default.pvar ~loc vname] = [%e gen_fn] in
          [%e acc]])
      gen_fns tuple_expr
  in
  [%expr fun _hegel_tc -> [%e body]]

(** Generate code for a record type. Produces:
    [fun _hegel_tc -> { field1 = gen1 _hegel_tc; field2 = gen2 _hegel_tc; ... }]
*)
let generator_of_record ~loc (labels : label_declaration list) : expression =
  if labels = [] then
    Location.raise_errorf ~loc
      "ppx_hegel_generator: empty record types not supported";
  let field_gen_fns =
    List.map
      (fun (ld : label_declaration) ->
        (ld.pld_name.txt, generate_expr_of_core_type ld.pld_type))
      labels
  in
  let record_expr =
    Ast_builder.Default.pexp_record ~loc
      (List.map
         (fun (name, _) ->
           let gen_var = "_gen_" ^ name in
           ( { txt = Lident name; loc },
             [%expr
               [%e
                 Ast_builder.Default.pexp_ident ~loc
                   { txt = Lident gen_var; loc }]
                 _hegel_tc] ))
         field_gen_fns)
      None
  in
  let body =
    List.fold_right
      (fun (name, gen_fn) acc ->
        let gen_var = "_gen_" ^ name in
        [%expr
          let [%p Ast_builder.Default.pvar ~loc gen_var] = [%e gen_fn] in
          [%e acc]])
      field_gen_fns record_expr
  in
  [%expr fun _hegel_tc -> [%e body]]

(** Generate code for a variant type. Produces a [fun _hegel_tc -> ...] that
    picks a constructor index via sampled_from, then generates the appropriate
    arguments. *)
let generator_of_variant ~loc (constrs : constructor_declaration list) :
    expression =
  let n = List.length constrs in
  if n = 0 then
    Location.raise_errorf ~loc
      "ppx_hegel_generator: empty variant types not supported";
  let index_options = List.init n (fun i -> Ast_builder.Default.eint ~loc i) in
  let match_arms =
    List.mapi
      (fun i (cd : constructor_declaration) ->
        let constr_name = cd.pcd_name.txt in
        let constr_lid = { txt = Lident constr_name; loc } in
        match cd.pcd_args with
        | Pcstr_tuple [] ->
            Ast_builder.Default.case
              ~lhs:(Ast_builder.Default.pint ~loc i)
              ~guard:None
              ~rhs:(Ast_builder.Default.pexp_construct ~loc constr_lid None)
        | Pcstr_tuple [ ct ] ->
            let gen_fn = generate_expr_of_core_type ct in
            let body =
              Ast_builder.Default.pexp_construct ~loc constr_lid
                (Some [%expr [%e gen_fn] _hegel_tc])
            in
            Ast_builder.Default.case
              ~lhs:(Ast_builder.Default.pint ~loc i)
              ~guard:None ~rhs:body
        | Pcstr_tuple cts ->
            let gen_fns =
              List.mapi
                (fun j ct ->
                  let gen_fn = generate_expr_of_core_type ct in
                  let vname = Printf.sprintf "_arg_%d" j in
                  (vname, gen_fn))
                cts
            in
            let tuple_expr =
              Ast_builder.Default.pexp_tuple ~loc
                (List.map
                   (fun (vname, _) ->
                     [%expr
                       [%e
                         Ast_builder.Default.pexp_ident ~loc
                           { txt = Lident vname; loc }]
                         _hegel_tc])
                   gen_fns)
            in
            let inner_body =
              List.fold_right
                (fun (vname, gen_fn) acc ->
                  [%expr
                    let [%p Ast_builder.Default.pvar ~loc vname] =
                      [%e gen_fn]
                    in
                    [%e acc]])
                gen_fns
                (Ast_builder.Default.pexp_construct ~loc constr_lid
                   (Some tuple_expr))
            in
            Ast_builder.Default.case
              ~lhs:(Ast_builder.Default.pint ~loc i)
              ~guard:None ~rhs:inner_body
        | Pcstr_record labels ->
            let record_gen = generator_of_record ~loc labels in
            let body =
              Ast_builder.Default.pexp_construct ~loc constr_lid
                (Some [%expr [%e record_gen] _hegel_tc])
            in
            Ast_builder.Default.case
              ~lhs:(Ast_builder.Default.pint ~loc i)
              ~guard:None ~rhs:body)
      constrs
  in
  let catch_all =
    Ast_builder.Default.case
      ~lhs:[%pat? _]
      ~guard:None
      ~rhs:[%expr failwith "ppx_hegel_generator: unreachable variant index"]
  in
  let all_arms = match_arms @ [ catch_all ] in
  let match_expr =
    Ast_builder.Default.pexp_match ~loc [%expr _variant_idx] all_arms
  in
  [%expr
    fun _hegel_tc ->
      let _variant_idx =
        Hegel.draw _hegel_tc
          (Hegel.Generators.sampled_from
             [%e Ast_builder.Default.elist ~loc index_options])
      in
      [%e match_expr]]

(** Generate code for a type alias. *)
let generator_of_alias ~loc:_ (ct : core_type) : expression =
  generate_expr_of_core_type ct

(** The main deriver. *)
let generate_impl ~ctxt
    ((_rec_flag, type_decls) : rec_flag * type_declaration list) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun (td : type_declaration) ->
      let name = td.ptype_name.txt in
      let gen_name = name ^ "_generator" in
      let gen_expr =
        match (td.ptype_kind, td.ptype_manifest) with
        | Ptype_record labels, _ -> generator_of_record ~loc labels
        | Ptype_variant constrs, _ -> generator_of_variant ~loc constrs
        | Ptype_abstract, Some ct -> generator_of_alias ~loc ct
        | Ptype_abstract, None ->
            Location.raise_errorf ~loc
              "ppx_hegel_generator: abstract types without manifest not \
               supported"
        | Ptype_open, _ ->
            Location.raise_errorf ~loc
              "ppx_hegel_generator: open types not supported"
      in
      [%stri let [%p Ast_builder.Default.pvar ~loc gen_name] = [%e gen_expr]])
    type_decls

let _deriver =
  Deriving.add "generator"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)
