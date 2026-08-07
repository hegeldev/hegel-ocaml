(** PPX deriver for [@@deriving hegel_generator].

    Reads OCaml type declarations annotated with [@@deriving hegel_generator]
    and synthesizes a generator value. The type [t] derives [hegel_generator]
    and any other type [foo] derives [hegel_generator_foo].

    Generated code resolves every type constructor by name with the same
    convention. [int] becomes [hegel_generator_int], [M.t] becomes
    [M.hegel_generator], and a parameterized type applies the arguments
    ([int list] becomes [hegel_generator_list hegel_generator_int]).

    Supported type declarations:
    - Records: generates all fields, then constructs the record
    - Variants: picks a constructor via [sampled_from] then generates args
    - Type aliases: delegates to the generator for the aliased type
    - Tuples inside any of the above *)

open Ppxlib

let generator_name = function
  | "t" -> "hegel_generator"
  | name -> "hegel_generator_" ^ name
;;

let mangle_lid ~loc = function
  | Lident name -> Lident (generator_name name)
  | Ldot (path, name) -> Ldot (path, generator_name name)
  | Lapply _ ->
    Location.raise_errorf
      ~loc
      "ppx_hegel_generator: functor application types not supported"
;;

let generator_override_attribute =
  Attribute.declare
    "hegel.generator"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    Fun.id
;;

let do_not_generate_attribute =
  Attribute.declare
    "hegel.do_not_generate"
    Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    ()
;;

let rec generator_expr_of_core_type (ct : core_type) : expression =
  let loc = ct.ptyp_loc in
  match Attribute.get generator_override_attribute ct with
  | Some override -> override
  | None ->
    (match ct.ptyp_desc with
     | Ptyp_constr ({ txt = lid; _ }, args) ->
       let gen_ident =
         Ast_builder.Default.pexp_ident ~loc { txt = mangle_lid ~loc lid; loc }
       in
       (match args with
        | [] -> gen_ident
        | args ->
          Ast_builder.Default.pexp_apply
            ~loc
            gen_ident
            (List.map (fun arg -> Nolabel, generator_expr_of_core_type arg) args))
     | _ ->
       (match Ppx_compat.extract_tuple_types ct with
        | Some components ->
          [%expr Hegel.Generators.composite [%e tuple_thunk ~loc components]]
        | None ->
          Location.raise_errorf
            ~loc
            "ppx_hegel_generator: unsupported type in [@@deriving hegel_generator]"))

and make_draw_expr (ct : core_type) : expression =
  let loc = ct.ptyp_loc in
  [%expr Hegel.draw_silent _hegel_tc [%e generator_expr_of_core_type ct]]

and tuple_thunk ~loc components =
  let named =
    List.mapi (fun i ct -> Printf.sprintf "_tup_gen_%d" i, make_draw_expr ct) components
  in
  let tuple_expr =
    Ast_builder.Default.pexp_tuple
      ~loc
      (List.map
         (fun (vname, _) ->
            Ast_builder.Default.pexp_ident ~loc { txt = Lident vname; loc })
         named)
  in
  let body =
    List.fold_right
      (fun (vname, drawn) acc ->
         [%expr
           let [%p Ast_builder.Default.pvar ~loc vname] = [%e drawn] in
           [%e acc]])
      named
      tuple_expr
  in
  [%expr fun _hegel_tc -> [%e body]]
;;

(** [drawn_record ~loc ~wrap labels] returns an expression that draws the
    fields in declaration order and applies [wrap] to the record literal.
    [wrap] lets an inline-record constructor enclose the literal directly *)
let drawn_record ~loc ~wrap (labels : label_declaration list) : expression =
  if labels = []
  then Location.raise_errorf ~loc "ppx_hegel_generator: empty record types not supported";
  let named =
    List.map
      (fun (ld : label_declaration) ->
         ld.pld_name.txt, "_gen_" ^ ld.pld_name.txt, make_draw_expr ld.pld_type)
      labels
  in
  let record_expr =
    Ast_builder.Default.pexp_record
      ~loc
      (List.map
         (fun (field, vname, _) ->
            ( { txt = Lident field; loc }
            , Ast_builder.Default.pexp_ident ~loc { txt = Lident vname; loc } ))
         named)
      None
  in
  List.fold_right
    (fun (_, vname, drawn) acc ->
       [%expr
         let [%p Ast_builder.Default.pvar ~loc vname] = [%e drawn] in
         [%e acc]])
    named
    (wrap record_expr)
;;

(** [generator_of_record ~loc labels] returns a [test_case -> record]
    function that draws the fields in declaration order. *)
let generator_of_record ~loc (labels : label_declaration list) : expression =
  [%expr fun _hegel_tc -> [%e drawn_record ~loc ~wrap:Fun.id labels]]
;;

let generator_of_data_variant ~loc (constrs : constructor_declaration list) : expression =
  let n = List.length constrs in
  let index_options = List.init n (fun i -> Ast_builder.Default.eint ~loc i) in
  let match_arms =
    List.mapi
      (fun i (cd : constructor_declaration) ->
         let constr_lid = { txt = Lident cd.pcd_name.txt; loc } in
         let case rhs =
           Ast_builder.Default.case
             ~lhs:(Ast_builder.Default.pint ~loc i)
             ~guard:None
             ~rhs
         in
         match Ppx_compat.extract_constr_args cd.pcd_args with
         | Tuple [] -> case (Ast_builder.Default.pexp_construct ~loc constr_lid None)
         | Tuple [ ct ] ->
           case
             (Ast_builder.Default.pexp_construct
                ~loc
                constr_lid
                (Some (make_draw_expr ct)))
         | Tuple cts ->
           let named =
             List.mapi (fun j ct -> Printf.sprintf "_arg_%d" j, make_draw_expr ct) cts
           in
           let tuple_expr =
             Ast_builder.Default.pexp_tuple
               ~loc
               (List.map
                  (fun (vname, _) ->
                     Ast_builder.Default.pexp_ident ~loc { txt = Lident vname; loc })
                  named)
           in
           let inner_body =
             List.fold_right
               (fun (vname, drawn) acc ->
                  [%expr
                    let [%p Ast_builder.Default.pvar ~loc vname] = [%e drawn] in
                    [%e acc]])
               named
               (Ast_builder.Default.pexp_construct ~loc constr_lid (Some tuple_expr))
           in
           case inner_body
         | Record labels ->
           case
             (drawn_record
                ~loc
                ~wrap:(fun record ->
                  Ast_builder.Default.pexp_construct ~loc constr_lid (Some record))
                labels))
      constrs
  in
  let catch_all =
    Ast_builder.Default.case
      ~lhs:[%pat? _]
      ~guard:None
      ~rhs:[%expr failwith "ppx_hegel_generator: unreachable variant index"]
  in
  let all_arms = match_arms @ [ catch_all ] in
  let match_expr = Ast_builder.Default.pexp_match ~loc [%expr _variant_idx] all_arms in
  [%expr
    Hegel.Generators.Ppx_internal.composite_with_label
      ~label:Hegel.Generators.Ppx_internal.Labels.enum_variant
      (fun _hegel_tc ->
         let _variant_idx =
           Hegel.draw_silent
             _hegel_tc
             (Hegel.Generators.sampled_from
                [%e Ast_builder.Default.elist ~loc index_options])
         in
         [%e match_expr])]
;;

let generator_of_variant ~loc (constrs : constructor_declaration list) : expression =
  if constrs = []
  then Location.raise_errorf ~loc "ppx_hegel_generator: empty variant types not supported";
  let constrs =
    List.filter
      (fun cd -> Option.is_none (Attribute.get do_not_generate_attribute cd))
      constrs
  in
  if constrs = []
  then
    Location.raise_errorf
      ~loc
      "ppx_hegel_generator: all constructors are marked [@hegel.do_not_generate]";
  let is_nullary (cd : constructor_declaration) =
    match Ppx_compat.extract_constr_args cd.pcd_args with
    | Tuple [] -> true
    | _ -> false
  in
  if List.for_all is_nullary constrs
  then (
    let constr_values =
      List.map
        (fun (cd : constructor_declaration) ->
           Ast_builder.Default.pexp_construct
             ~loc
             { txt = Lident cd.pcd_name.txt; loc }
             None)
        constrs
    in
    [%expr
      Hegel.Generators.sampled_from [%e Ast_builder.Default.elist ~loc constr_values]])
  else generator_of_data_variant ~loc constrs
;;

(** [opaque_excluded_args td] adds [\[@sexp.opaque\]] to the argument types of
    [\[@hegel.do_not_generate\]] constructors. *)
let opaque_excluded_args (td : type_declaration) : type_declaration =
  match td.ptype_kind with
  | Ptype_variant constrs ->
    let opaque ct =
      let loc = ct.ptyp_loc in
      let attr =
        Ast_builder.Default.attribute
          ~loc
          ~name:{ txt = "sexp.opaque"; loc }
          ~payload:(PStr [])
      in
      { ct with ptyp_attributes = ct.ptyp_attributes @ [ attr ] }
    in
    let constrs =
      List.map
        (fun (cd : constructor_declaration) ->
           match Attribute.get do_not_generate_attribute cd with
           | Some () ->
             { cd with pcd_args = Ppx_compat.map_constr_arg_types opaque cd.pcd_args }
           | None -> cd)
        constrs
    in
    { td with ptype_kind = Ptype_variant constrs }
  | _ -> td
;;

(** [excluded_constructor_uses ~loc td] returns one [let _ = ...] item per
    excluded constructor, each constructing it once. Without these the
    compiler reports a constructor never used to build values warning. *)
let excluded_constructor_uses ~loc (td : type_declaration) : structure =
  match td.ptype_kind with
  | Ptype_variant constrs ->
    List.filter_map
      (fun (cd : constructor_declaration) ->
         match Attribute.get do_not_generate_attribute cd with
         | None -> None
         | Some () ->
           let constr_lid = { txt = Lident cd.pcd_name.txt; loc } in
           let construct arg = Ast_builder.Default.pexp_construct ~loc constr_lid arg in
           let evar name = Ast_builder.Default.evar ~loc name in
           (* turn an excluded constructor C(a,b,...) into fun a -> fun b -> ... -> C(a, b, ...) *)
           let to_curried_lam names body =
             List.fold_right
               (fun name acc ->
                  [%expr fun [%p Ast_builder.Default.pvar ~loc name] -> [%e acc]])
               names
               body
           in
           let build =
             match Ppx_compat.extract_constr_args cd.pcd_args with
             | Tuple [] -> construct None
             | Tuple cts ->
               let names = List.mapi (fun i _ -> Printf.sprintf "arg_%d" i) cts in
               let arg =
                 match names with
                 | [ name ] -> evar name
                 | _ -> Ast_builder.Default.pexp_tuple ~loc (List.map evar names)
               in
               to_curried_lam names (construct (Some arg))
             | Record labels ->
               let names =
                 List.map (fun (ld : label_declaration) -> ld.pld_name.txt) labels
               in
               let record =
                 Ast_builder.Default.pexp_record
                   ~loc
                   (List.map (fun name -> { txt = Lident name; loc }, evar name) names)
                   None
               in
               to_curried_lam names (construct (Some record))
           in
           Some [%stri let _ = [%e build]])
      constrs
  | _ -> []
;;

(** The main deriver. Emits the [sexp_of_<t>] definitions first (via
    ppx_sexp_conv's expander), then one printable generator per type:
    [with_printer sexp_of_<t>] around the derived generator. *)
let generate_impl ~ctxt ((rec_flag, type_decls) : rec_flag * type_declaration list)
  : structure
  =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let path = Code_path.to_string_path (Expansion_context.Deriver.code_path ctxt) in
  let sexp_items =
    Ppx_sexp_conv_expander.Sexp_of.str_type_decl
      ~loc
      ~path
      (rec_flag, List.map opaque_excluded_args type_decls)
  in
  let generator_items =
    List.map
      (fun (td : type_declaration) ->
         let gen_name = generator_name td.ptype_name.txt in
         let sexp_of =
           Ast_builder.Default.pexp_ident
             ~loc
             { txt = Lident ("sexp_of_" ^ td.ptype_name.txt); loc }
         in
         (* A record yields a [test_case -> t] thunk that [composite] wraps
            into a generator. A variant or an alias yields a complete
            generator expression. *)
         let generator_expr =
           match td.ptype_kind, td.ptype_manifest with
           | Ptype_record labels, _ ->
             [%expr Hegel.Generators.composite [%e generator_of_record ~loc labels]]
           | Ptype_variant constrs, _ -> generator_of_variant ~loc constrs
           | Ptype_abstract, Some ct -> generator_expr_of_core_type ct
           | Ptype_abstract, None ->
             Location.raise_errorf
               ~loc
               "ppx_hegel_generator: abstract types without manifest not supported"
           | _, _ ->
             Location.raise_errorf
               ~loc
               "ppx_hegel_generator: unsupported type kind in [@@deriving \
                hegel_generator]"
         in
         [%stri
           let [%p Ast_builder.Default.pvar ~loc gen_name] =
             Hegel.with_printer [%e sexp_of] [%e generator_expr]
           ;;])
      type_decls
  in
  sexp_items
  @ List.concat_map (excluded_constructor_uses ~loc) type_decls
  @ generator_items
;;

let _deriver =
  Deriving.add
    "hegel_generator"
    ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)
;;
