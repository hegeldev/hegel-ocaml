(** PPX expander for [let%hegel_test ...] and [module%hegel_state_machine ...].

    Rewrites:
    {[
      let%hegel_test my_test tc = body
      [@@settings expr]
      [@@failure_blobs [ "<base64>"; ... ]]
    ]}
    into:
    {[
      let my_test () =
        Hegel.run_hegel_test_ppx
          ~settings:expr
          ~test_location:{ function_name; file; begin_line }
          ~failure_blobs:[ "<base64>"; ... ]
          (fun tc -> body)
      ;;
    ]}

    [my_test] has the type [unit -> unit]. The [@@settings ...] and
    [@@failure_blobs ...] attributes are both optional.

    A state machine is a module whose rules and invariants are marked:
    {[
    module%hegel_state_machine Counter = struct
      type state = int ref

      let sexp_of_state n = sexp_of_int !n
      let add tc n = n := !n + draw tc (integers ~min_value:1 ~max_value:10 ()) [@@rule]
      let small _tc n = assert (!n < 100) [@@invariant]
      let positive _tc n = assert (!n >= 0) [@@invariant { always_check = true }]
    end
    ]}
    The above is rewritten into the following:
    {[
    let rules = [ Hegel.Stateful.Rule.create ~name:"add" ~weight:1.0 ~step:add () ]

    let invariants =
      [ Hegel.Stateful.Invariant.create ~name:"small" ~inv:small ~always_check:false ()
      ; Hegel.Stateful.Invariant.create
          ~name:"positive"
          ~inv:positive
          ~always_check:true
          ()
      ]
    ;;

    let run ?step_count ?(sexp_of_state = sexp_of_state) tc ~init =
      Hegel.Stateful.run_internal ~init ~rules ~invariants ~sexp_of_state ?step_count tc
    ;;
    ]}

    A marker's options are written as a record, whose fields may be given in
    any order and any of which may be left out: [[@@rule { weight = 2.5 }]],
    [[@@invariant { always_check = true }]]. A rule's [weight] tells the engine
    how often to pick that rule relative to the others, and defaults to [1.0].
    A rule in a [module%hegel_concurrent_state_machine] also takes a [group],
    as [[@@rule { group = "io"; weight = 2.5 }]]; a group on a sequential rule
    is an error.

    In a test body and in a marked rule or invariant body, a
    [let x = draw tc gen] binding has its name injected so the drawn value
    prints as [x = value]. *)

open Ppxlib

let settings_attribute =
  Attribute.declare
    "hegel.settings"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload __)
    Fun.id
;;

let failure_blobs_attribute =
  Attribute.declare_with_attr_loc
    "hegel.failure_blobs"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (elist (estring __)))
    (fun ~attr_loc blobs -> attr_loc, blobs)
;;

type rule_payload =
  { group : string option
  ; weight : string option
  }

let weight_pattern () =
  Ast_pattern.(efloat __ ||| map1 (eint __) ~f:(fun weight -> string_of_int weight ^ "."))
;;

let record_payload fields = Ast_pattern.(single_expr_payload (pexp_record fields none))
let field name pattern = Ast_pattern.(loc (lident (string name)) ** pattern)
let group_field () = field "group" Ast_pattern.(estring __)
let weight_field () = field "weight" (weight_pattern ())

let rule_attribute =
  Attribute.declare_with_attr_loc
    "hegel.rule"
    Attribute.Context.value_binding
    Ast_pattern.(
      map0 (pstr nil) ~f:{ group = None; weight = None }
      ||| map1
            (record_payload (group_field () ^:: nil))
            ~f:(fun group -> { weight = None; group = Some group })
      ||| map1
            (record_payload (weight_field () ^:: nil))
            ~f:(fun weight -> { group = None; weight = Some weight })
      ||| map2
            (record_payload (group_field () ^:: weight_field () ^:: nil))
            ~f:(fun group weight -> { group = Some group; weight = Some weight })
      ||| map2
            (record_payload (weight_field () ^:: group_field () ^:: nil))
            ~f:(fun weight group -> { group = Some group; weight = Some weight }))
    (fun ~attr_loc payload -> attr_loc, payload)
;;

let invariant_attribute =
  Attribute.declare
    "hegel.invariant"
    Attribute.Context.value_binding
    Ast_pattern.(
      map0 (pstr nil) ~f:false ||| record_payload (field "always_check" (ebool __) ^:: nil))
    Fun.id
;;

let consume attr node =
  match Attribute.consume attr node with
  | Some (node, payload) -> node, Some payload
  | None -> node, None
;;

(** [extract_function_name ~what pat] returns the name bound by [pat] if [pat]
    is a simple variable, else raises naming [what] ("test", "rule", or
    "invariant"). *)
let extract_function_name ~what (pat : pattern) : string =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "ppx_hegel_test: the %s binding must be a simple identifier"
      what
;;

(** [build_location_record ~loc ~function_name] returns an expression of type
    [Hegel.test_location] populated from the binding's source location. *)
let build_location_record ~loc ~function_name : expression =
  let file_str = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  [%expr
    { Hegel.function_name = [%e Ast_builder.Default.estring ~loc function_name]
    ; file = [%e Ast_builder.Default.estring ~loc file_str]
    ; begin_line = [%e Ast_builder.Default.eint ~loc line]
    }]
;;

(** [set_attributes attrs item] is the [let] stri [item] with [attrs]
    as the attributes of every binding in [item]. *)
let set_attributes (attrs : attributes) (item : structure_item) : structure_item =
  match item.pstr_desc with
  | Pstr_value (rec_flag, vbs) ->
    let vbs = List.map (fun vb -> { vb with pvb_attributes = attrs }) vbs in
    { item with pstr_desc = Pstr_value (rec_flag, vbs) }
  | _ -> item
;;

(** [build_items ~loc ~function_name ~settings_expr ~body_fn] returns the single
    structure item the expander splices in:

    {[
      let function_name () =
        Hegel.run_hegel_test [?settings] location body_fn
      ;;
    ]}

    [attrs] are the attributes of the original binding that the expander did not
    consume. *)
let build_items ~loc ~function_name ~settings_expr ~failure_blobs ~body_fn ~attrs
  : structure_item list
  =
  let loc = { loc with loc_ghost = true } in
  let location_record = build_location_record ~loc ~function_name in
  let base_call =
    match settings_expr with
    | Some s ->
      [%expr
        Hegel.run_hegel_test_ppx ~settings:[%e s] ~test_location:[%e location_record]]
    | None -> [%expr Hegel.run_hegel_test_ppx ~test_location:[%e location_record]]
  in
  let call =
    match failure_blobs with
    | Some bs ->
      let elements = List.map (Ast_builder.Default.estring ~loc) bs in
      let failure_blobs_e = Ast_builder.Default.elist ~loc elements in
      [%expr [%e base_call] ~failure_blobs:[%e failure_blobs_e] [%e body_fn]]
    | None -> [%expr [%e base_call] [%e body_fn]]
  in
  let pat = Ast_builder.Default.pvar ~loc function_name in
  let definition = [%stri let [%p pat] = fun () -> [%e call]] in
  [ set_attributes attrs definition ]
;;

(** [is_named name lid] is [true] when [lid]'s final component is [name],
    whether unqualified ([draw]) or qualified ([Hegel.draw], a module alias
    [G.draw], …). *)
let is_named name : longident -> bool = function
  | Lident n | Ldot (_, n) -> String.equal n name
  | Lapply _ -> false
;;

let rename_last ~to_ : longident -> longident = function
  | Lident _ -> Lident to_
  | Ldot (prefix, _) -> Ldot (prefix, to_)
  | other -> other
;;

let has_label label (args : (arg_label * expression) list) : bool =
  List.exists
    (fun (lbl, _) ->
       match lbl with
       | Labelled l | Optional l -> String.equal l label
       | Nolabel -> false)
    args
;;

(** [param_name pat] is the variable bound by [pat], if it is a simple variable
    (possibly type-annotated), else [None]. *)
let rec param_name (pat : pattern) : string option =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | _ ->
    (* [Ppat_constraint] arity differs between standard OCaml and OxCaml, so
       unwrap a type-annotated pattern through the compat shim. *)
    (match Ppx_compat.unwrap_pattern_constraint pat with
     | Some p -> param_name p
     | None -> None)
;;

(** [test_case_name e] is the name of the test function's first parameter (its
    [tc]), used as the receiver the rewrite keys off. [None] when the binding is
    not a function or its parameter is not a simple variable. *)
let test_case_name (e : expression) : string option =
  match Ppx_compat.expr_first_param_pat e with
  | Some pat -> param_name pat
  | None -> None
;;

(** [tc_arg_is ~tc_name args] is [true] when the first positional argument of an
    application is exactly the identifier [tc_name]. *)
let tc_arg_is ~tc_name (args : (arg_label * expression) list) : bool =
  match
    List.find_map
      (fun (lbl, e) ->
         match lbl with
         | Nolabel -> Some e
         | Labelled _ | Optional _ -> None)
      args
  with
  | Some { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ } -> String.equal n tc_name
  | _ -> false
;;

let drawn_binding_name ~tc_name ~fn_name (vb : value_binding) : string option =
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | ( Ppat_var { txt = name; _ }
    , Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args) )
    when is_named fn_name lid && tc_arg_is ~tc_name args -> Some name
  | _ -> None
;;

(** [collect_repeatable body] maps each draw-bound name to whether its draws
    should be numbered. A name is repeatable if it is drawn more than once, or
    drawn anywhere at block depth > 0 (inside a function, [for], or [while]
    body, where it may run repeatedly). *)
let collect_repeatable ~tc_name (body : expression) : (string, bool) Stdlib.Hashtbl.t =
  let flags : (string, bool) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 8 in
  let depth = ref 0 in
  let record name =
    let seen = Stdlib.Hashtbl.mem flags name in
    if not seen then Stdlib.Hashtbl.replace flags name false;
    if !depth > 0 || seen then Stdlib.Hashtbl.replace flags name true
  in
  let collector =
    object
      inherit Ast_traverse.iter as super

      method! expression e =
        (* [Pexp_let]/[Pexp_fun]/[Pexp_function] arities differ between OCaml
           flavors, so go through the compat shim. *)
        match Ppx_compat.extract_let_bindings e with
        | Some vbs ->
          List.iter
            (fun vb ->
               match drawn_binding_name ~tc_name ~fn_name:"draw" vb with
               | Some name -> record name
               | None -> ())
            vbs;
          super#expression e
        | None ->
          (match e.pexp_desc with
           | Pexp_for _ | Pexp_while _ ->
             incr depth;
             super#expression e;
             decr depth
           | _ when Ppx_compat.is_function_expr e ->
             incr depth;
             super#expression e;
             decr depth
           | _ -> super#expression e)
    end
  in
  collector#expression body;
  flags
;;

(** [inject ~tc_name ~fn_name ~veto ~extra_args vb] rewrites
    [let x = M.<fn_name> tc gen] into [let x = M.<fn_name>_named <extra_args> tc gen] *)
let inject ~tc_name ~fn_name ~veto ~extra_args (vb : value_binding) : value_binding =
  match drawn_binding_name ~tc_name ~fn_name vb, vb.pvb_expr.pexp_desc with
  | ( Some name
    , Pexp_apply (({ pexp_desc = Pexp_ident ({ txt = lid; _ } as ident); _ } as fn), args)
    )
    when not (has_label veto args) ->
    let loc = { vb.pvb_expr.pexp_loc with loc_ghost = true } in
    let named_fn =
      { fn with
        pexp_desc =
          Pexp_ident { ident with txt = rename_last ~to_:(fn_name ^ "_named") lid }
      }
    in
    { vb with
      pvb_expr = Ast_builder.Default.pexp_apply ~loc named_fn (extra_args ~loc name @ args)
    }
  | _ -> vb
;;

let inject_labels ~tc_name (flags : (string, bool) Stdlib.Hashtbl.t) =
  let inject_draw =
    inject ~tc_name ~fn_name:"draw" ~veto:"label" ~extra_args:(fun ~loc name ->
      let repeatable =
        match Stdlib.Hashtbl.find_opt flags name with
        | Some b -> b
        | None -> false
      in
      [ Labelled "label", Ast_builder.Default.estring ~loc name
      ; Labelled "repeatable", Ast_builder.Default.ebool ~loc repeatable
      ])
  in
  let inject_draw_silent =
    inject ~tc_name ~fn_name:"draw_silent" ~veto:"name" ~extra_args:(fun ~loc name ->
      [ Labelled "name", Ast_builder.Default.estring ~loc name ])
  in
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      Ppx_compat.map_let_value_bindings
        (List.map (fun vb -> inject_draw_silent (inject_draw vb)))
        e
  end
;;

(** [inject_labels fn] is the lambda [fn] ([fun tc <args> -> body]) with
    [~label]/[~repeatable] injected into the draws on its own [tc] parameter, so
    the counterexample replay prints [name = value]. *)
let inject_labels (fn : expression) : expression =
  match test_case_name fn with
  | None -> fn
  | Some tc_name ->
    let flags = collect_repeatable ~tc_name (Ppx_compat.peel_fun_params fn) in
    (inject_labels ~tc_name flags)#expression fn
;;

(** Expander for a single [let%hegel_test ...] structure item. *)
let expand_value_binding ~loc (vb : value_binding) : structure_item list =
  let function_name = extract_function_name ~what:"test" vb.pvb_pat in
  let vb, settings_expr = consume settings_attribute vb in
  let vb, failure_blobs = consume failure_blobs_attribute vb in
  let failure_blobs =
    Option.map
      (fun (attr_loc, blobs) ->
         if List.is_empty blobs
         then
           Location.raise_errorf
             ~loc:attr_loc
             "ppx_hegel_test: [@@@@failure_blobs ...] must have at least one element";
         blobs)
      failure_blobs
  in
  (* The body of [let%hegel_test name <args> = expr] is parsed as [let name = <args -> expr>]. We pass that lambda as the [test_fn] to
     [Hegel.run_hegel_test]. *)
  let body_fn = inject_labels vb.pvb_expr in
  build_items
    ~loc
    ~function_name
    ~settings_expr
    ~failure_blobs
    ~body_fn
    ~attrs:vb.pvb_attributes
;;

(** A marker attribute on a binding inside a [module%hegel_state_machine]. *)
type marker =
  | Rule of
      { group : string option
      ; weight : string
      }
  | Invariant of { always_check : bool }

let rule_marker ~concurrent ~attr_loc { group; weight } =
  if Option.is_some group && not concurrent
  then
    Location.raise_errorf
      ~loc:attr_loc
      "ppx_hegel_test: rule groups are only supported in \
       module%%hegel_concurrent_state_machine";
  Rule { group; weight = Option.value weight ~default:"1.0" }
;;

(** [marker_of_binding ~concurrent vb] is [vb] with its marker attribute
    removed and the marker that attribute carried, if any. A binding carries
    at most one marker, so the other attribute is consumed only to reject one
    that carries both. *)
let marker_of_binding ~concurrent (vb : value_binding) : value_binding * marker option =
  let marked_both (vb : value_binding) =
    Location.raise_errorf
      ~loc:vb.pvb_loc
      "ppx_hegel_test: a binding can be marked [@@@@rule] or [@@@@invariant], not both"
  in
  match consume rule_attribute vb with
  | vb, Some (attr_loc, payload) ->
    (match consume invariant_attribute vb with
     | vb, None -> vb, Some (rule_marker ~concurrent ~attr_loc payload)
     | vb, Some _ -> marked_both vb)
  | vb, None ->
    (match consume invariant_attribute vb with
     | vb, None -> vb, None
     | vb, Some always_check -> vb, Some (Invariant { always_check }))
;;

(** [expand_machine_item item] returns [item] with its marker attributes
    removed and the draws in marked bodies labeled and the
    [(name, marker)] of every marked binding it held. *)
let expand_machine_item ~concurrent (item : structure_item)
  : structure_item * (string * marker) list
  =
  match item.pstr_desc with
  | Pstr_value (rec_flag, vbs) ->
    let expand_binding (vb : value_binding) =
      match marker_of_binding ~concurrent vb with
      | vb, None -> vb, None
      | vb, Some marker ->
        let what =
          match marker with
          | Rule _ -> "rule"
          | Invariant _ -> "invariant"
        in
        let name = extract_function_name ~what vb.pvb_pat in
        { vb with pvb_expr = inject_labels vb.pvb_expr }, Some (name, marker)
    in
    let vbs, marked = List.split (List.map expand_binding vbs) in
    { item with pstr_desc = Pstr_value (rec_flag, vbs) }, List.filter_map Fun.id marked
  | _ -> item, []
;;

let has_sexp_of_state (items : structure_item list) : bool =
  let mentions_sexp (e : expression) =
    let finder =
      object
        inherit [bool] Ast_traverse.fold as super

        method! expression e found =
          match e.pexp_desc with
          | Pexp_ident { txt = Lident ("sexp_of" | "sexp"); _ } -> true
          | _ -> super#expression e found
      end
    in
    finder#expression e false
  in
  List.exists
    (fun (item : structure_item) ->
       match item.pstr_desc with
       | Pstr_value (_, vbs) ->
         List.exists
           (fun (vb : value_binding) ->
              match vb.pvb_pat.ppat_desc with
              | Ppat_var { txt = "sexp_of_state"; _ } -> true
              | _ -> false)
           vbs
       | Pstr_type (_, decls) ->
         List.exists
           (fun (decl : type_declaration) ->
              String.equal decl.ptype_name.txt "state"
              && List.exists
                   (fun (attr : attribute) ->
                      String.equal attr.attr_name.txt "deriving"
                      &&
                      match attr.attr_payload with
                      | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> mentions_sexp e
                      | _ -> false)
                   decl.ptype_attributes)
           decls
       | _ -> false)
    items
;;

(** Expand sequential and concurrent state-machine modules, collecting marked
    bindings and appending their rules, invariants, and runner. *)
let expand_state_machine ~concurrent ~loc (mb : module_binding) : structure_item list =
  let extension_name =
    if concurrent then "hegel_concurrent_state_machine" else "hegel_state_machine"
  in
  let items =
    match mb.pmb_expr.pmod_desc with
    | Pmod_structure items -> items
    | _ ->
      Location.raise_errorf
        ~loc
        "ppx_hegel_test: module%%%s expects a [struct … end] body"
        extension_name
  in
  let items, marked = List.split (List.map (expand_machine_item ~concurrent) items) in
  let marked = List.concat marked in
  let rules, invariants =
    List.fold_right
      (fun (name, marker) (rules, invariants) ->
         match marker with
         | Rule { group; weight } -> (name, group, weight) :: rules, invariants
         | Invariant { always_check } -> rules, (name, always_check) :: invariants)
      marked
      ([], [])
  in
  if List.is_empty rules
  then
    Location.raise_errorf
      ~loc
      "ppx_hegel_test: module%%%s needs at least one [@@@@rule] binding"
      extension_name;
  let loc = { loc with loc_ghost = true } in
  let open Ast_builder.Default in
  let rule_exprs =
    List.map
      (fun (name, group, weight) ->
         if concurrent
         then
           [%expr
             Hegel.Stateful.Concurrent_rule.create
               ?group:
                 [%e
                   match group with
                   | None -> [%expr None]
                   | Some group -> [%expr Some [%e estring ~loc group]]]
               ~weight:[%e efloat ~loc weight]
               ~name:[%e estring ~loc name]
               ~step:[%e evar ~loc name]
               ()]
         else
           [%expr
             Hegel.Stateful.Rule.create
               ~name:[%e estring ~loc name]
               ~weight:[%e efloat ~loc weight]
               ~step:[%e evar ~loc name]
               ()])
      rules
  in
  let invariant_exprs =
    List.map
      (fun (name, always_check) ->
         [%expr
           Hegel.Stateful.Invariant.create
             ~name:[%e estring ~loc name]
             ~inv:[%e evar ~loc name]
             ~always_check:[%e ebool ~loc always_check]
             ()])
      invariants
  in
  let default_sexp_of_state =
    if has_sexp_of_state items then [%expr Some sexp_of_state] else [%expr None]
  in
  let run =
    if concurrent
    then
      [%stri
        let run
              ?concurrency
              ?min_concurrency
              ?max_concurrency
              ?step_count
              ?sexp_of_state:override
              tc
              ~init
          =
          Hegel.Stateful.run_concurrent_internal
            ~init
            ~rules
            ~invariants
            ?concurrency
            ?min_concurrency
            ?max_concurrency
            ?sexp_of_state:
              (match override with
               | Some _ as s -> s
               | None -> [%e default_sexp_of_state])
            ?step_count
            tc
        ;;]
    else
      [%stri
        let run ?step_count ?sexp_of_state:override tc ~init =
          Hegel.Stateful.run_internal
            ~init
            ~rules
            ~invariants
            ?sexp_of_state:
              (match override with
               | Some _ as s -> s
               | None -> [%e default_sexp_of_state])
            ?step_count
            tc
        ;;]
  in
  let generated =
    [ [%stri let rules = [%e elist ~loc rule_exprs]]
    ; [%stri let invariants = [%e elist ~loc invariant_exprs]]
    ; run
    ]
  in
  let pmb_expr = { mb.pmb_expr with pmod_desc = Pmod_structure (items @ generated) } in
  [ pstr_module ~loc { mb with pmb_expr } ]
;;

let extension =
  Extension.declare_inline
    "hegel_test"
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive (__ ^:: nil) ^:: nil))
    (fun ~loc ~path:_ vb -> expand_value_binding ~loc vb)
;;

let state_machine_extension =
  Extension.declare_inline
    "hegel_state_machine"
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_module __ ^:: nil))
    (fun ~loc ~path:_ mb -> expand_state_machine ~concurrent:false ~loc mb)
;;

let concurrent_state_machine_extension =
  Extension.declare_inline
    "hegel_concurrent_state_machine"
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_module __ ^:: nil))
    (fun ~loc ~path:_ mb -> expand_state_machine ~concurrent:true ~loc mb)
;;

let () =
  Driver.register_transformation
    "ppx_hegel_test"
    ~rules:
      [ Context_free.Rule.extension extension
      ; Context_free.Rule.extension state_machine_extension
      ; Context_free.Rule.extension concurrent_state_machine_extension
      ]
;;
