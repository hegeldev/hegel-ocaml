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
      type state = int [@@deriving sexp_of]

      let add tc n = n + draw tc (integers ~min_value:1 ~max_value:10 ()) [@@rule]
      let small _tc n = assert (n < 100) [@@invariant]
      let positive _tc n = assert (n >= 0) [@@invariant always_check]
    end
    ]}
    The above is rewritten into the following:
    {[
    let rules = [ Hegel.Stateful.Rule.create ~name:"add" ~step:add ]

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

    In a test body and in a marked rule or invariant body, a
    [let x = draw tc gen] binding has its name injected so the drawn value
    prints as [x = value]. *)

open Ppxlib

(** [extract_settings_attr attrs] returns the expression carried by
    [[@@settings expr]] if present, else [None]. *)
let extract_settings_attr (attrs : attributes) : expression option =
  List.find_map
    (fun (attr : attribute) ->
       if String.equal attr.attr_name.txt "settings"
       then (
         match attr.attr_payload with
         | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> Some e
         | _ ->
           Location.raise_errorf
             ~loc:attr.attr_loc
             "ppx_hegel_test: [@@settings ...] must carry a single expression")
       else None)
    attrs
;;

(** [parse_string_list e] returns the list of literal strings carried by [e]
    when [e] has the shape [[ "..."; "..."; ... ]], else raises a located error
    pointing at the offending sub-expression. *)
let rec parse_string_list (e : expression) : string list =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some payload) ->
    (match Ppx_compat.extract_expr_tuple payload with
     | Some [ head; tail ] ->
       let head_str =
         match head.pexp_desc with
         | Pexp_constant (Pconst_string (s, _, _)) -> s
         | _ ->
           Location.raise_errorf
             ~loc:head.pexp_loc
             "ppx_hegel_test: elements must be string literals"
       in
       head_str :: parse_string_list tail
     | _ -> Location.raise_errorf ~loc:e.pexp_loc "ppx_hegel_test: malformed list payload")
  | _ ->
    Location.raise_errorf
      ~loc:e.pexp_loc
      "ppx_hegel_test: expected a list literal of string literals"
;;

(** [extract_failure_blobs_attr attrs] returns the parsed string list if a
    [[@@failure_blobs ...]] attribute is present, else [None]. *)
let extract_failure_blobs_attr (attrs : attributes) : string list option =
  List.find_map
    (fun (attr : attribute) ->
       if String.equal attr.attr_name.txt "failure_blobs"
       then (
         match attr.attr_payload with
         | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] ->
           (match parse_string_list e with
            | [] ->
              Location.raise_errorf
                ~loc:attr.attr_loc
                "ppx_hegel_test: [@@failure_blobs ...] must have at least one element"
            | lst -> Some lst)
         | _ ->
           Location.raise_errorf
             ~loc:attr.attr_loc
             "ppx_hegel_test: [@@failure_blobs ...] must carry a list literal")
       else None)
    attrs
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

(** [build_items ~loc ~function_name ~settings_expr ~body_fn] returns the single
    structure item the expander splices in:

    {[
      let function_name () =
        Hegel.run_hegel_test [?settings] location body_fn
      ;;
    ]} *)
let build_items ~loc ~function_name ~settings_expr ~failure_blobs ~body_fn
  : structure_item list
  =
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
  [ definition ]
;;

(** [is_draw_lident lid] is [true] when [lid]'s final component is [draw],
    whether unqualified ([draw]) or qualified ([Hegel.draw], [Generators.draw],
    a module alias [G.draw], …). Precision comes from the receiver check in
    {!draw_binding_name} (the draw must be applied to the test's own [tc]), so
    this only needs to recognize the name; [draw_silent] is a different name and
    is excluded. *)
let is_draw_lident : longident -> bool = function
  | Lident "draw" | Ldot (_, "draw") -> true
  | _ -> false
;;

(** [draw_named_lident lid] is [lid] with its final [draw] component replaced by
    [draw_named], preserving the module prefix the user wrote, so the rewrite
    targets the same module's internal entry point (e.g. [Hegel.draw] becomes
    [Hegel.draw_named]). *)
let draw_named_lident : longident -> longident = function
  | Lident "draw" -> Lident "draw_named"
  | Ldot (prefix, "draw") -> Ldot (prefix, "draw_named")
  | other -> other
;;

(** [has_label_arg args] is [true] when an application already passes [~label]
    (or [?label]) explicitly, in which case the hand-written label wins. *)
let has_label_arg (args : (arg_label * expression) list) : bool =
  List.exists
    (fun (lbl, _) ->
       match lbl with
       | Labelled "label" | Optional "label" -> true
       | _ -> false)
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
  match List.find_map (fun (lbl, e) -> if lbl = Nolabel then Some e else None) args with
  | Some { pexp_desc = Pexp_ident { txt = Lident n; _ }; _ } -> String.equal n tc_name
  | _ -> false
;;

(** [draw_binding_name ~tc_name vb] returns [Some name] when [vb] is
    [let <name> = draw tc …] — a simple-variable binding whose right-hand side
    is a [draw] application on the test's own [tc] — and [None] otherwise. *)
let draw_binding_name ~tc_name (vb : value_binding) : string option =
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | ( Ppat_var { txt = name; _ }
    , Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args) )
    when is_draw_lident lid && tc_arg_is ~tc_name args -> Some name
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
               match draw_binding_name ~tc_name vb with
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

(** [inject_draw ~tc_name flags vb] rewrites [let x = M.draw tc gen] into
    [let x = M.draw_named ~label:"x" ~repeatable:b tc gen], so the drawn value
    prints as [x = value] (numbered when [x] is flagged repeatable in [flags] —
    reused name or drawn in a loop). It targets the internal [draw_named] rather
    than the public [draw] (so [repeatable] stays off the public API), keeping
    the module prefix [M] the user wrote. It fires only for a simple-variable
    binding whose right-hand side is a [draw] application on [tc] with no
    explicit [~label]; every other binding is unchanged. *)
let inject_draw ~tc_name (flags : (string, bool) Stdlib.Hashtbl.t) (vb : value_binding)
  : value_binding
  =
  match draw_binding_name ~tc_name vb, vb.pvb_expr.pexp_desc with
  | ( Some name
    , Pexp_apply (({ pexp_desc = Pexp_ident ({ txt = lid; _ } as ident); _ } as fn), args)
    )
    when not (has_label_arg args) ->
    let loc = vb.pvb_expr.pexp_loc in
    let repeatable =
      match Stdlib.Hashtbl.find_opt flags name with
      | Some b -> b
      | None -> false
    in
    let named_fn =
      { fn with pexp_desc = Pexp_ident { ident with txt = draw_named_lident lid } }
    in
    let named_args =
      [ Labelled "label", Ast_builder.Default.estring ~loc name
      ; Labelled "repeatable", Ast_builder.Default.ebool ~loc repeatable
      ]
    in
    { vb with
      pvb_expr = Ast_builder.Default.pexp_apply ~loc named_fn (named_args @ args)
    }
  | _ -> vb
;;

(** [is_draw_silent_lident lid] is [true] when [lid]'s final component is
    [draw_silent], qualified or not. Distinct from {!is_draw_lident}, which keys
    on the [draw] component; [draw_silent] is its own name. *)
let is_draw_silent_lident : longident -> bool = function
  | Lident "draw_silent" | Ldot (_, "draw_silent") -> true
  | _ -> false
;;

(** [draw_silent_named_lident lid] is [lid] with its final [draw_silent]
    component replaced by [draw_silent_named], preserving the module prefix, so
    the rewrite targets the same module's internal entry point (e.g.
    [Hegel.draw_silent] becomes [Hegel.draw_silent_named]). *)
let draw_silent_named_lident : longident -> longident = function
  | Lident "draw_silent" -> Lident "draw_silent_named"
  | Ldot (prefix, "draw_silent") -> Ldot (prefix, "draw_silent_named")
  | other -> other
;;

(** [has_name_arg args] is [true] when an application already passes [~name] (or
    [?name]) explicitly, in which case the hand-written name wins. *)
let has_name_arg (args : (arg_label * expression) list) : bool =
  List.exists
    (fun (lbl, _) ->
       match lbl with
       | Labelled "name" | Optional "name" -> true
       | _ -> false)
    args
;;

(** [draw_silent_binding_name ~tc_name vb] returns [Some name] when [vb] is
    [let <name> = draw_silent tc …] — a simple-variable binding whose right-hand
    side is a [draw_silent] application on the test's own [tc] — and [None]
    otherwise. *)
let draw_silent_binding_name ~tc_name (vb : value_binding) : string option =
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | ( Ppat_var { txt = name; _ }
    , Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args) )
    when is_draw_silent_lident lid && tc_arg_is ~tc_name args -> Some name
  | _ -> None
;;

(** [inject_draw_silent ~tc_name vb] rewrites [let x = draw_silent tc gen] into
    [let x = draw_silent_named ~name:"x" tc gen], so a function generator
    ({!Hegel.Generators.functions}) drawn there labels its shown pairs
    [x arg = result]. [~name] is ignored for every other generator, so the
    rewrite is harmless. Like {!inject_draw} it targets the internal
    [draw_silent_named] (keeping [~name] off the public [draw_silent]) and keeps
    the module prefix the user wrote. It fires only for a simple-variable
    binding whose right-hand side is a [draw_silent] application on [tc] with no
    explicit [~name]. *)
let inject_draw_silent ~tc_name (vb : value_binding) : value_binding =
  match draw_silent_binding_name ~tc_name vb, vb.pvb_expr.pexp_desc with
  | ( Some name
    , Pexp_apply (({ pexp_desc = Pexp_ident ({ txt = lid; _ } as ident); _ } as fn), args)
    )
    when not (has_name_arg args) ->
    let loc = vb.pvb_expr.pexp_loc in
    let named_fn =
      { fn with pexp_desc = Pexp_ident { ident with txt = draw_silent_named_lident lid } }
    in
    let named_args = [ Labelled "name", Ast_builder.Default.estring ~loc name ] in
    { vb with
      pvb_expr = Ast_builder.Default.pexp_apply ~loc named_fn (named_args @ args)
    }
  | _ -> vb
;;

(** A traversal that applies {!inject_draw} and {!inject_draw_silent} to every
    [let]-binding in an expression, threading the precomputed [flags], so labels
    are injected throughout the test body (nested [let]s, helper functions,
    match arms, …). Each binding is at most one of a [draw] or a [draw_silent]
    on [tc], so the two injectors compose (each leaves the other's bindings
    untouched). Draws nested inside a generation span are still suppressed at
    runtime by the depth gate, so labeling them is harmless. *)
let label_injector ~tc_name flags =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      Ppx_compat.map_let_value_bindings
        (List.map (fun vb -> inject_draw_silent ~tc_name (inject_draw ~tc_name flags vb)))
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
    (label_injector ~tc_name flags)#expression fn
;;

(** Expander for a single [let%hegel_test ...] structure item. *)
let expand_value_binding ~loc (vb : value_binding) : structure_item list =
  let function_name = extract_function_name ~what:"test" vb.pvb_pat in
  let settings_expr = extract_settings_attr vb.pvb_attributes in
  let failure_blobs = extract_failure_blobs_attr vb.pvb_attributes in
  (* The body of [let%hegel_test name <args> = expr] is parsed as [let name = <args -> expr>]. We pass that lambda as the [test_fn] to
     [Hegel.run_hegel_test]. *)
  let body_fn = inject_labels vb.pvb_expr in
  build_items ~loc ~function_name ~settings_expr ~failure_blobs ~body_fn
;;

(** A marker attribute on a binding inside a [module%hegel_state_machine]. *)
type marker =
  | Rule
  | Invariant of { always_check : bool }

let marker_of_attr (attr : attribute) : marker option =
  match attr.attr_name.txt, attr.attr_payload with
  | "rule", PStr [] -> Some Rule
  | "rule", _ ->
    Location.raise_errorf ~loc:attr.attr_loc "ppx_hegel_test: [@@@@rule] takes no payload"
  | "invariant", PStr [] -> Some (Invariant { always_check = false })
  | ( "invariant"
    , PStr
        [ { pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_ident { txt = Lident "always_check"; _ }; _ }, _)
          ; _
          }
        ] ) -> Some (Invariant { always_check = true })
  | "invariant", _ ->
    Location.raise_errorf
      ~loc:attr.attr_loc
      "ppx_hegel_test: [@@@@invariant] takes no payload, or [always_check]"
  | _ -> None
;;

let is_marker (attr : attribute) =
  String.equal attr.attr_name.txt "rule" || String.equal attr.attr_name.txt "invariant"
;;

(** [expand_machine_item item] returns [item] with its marker attributes
    removed and the draws in marked bodies labeled and the
    [(name, marker)] of every marked binding it held. *)
let expand_machine_item (item : structure_item) : structure_item * (string * marker) list =
  match item.pstr_desc with
  | Pstr_value (rec_flag, vbs) ->
    let expand_binding (vb : value_binding) =
      match List.filter_map marker_of_attr vb.pvb_attributes with
      | [] -> vb, None
      | _ :: _ :: _ ->
        Location.raise_errorf
          ~loc:vb.pvb_loc
          "ppx_hegel_test: a binding can be marked [@@@@rule] or [@@@@invariant], not \
           both"
      | [ marker ] ->
        let what =
          match marker with
          | Rule -> "rule"
          | Invariant _ -> "invariant"
        in
        let name = extract_function_name ~what vb.pvb_pat in
        ( { vb with
            pvb_expr = inject_labels vb.pvb_expr
          ; pvb_attributes = List.filter (fun a -> not (is_marker a)) vb.pvb_attributes
          }
        , Some (name, marker) )
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

(** Expander for [module%hegel_state_machine M = struct … end]. It keeps the
    body's items, with the marker attributes removed and the draws in marked
    bodies labelled, and appends [rules], [invariants], and
    [run ?step_count ?sexp_of_state tc ~init]. At least one [[@@rule]] binding
    is required. *)
let expand_state_machine ~loc (mb : module_binding) : structure_item list =
  let items =
    match mb.pmb_expr.pmod_desc with
    | Pmod_structure items -> items
    | _ ->
      Location.raise_errorf
        ~loc
        "ppx_hegel_test: module%%hegel_state_machine expects a [struct … end] body"
  in
  let items, marked = List.split (List.map expand_machine_item items) in
  let marked = List.concat marked in
  let rules, invariants =
    List.fold_right
      (fun (name, marker) (rules, invariants) ->
         match marker with
         | Rule -> name :: rules, invariants
         | Invariant { always_check } -> rules, (name, always_check) :: invariants)
      marked
      ([], [])
  in
  if List.is_empty rules
  then
    Location.raise_errorf
      ~loc
      "ppx_hegel_test: a state machine needs at least one [@@@@rule] binding";
  let open Ast_builder.Default in
  let rule_exprs =
    List.map
      (fun name ->
         [%expr
           Hegel.Stateful.Rule.create
             ~name:[%e estring ~loc name]
             ~step:[%e evar ~loc name]])
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
  let run =
    if has_sexp_of_state items
    then
      [%stri
        let run ?step_count ?(sexp_of_state = sexp_of_state) tc ~init =
          Hegel.Stateful.run_internal
            ~init
            ~rules
            ~invariants
            ~sexp_of_state
            ?step_count
            tc
        ;;]
    else
      [%stri
        let run ?step_count ?sexp_of_state tc ~init =
          Hegel.Stateful.run_internal
            ~init
            ~rules
            ~invariants
            ?sexp_of_state
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
    (fun ~loc ~path:_ mb -> expand_state_machine ~loc mb)
;;

let () =
  Driver.register_transformation
    "ppx_hegel_test"
    ~rules:
      [ Context_free.Rule.extension extension
      ; Context_free.Rule.extension state_machine_extension
      ]
;;
