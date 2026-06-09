(** PPX expander for [let%hegel_test ...].

    Rewrites:
    {[
      let%hegel_test my_test tc = body
      [@@settings expr]
      [@@failure_blobs [ "<base64>"; ... ]]
    ]}
    into:
    {[
      let my_test () =
        Hegel.run_hegel_test
          ~settings:expr
          ~test_location:{ function_name; file; begin_line }
          ~failure_blobs:[ "<base64>"; ... ]
          (fun tc -> body)
      ;;

      let () =
        Hegel_test_runtime.register
          ~name:"my_test"
          ~file:"…"
          ~line:…
          my_test
    ]}

    The generated function remains directly callable, but is also auto-registered
    with [Hegel_test_runtime] so that [dune runtest] (via the [ppx_hegel_test]
    inline-tests backend) discovers and runs it.

    The [@@settings ...] and [@@failure_blobs ...] attributes are both optional. *)

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

(** [parse_string_list e] returns the list of literal strings carried by
    [e] when [e] has the shape [[ "..."; "..."; ... ]], else raises a
    located error pointing at the offending sub-expression. *)
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

(** [extract_failure_blobs_attr attrs] returns the parsed string list
    if a [[@@failure_blobs ...]] attribute is present, else [None]. *)
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

(** [extract_function_name pat] returns the name bound by [pat] if [pat] is a
    simple variable, else raises. *)
let extract_function_name (pat : pattern) : string =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ ->
    Location.raise_errorf
      ~loc:pat.ppat_loc
      "ppx_hegel_test: the test binding must be a simple identifier"
;;

(** [build_location_record ~loc ~function_name] returns an expression of type
    [Hegel.Antithesis.test_location] populated from the binding's source
    location. *)
let build_location_record ~loc ~function_name : expression =
  let file_str = loc.loc_start.pos_fname in
  let line = loc.loc_start.pos_lnum in
  [%expr
    { Hegel.Antithesis.function_name = [%e Ast_builder.Default.estring ~loc function_name]
    ; file = [%e Ast_builder.Default.estring ~loc file_str]
    ; begin_line = [%e Ast_builder.Default.eint ~loc line]
    }]
;;

(** [build_items ~loc ~function_name ~settings_expr ~body_fn] returns the pair
    of structure items the expander splices in:

    {[
      let function_name () =
        Hegel.run_hegel_test [?settings] location body_fn
      ;;

      let () =
        Hegel_test_runtime.register ~name:.. ~file:.. ~line:.. function_name
    ]} *)
let build_items ~loc ~function_name ~settings_expr ~failure_blobs ~body_fn
  : structure_item list
  =
  let location_record = build_location_record ~loc ~function_name in
  let base_call =
    match settings_expr with
    | Some s ->
      [%expr Hegel.run_hegel_test ~settings:[%e s] ~test_location:[%e location_record]]
    | None -> [%expr Hegel.run_hegel_test ~test_location:[%e location_record]]
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
  let name_e = Ast_builder.Default.estring ~loc function_name in
  let file_e = Ast_builder.Default.estring ~loc loc.loc_start.pos_fname in
  let line_e = Ast_builder.Default.eint ~loc loc.loc_start.pos_lnum in
  let ident = Ast_builder.Default.evar ~loc function_name in
  let registration =
    [%stri
      let () =
        Hegel_test_runtime.register
          ~name:[%e name_e]
          ~file:[%e file_e]
          ~line:[%e line_e]
          [%e ident]
      ;;]
  in
  [ definition; registration ]
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
    [let <name> = draw tc …] — a simple-variable binding whose right-hand side is
    a [draw] application on the test's own [tc] — and [None] otherwise. *)
let draw_binding_name ~tc_name (vb : value_binding) : string option =
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | ( Ppat_var { txt = name; _ }
    , Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args) )
    when is_draw_lident lid && tc_arg_is ~tc_name args -> Some name
  | _ -> None
;;

(** [collect_repeatable body] maps each draw-bound name to whether its draws
    should be numbered. A name is repeatable
    if it is drawn more than once, or drawn anywhere at block depth > 0 (inside a
    function, [for], or [while] body, where it may run repeatedly). *)
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

(** A traversal that applies {!inject_draw} to every [let]-binding in an
    expression, threading the precomputed [flags], so labels are injected
    throughout the test body (nested [let]s, helper functions, match arms, …).
    Draws nested inside a generation span are still suppressed at runtime by the
    depth gate, so labeling them is harmless. *)
let label_injector ~tc_name flags =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      Ppx_compat.map_let_value_bindings (List.map (inject_draw ~tc_name flags)) e
  end
;;

(** Expander for a single [let%hegel_test ...] structure item. *)
let expand_value_binding ~loc (vb : value_binding) : structure_item list =
  let function_name = extract_function_name vb.pvb_pat in
  let settings_expr = extract_settings_attr vb.pvb_attributes in
  let failure_blobs = extract_failure_blobs_attr vb.pvb_attributes in
  (* The body of [let%hegel_test name <args> = expr] is parsed as
     [let name = <args -> expr>]. We pass that lambda as the [test_fn] to
     [Hegel.run_hegel_test], first injecting [~label]/[~repeatable] from the
     binding names — for draws on the test's own [tc] — so the counterexample
     replay prints [name = value]. With no recognizable [tc] parameter, the body
     is passed through unchanged. *)
  let body_fn =
    match test_case_name vb.pvb_expr with
    | None -> vb.pvb_expr
    | Some tc_name ->
      let flags = collect_repeatable ~tc_name (Ppx_compat.peel_fun_params vb.pvb_expr) in
      (label_injector ~tc_name flags)#expression vb.pvb_expr
  in
  build_items ~loc ~function_name ~settings_expr ~failure_blobs ~body_fn
;;

(** The [hegel_test] extension is attached to [structure_item] (top-level
    [let%hegel_test]). It supports only the non-recursive single-binding
    form. The expander splices in two top-level items: the test function
    itself and a [Hegel_test_runtime.register] side effect so that
    [dune runtest] (via the [ppx_hegel_test] inline-tests backend) discovers
    and runs the test. *)
let extension =
  Extension.declare_inline
    "hegel_test"
    Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive (__ ^:: nil) ^:: nil))
    (fun ~loc ~path:_ vb -> expand_value_binding ~loc vb)
;;

let () =
  Driver.register_transformation
    "ppx_hegel_test"
    ~rules:[ Context_free.Rule.extension extension ]
;;
