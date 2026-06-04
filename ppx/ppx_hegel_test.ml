(** PPX expander for [let%hegel_test ...].

    Rewrites:
    {[
      let%hegel_test my_test tc = body
      [@@settings expr]
    ]}
    into:
    {[
      let my_test () =
        Hegel.run_hegel_test
          ~settings:expr
          ~test_location:{ function_name; file; begin_line }
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

    The [@@settings ...] attribute is optional. *)

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
let build_items ~loc ~function_name ~settings_expr ~body_fn : structure_item list =
  let location_record = build_location_record ~loc ~function_name in
  let base_call =
    match settings_expr with
    | Some s ->
      [%expr Hegel.run_hegel_test ~settings:[%e s] ~test_location:[%e location_record]]
    | None -> [%expr Hegel.run_hegel_test ~test_location:[%e location_record]]
  in
  let call = [%expr [%e base_call] [%e body_fn]] in
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

(** [is_draw_lident lid] is [true] when [lid] names the [draw] function, whether
    unqualified ([draw]) or qualified ([Hegel.draw], [Generators.draw], …). It
    deliberately excludes [draw_silent], which takes no [~label]. *)
let is_draw_lident : longident -> bool = function
  | Lident "draw" | Ldot (_, "draw") -> true
  | _ -> false
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

(** [inject_label vb] rewrites [let x = draw tc gen] into
    [let x = draw ~label:"x" tc gen] so the drawn value prints as [x = value] on
    a failing replay. It fires only when the bound pattern is a simple variable
    and the right-hand side is a [draw] application with no explicit [~label];
    every other binding is returned unchanged. *)
let inject_label (vb : value_binding) : value_binding =
  match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
  | ( Ppat_var { txt = name; _ }
    , Pexp_apply (({ pexp_desc = Pexp_ident { txt = lid; _ }; _ } as fn), args) )
    when is_draw_lident lid && not (has_label_arg args) ->
    let loc = vb.pvb_expr.pexp_loc in
    let label_arg = Labelled "label", Ast_builder.Default.estring ~loc name in
    { vb with pvb_expr = Ast_builder.Default.pexp_apply ~loc fn (label_arg :: args) }
  | _ -> vb
;;

(** A traversal that applies {!inject_label} to every [let]-binding in an
    expression, so labels are injected throughout the test body (nested [let]s,
    helper functions, match arms, …). Draws nested inside a span are still
    suppressed at runtime by the depth gate, so labeling them is harmless. *)
let label_injector =
  object
    inherit Ast_traverse.map as super

    method! expression e =
      let e = super#expression e in
      match e.pexp_desc with
      | Pexp_let (rec_flag, vbs, body) ->
        { e with pexp_desc = Pexp_let (rec_flag, List.map inject_label vbs, body) }
      | _ -> e
  end
;;

(** Expander for a single [let%hegel_test ...] structure item. *)
let expand_value_binding ~loc (vb : value_binding) : structure_item list =
  let function_name = extract_function_name vb.pvb_pat in
  let settings_expr = extract_settings_attr vb.pvb_attributes in
  (* The body of [let%hegel_test name <args> = expr] is parsed as
     [let name = <args -> expr>]. We pass that lambda as the [test_fn] to
     [Hegel.run_hegel_test], first injecting [~label]s drawn from the binding
     names so the counterexample replay prints [name = value]. *)
  let body_fn = label_injector#expression vb.pvb_expr in
  build_items ~loc ~function_name ~settings_expr ~body_fn
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
