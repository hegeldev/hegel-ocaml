(** PPX expander for [let%hegel_test ...].

    Rewrites:
    {[
      let%hegel_test my_test tc = body
      [@@settings expr]
    ]}
    into:
    {[
      let my_test () =
        Hegel.Session.run_hegel_test
          ~settings:expr
          { function_name; file; begin_line }
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

    The [@@settings ...] attribute is optional. When omitted, the [~settings]
    argument is also omitted and [Hegel.Session.run_hegel_test] uses its default. *)

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

(** [build_items ~loc ~function_name ~settings_expr ~body_fn] returns the
    pair of structure items the expander splices in:

    {[
      let function_name () =
        Hegel.Session.run_hegel_test [?settings] location body_fn
      ;;

      let () =
        Hegel_test_runtime.register ~name:.. ~file:.. ~line:.. function_name
    ]} *)
let build_items ~loc ~function_name ~settings_expr ~body_fn : structure_item list =
  let location_record = build_location_record ~loc ~function_name in
  let call =
    match settings_expr with
    | Some s ->
      [%expr
        Hegel.Session.run_hegel_test ~settings:[%e s] [%e location_record] [%e body_fn]]
    | None -> [%expr Hegel.Session.run_hegel_test [%e location_record] [%e body_fn]]
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

(** Expander for a single [let%hegel_test ...] structure item. *)
let expand_value_binding ~loc (vb : value_binding) : structure_item list =
  let function_name = extract_function_name vb.pvb_pat in
  let settings_expr = extract_settings_attr vb.pvb_attributes in
  (* The body of [let%hegel_test name <args> = expr] is parsed as
     [let name = <args -> expr>]. We pass that lambda as the [test_fn] to
     [Hegel.Session.run_hegel_test]. *)
  build_items ~loc ~function_name ~settings_expr ~body_fn:vb.pvb_expr
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

let _rule = Context_free.Rule.extension extension
let () = Driver.register_transformation "ppx_hegel_test" ~rules:[ _rule ]
