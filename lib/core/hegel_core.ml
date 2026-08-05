(** Core companion library for Hegel. See [hegel_core.mli]. *)

open! Core
module G = Hegel.Generators

let hash_tables keys values ?(min_size = 0) ?max_size () =
  G.hash_tables_core
    ~of_pairs:Hashtbl.Poly.of_alist_exn
    ~sexp_of_t:Hashtbl.Poly.sexp_of_t
    keys
    values
    ~min_size
    ?max_size
    ()
;;

let resolve_draw values ~consume variable_id =
  G.Ppx_internal.resolve_draw_core
    ~find:(Hashtbl.find values)
    ~remove:(Hashtbl.remove values)
    ~consume
    variable_id
;;

let pool_values ~pool_id ~values ~consume =
  G.Ppx_internal.values_core
    ~pool_id
    ~find:(Hashtbl.find values)
    ~remove:(Hashtbl.remove values)
    ~is_empty:(fun () -> Hashtbl.is_empty values)
    ~consume
;;

let sexp_diff_renderer ~colored ~original ~updated =
  let diff = Sexp_diff.Algo.diff ~original ~updated () in
  let display_options = Sexp_diff.Display.Display_options.create Two_column in
  if colored
  then Sexp_diff.Display.display_with_ansi_colors display_options diff
  else Sexp_diff.Display.display_as_plain_string display_options diff
;;

let set_sexp_diff () = Hegel.Internal.set_diff_renderer (Some sexp_diff_renderer)
