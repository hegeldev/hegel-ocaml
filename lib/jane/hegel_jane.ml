(** Core companion library for Hegel. See [hegel_jane.mli]. *)

open! Core
module G = Hegel.Generators

let dates () =
  G.make_dates
    ~of_parts:(fun ~year ~month ~day ->
      Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day)
    ~sexp_of:Date.sexp_of_t
    ()
;;

let times () =
  G.make_times
    ~of_parts:(fun ~hour ~minute ~second ~microsecond ->
      Time_ns.Ofday.create ~hr:hour ~min:minute ~sec:second ~us:microsecond ())
    ~sexp_of:Time_ns.Ofday.sexp_of_t
    ()
;;

let datetimes () =
  G.make_datetimes
    ~of_parts:(fun ~year ~month ~day ~hour ~minute ~second ~microsecond ->
      ( Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day
      , Time_ns.Ofday.create ~hr:hour ~min:minute ~sec:second ~us:microsecond () ))
    ~sexp_of:(fun (date, time) ->
      Sexp.List [ Date.sexp_of_t date; Time_ns.Ofday.sexp_of_t time ])
    ()
;;

let char () = G.make_characters ~of_char:Fun.id ~sexp_of:Char.sexp_of_t ()

let ns_draw tc =
  Int63.of_int
    (Hegel.Internal.generate_integer tc ~min_value:Int.min_value ~max_value:Int.max_value)
;;

let time_ns_spans () =
  G.leaf
    ~draw:(fun tc -> Time_ns.Span.of_int63_ns (ns_draw tc))
    ~sexp_of:Time_ns.Span.sexp_of_t
;;

let time_ns () =
  G.leaf
    ~draw:(fun tc -> Time_ns.of_int63_ns_since_epoch (ns_draw tc))
    ~sexp_of:Time_ns.Alternate_sexp.sexp_of_t
;;

let hash_tables keys values ?min_size ?max_size () =
  G.make_hash_tables
    ~of_pairs:Hashtbl.Poly.of_alist_exn
    ~sexp_of_t:Hashtbl.Poly.sexp_of_t
    keys
    values
    ?min_size
    ?max_size
    ()
;;

let resolve_draw values ~consume variable_id =
  G.Ppx_internal.resolve_pool_draw
    ~find:(Hashtbl.find values)
    ~remove:(Hashtbl.remove values)
    ~consume
    variable_id
;;

let pool_values ~pool_id ~values ~consume =
  G.Ppx_internal.make_pool_values
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

module Export = struct
  include Hegel.Export

  let hegel_generator_char = char ()
  let sexp_of_char = Char.sexp_of_t

  module Date = struct
    include Date

    let hegel_generator = dates ()
  end

  module Time_ns = struct
    include Time_ns

    let hegel_generator = time_ns ()

    module Span = struct
      include Span

      let hegel_generator = time_ns_spans ()
    end
  end
end
