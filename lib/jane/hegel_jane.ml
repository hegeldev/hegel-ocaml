(** Core companion library for Hegel. See [hegel_jane.mli]. *)

open! Core
module G = Hegel.Generators

let first_date = Date.create_exn ~y:1 ~m:Jan ~d:1
let last_date = Date.create_exn ~y:9999 ~m:Dec ~d:31
let first_ofday = Time_ns.Ofday.start_of_day
let last_ofday = Time_ns.Ofday.approximate_end_of_day

let to_hegel_date d : G.date =
  { year = Date.year d; month = Month.to_int (Date.month d); day = Date.day d }
;;

let to_core_date ({ year; month; day } : G.date) =
  Date.create_exn ~y:year ~m:(Month.of_int_exn month) ~d:day
;;

let to_hegel_time t : G.time =
  let ({ hr; min; sec; ms; us; ns; sign = _ } : Time_ns.Span.Parts.t) =
    Time_ns.Ofday.to_parts t
  in
  { hour = hr
  ; minute = min
  ; second = sec
  ; nanosecond = (ms * 1_000_000) + (us * 1_000) + ns
  }
;;

let to_ofday ({ hour; minute; second; nanosecond } : G.time) =
  Time_ns.Ofday.create ~hr:hour ~min:minute ~sec:second ~ns:nanosecond ()
;;

let dates ?(min_date = first_date) ?(max_date = last_date) () =
  G.make_dates
    ~of_date:to_core_date
    ~sexp_of:Date.sexp_of_t
    ~min_date:(to_hegel_date min_date)
    ~max_date:(to_hegel_date max_date)
    ()
;;

let ofdays ?(min_ofday = first_ofday) ?(max_ofday = last_ofday) () =
  G.make_times
    ~of_time:to_ofday
    ~sexp_of:Time_ns.Ofday.sexp_of_t
    ~min_time:(to_hegel_time min_ofday)
    ~max_time:(to_hegel_time max_ofday)
    ()
;;

let datetimes
      ?(min_datetime = first_date, first_ofday)
      ?(max_datetime = last_date, last_ofday)
      ()
  =
  let parts (date, time) = to_hegel_date date, to_hegel_time time in
  G.make_datetimes
    ~of_datetime:(fun (date, time) -> to_core_date date, to_ofday time)
    ~sexp_of:(fun (date, time) ->
      Sexp.List [ Date.sexp_of_t date; Time_ns.Ofday.sexp_of_t time ])
    ~min_datetime:(parts min_datetime)
    ~max_datetime:(parts max_datetime)
    ()
;;

let chars () = G.make_characters ~of_char:Fun.id ~sexp_of:Char.sexp_of_t ()

let time_nanosecond_spans
      ?(min_span = Time_ns.Span.min_value_representable)
      ?(max_span = Time_ns.Span.max_value_representable)
      ()
  =
  let ns_span_gen =
    G.integers
      ~min_value:(Time_ns.Span.to_int_ns min_span)
      ~max_value:(Time_ns.Span.to_int_ns max_span)
      ()
  in
  G.with_printer
    Time_ns.Span.sexp_of_t
    (G.composite (fun tc -> Time_ns.Span.of_int_ns (Hegel.draw_silent tc ns_span_gen)))
;;

let time_nanoseconds
      ?(min_time = Time_ns.min_value_representable)
      ?(max_time = Time_ns.max_value_representable)
      ()
  =
  let ns_gen =
    G.integers
      ~min_value:(Time_ns.to_int_ns_since_epoch min_time)
      ~max_value:(Time_ns.to_int_ns_since_epoch max_time)
      ()
  in
  G.with_printer
    Time_ns.Alternate_sexp.sexp_of_t
    (G.composite (fun tc -> Time_ns.of_int_ns_since_epoch (Hegel.draw_silent tc ns_gen)))
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

let pool_values ~pool ~values ~consume =
  G.Ppx_internal.make_pool_values
    ~pool
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

module Derive = struct
  include Hegel.Derive

  let hegel_generator_char = chars ()
  let sexp_of_char = Char.sexp_of_t

  module Date = struct
    include Date

    let hegel_generator = dates ()
  end

  module Time_ns = struct
    include Time_ns

    let hegel_generator = time_nanoseconds ()

    module Span = struct
      include Span

      let hegel_generator = time_nanosecond_spans ()
    end
  end
end
