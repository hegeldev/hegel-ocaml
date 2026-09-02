(** Jane Street [Core] companion library for Hegel ([hegel.jane]).

    It requires the [core] and [sexp_diff] opam packages. Each typed generator 
    draws through the corresponding [hegel] generator and converts the result.

    To use [hegel.jane], add [hegel.jane] and [core] to your dune [libraries]:

    {[
      (test
       (name my_tests)
       (libraries hegel hegel.jane core alcotest)
       (preprocess (pps ppx_hegel_test)))
    ]}

    Then draw [Core] values directly:

    {[
      open Hegel

      (* Optional: show a structural [sexp_diff] on [require_equal] failures. *)
      let () = Hegel_jane.set_sexp_diff ()

      let%hegel_test dates_are_in_range tc =
        let d = draw tc (Hegel_jane.dates ()) in
        assert (Core.Date.year d >= 1 && Core.Date.year d <= 9999)
      ;;
    ]} *)

(** [dates ?min_date ?max_date ()] creates a generator for [Core.Date.t]
    values in [\[min_date, max_date\]]. The default range is [\[0001-01-01, 9999-12-31\]]. *)
val dates
  :  ?min_date:Core.Date.t
  -> ?max_date:Core.Date.t
  -> unit
  -> (Core.Date.t, Hegel.printable) Hegel.generator

(** [ofdays ?min_ofday ?max_ofday ()] creates a generator for
    [Core.Time_ns.Ofday.t] values in [\[min_ofday, max_ofday\]].
     The default range is [\[00:00:00.000000000, 23:59:59.999999999\]]. *)
val ofdays
  :  ?min_ofday:Core.Time_ns.Ofday.t
  -> ?max_ofday:Core.Time_ns.Ofday.t
  -> unit
  -> (Core.Time_ns.Ofday.t, Hegel.printable) Hegel.generator

(** [datetimes ?min_datetime ?max_datetime ()] creates a generator for naive
    datetimes as [(Core.Date.t * Core.Time_ns.Ofday.t)] pairs in
    [\[min_datetime, max_datetime\]] The default range is [\[0001-01-01T00:00:00.000000000, 9999-12-31T23:59:59.999999999\]]. *)
val datetimes
  :  ?min_datetime:Core.Date.t * Core.Time_ns.Ofday.t
  -> ?max_datetime:Core.Date.t * Core.Time_ns.Ofday.t
  -> unit
  -> (Core.Date.t * Core.Time_ns.Ofday.t, Hegel.printable) Hegel.generator

(** [chars ()] creates a generator for single characters (codepoints 0-255,
    i.e. Latin-1) as [Core.Char.t] values. *)
val chars : unit -> (Core.Char.t, Hegel.printable) Hegel.generator

(** [time_nanoseconds ?min_time ?max_time ()] creates a generator for
    [Core.Time_ns.t] values in [\[min_time, max_time\]]. The default range is
    the full representable range: [\[1823-11-12T00:06:21.572612096Z, 2116-02-20T23:53:38.427387903Z\]]. *)
val time_nanoseconds
  :  ?min_time:Core.Time_ns.t
  -> ?max_time:Core.Time_ns.t
  -> unit
  -> (Core.Time_ns.t, Hegel.printable) Hegel.generator

(** [time_nanosecond_spans ?min_span ?max_span ()] creates a generator for
    [Core.Time_ns.Span.t] values in [\[min_span, max_span\]]. The default range
    is the full representable range: [\[-53375d23h53m38.427387904s, 53375d23h53m38.427387903s\]]. *)
val time_nanosecond_spans
  :  ?min_span:Core.Time_ns.Span.t
  -> ?max_span:Core.Time_ns.Span.t
  -> unit
  -> (Core.Time_ns.Span.t, Hegel.printable) Hegel.generator

(** [hash_tables keys values ?min_size ?max_size ()] creates a generator for
    polymorphic [Core.Hashtbl.t] tables over printable [keys] and [values].
    [Hegel.make_hash_tables] closed over [Core.Hashtbl]. *)
val hash_tables
  :  ('a, Hegel.printable) Hegel.generator
  -> ('b, Hegel.printable) Hegel.generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> (('a, 'b) Core.Hashtbl.Poly.t, Hegel.printable) Hegel.generator

(** [resolve_draw values ~consume id] resolves a drawn pool [id] against the
    local [values] table, removing it when [consume]. *)
val resolve_draw : (int, 'a) Core.Hashtbl.t -> consume:bool -> int -> 'a

(** [pool_values ~pool ~values ~consume] builds a generator that picks a
    value from the engine pool [pool], resolving the drawn id against the
    local [values] table. When [consume], the picked value is removed from the
    pool. *)
val pool_values
  :  pool:Hegel.Internal.pool
  -> values:(int, 'a) Core.Hashtbl.t
  -> consume:bool
  -> ('a, Hegel.unprintable) Hegel.generator

(**/**)

(** [sexp_diff_renderer ~colored ~original ~updated] renders a structural
    [sexp_diff] two-column diff of the two values: red/green markings when
    [colored], [-]/[+] otherwise. The renderer {!set_sexp_diff} installs;
    doc-hidden — use {!set_sexp_diff}. *)
val sexp_diff_renderer
  :  colored:bool
  -> original:Sexplib0.Sexp.t
  -> updated:Sexplib0.Sexp.t
  -> string

(**/**)

(** [set_sexp_diff ()] makes [Hegel.require_equal] failures print a
    structural [sexp_diff] diff instead of the default both-values rendering.
    Call it once. It stays installed for the rest of the process. *)
val set_sexp_diff : unit -> unit

(** Auxiliary submodule for [@@deriving hegel_generator] for
    [Core] generators.

    This module enables the generator deriver to use generators for [Core] types
    such as [Date.t] and [Time_ns.Span.t]. [open Hegel_jane.Derive] is required
    for [@@deriving hegel_generator] to use generators for [Core] types.

    {[
      open! Core
      open Hegel_jane.Derive

      type event =
        { id : int
        ; day : Date.t
        ; elapsed : Time_ns.Span.t
        }
      [@@deriving hegel_generator]
    ]}

    Use [Core]-typed fields with their short paths (for example, [Date.t] instead 
    of [Core.Date.t]).

    {[
      type good = { day : Date.t } [@@deriving hegel_generator]

      (* Does not compile: [Core.Date] has no [hegel_generator]. *)
      type bad = { day : Core.Date.t } [@@deriving hegel_generator]
    ]}

    To keep a [Core]-qualified type, set its generator directly:

    {[
      type pinned =
        { day : (Core.Date.t[@hegel.generator Hegel_jane.dates ()]) }
      [@@deriving hegel_generator]
    ]} *)
module Derive : sig
  (**/**)

  val hegel_generator_int : (int, Hegel.printable) Hegel.generator
  val hegel_generator_bool : (bool, Hegel.printable) Hegel.generator
  val hegel_generator_float : (float, Hegel.printable) Hegel.generator
  val hegel_generator_string : (string, Hegel.printable) Hegel.generator
  val hegel_generator_char : (char, Hegel.printable) Hegel.generator

  val hegel_generator_list
    :  ('a, Hegel.printable) Hegel.generator
    -> ('a list, Hegel.printable) Hegel.generator

  val hegel_generator_option
    :  ('a, Hegel.printable) Hegel.generator
    -> ('a option, Hegel.printable) Hegel.generator

  val sexp_of_int : int -> Sexplib0.Sexp.t
  val sexp_of_bool : bool -> Sexplib0.Sexp.t
  val sexp_of_float : float -> Sexplib0.Sexp.t
  val sexp_of_string : string -> Sexplib0.Sexp.t
  val sexp_of_char : char -> Sexplib0.Sexp.t
  val sexp_of_list : ('a -> Sexplib0.Sexp.t) -> 'a list -> Sexplib0.Sexp.t
  val sexp_of_option : ('a -> Sexplib0.Sexp.t) -> 'a option -> Sexplib0.Sexp.t

  module Date : sig
    include module type of struct
      include Core.Date
    end

    val hegel_generator : (t, Hegel.printable) Hegel.generator
  end

  module Time_ns : sig
    include module type of struct
        include Core.Time_ns
      end
      with module Span := Core.Time_ns.Span

    module Span : sig
      include module type of struct
        include Core.Time_ns.Span
      end

      val hegel_generator : (t, Hegel.printable) Hegel.generator
    end

    val hegel_generator : (t, Hegel.printable) Hegel.generator
  end

  (**/**)
end
