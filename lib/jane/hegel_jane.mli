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

(** [dates ()] creates a generator for [Core.Date.t] values, with year in
    [\[1, 9999\]] and calendar-valid month/day. *)
val dates : unit -> (Core.Date.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [times ()] creates a generator for [Core.Time_ns.Ofday.t] times of day
    with microsecond precision. *)
val times
  :  unit
  -> (Core.Time_ns.Ofday.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [datetimes ()] creates a generator for naive datetimes as
    [(Core.Date.t * Core.Time_ns.Ofday.t)] pairs. *)
val datetimes
  :  unit
  -> ( Core.Date.t * Core.Time_ns.Ofday.t
       , Hegel.Generators.printable )
       Hegel.Generators.generator

(** [char ()] creates a generator for single characters (codepoints 0-255,
    i.e. Latin-1) as [Core.Char.t] values. *)
val char : unit -> (Core.Char.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [time_ns ()] creates a generator for [Core.Time_ns.t] values. *)
val time_ns
  :  unit
  -> (Core.Time_ns.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [time_ns_spans ()] creates a generator for [Core.Time_ns.Span.t] values. *)
val time_ns_spans
  :  unit
  -> (Core.Time_ns.Span.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [hash_tables keys values ?min_size ?max_size ()] creates a generator for
    polymorphic [Core.Hashtbl.t] tables over printable [keys] and [values].
    [Hegel.Generators.make_hash_tables] closed over [Core.Hashtbl]. *)
val hash_tables
  :  ('a, Hegel.Generators.printable) Hegel.Generators.generator
  -> ('b, Hegel.Generators.printable) Hegel.Generators.generator
  -> ?min_size:int
  -> ?max_size:int
  -> unit
  -> (('a, 'b) Core.Hashtbl.Poly.t, Hegel.Generators.printable) Hegel.Generators.generator

(** [resolve_draw values ~consume id] resolves a drawn pool [id] against the
    local [values] table, removing it when [consume]. *)
val resolve_draw : (int, 'a) Core.Hashtbl.t -> consume:bool -> int -> 'a

(** [pool_values ~pool_id ~values ~consume] builds a generator that picks a
    value from the engine pool [pool_id], resolving the drawn id against the
    local [values] table. When [consume], the picked value is removed from the
    pool. *)
val pool_values
  :  pool_id:int
  -> values:(int, 'a) Core.Hashtbl.t
  -> consume:bool
  -> ('a, Hegel.Generators.unprintable) Hegel.Generators.generator

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

(** Scope-resolved generator names for [@@deriving hegel_generator] for
    [Core] generators. [open Hegel_jane.Derive] is required for 
    [@@deriving hegel_generator] to use generators for [Core] types.

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

    This module enables the generator deriver to use generators for [Core] types
    such as [Date.t] and [Time_ns.Span.t].

    Open [Core] and write [Core]-typed fields with their short paths (for example, 
    [Date.t] instead of [Core.Date.t]).

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

  val hegel_generator_int : (int, Hegel.Generators.printable) Hegel.Generators.generator
  val hegel_generator_bool : (bool, Hegel.Generators.printable) Hegel.Generators.generator

  val hegel_generator_float
    : (float, Hegel.Generators.printable) Hegel.Generators.generator

  val hegel_generator_string
    : (string, Hegel.Generators.printable) Hegel.Generators.generator

  val hegel_generator_char : (char, Hegel.Generators.printable) Hegel.Generators.generator

  val hegel_generator_list
    :  ('a, Hegel.Generators.printable) Hegel.Generators.generator
    -> ('a list, Hegel.Generators.printable) Hegel.Generators.generator

  val hegel_generator_option
    :  ('a, Hegel.Generators.printable) Hegel.Generators.generator
    -> ('a option, Hegel.Generators.printable) Hegel.Generators.generator

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

    val hegel_generator : (t, Hegel.Generators.printable) Hegel.Generators.generator
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

      val hegel_generator : (t, Hegel.Generators.printable) Hegel.Generators.generator
    end

    val hegel_generator : (t, Hegel.Generators.printable) Hegel.Generators.generator
  end

  (**/**)
end
