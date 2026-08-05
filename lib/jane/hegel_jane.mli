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
