(** Jane Street [Core] companion library for Hegel ([hegel.core]).

    It requires the [core] and [sexp_diff] opam packages.

    Each typed generator draws through the corresponding [hegel] generator and 
    converts the result. *)

(** [hash_tables keys values ?min_size ?max_size ()] creates a generator for
    polymorphic [Core.Hashtbl.t] tables over printable [keys] and [values].
    [Hegel.Generators.hash_tables_core] closed over [Core.Hashtbl]. *)
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

(** [sexp_diff_renderer ~colored ~original ~updated] renders a structural
    [sexp_diff] two-column diff of the two values: red/green markings when
    [colored], [-]/[+] otherwise. The renderer {!set_sexp_diff} installs. *)
val sexp_diff_renderer
  :  colored:bool
  -> original:Sexplib0.Sexp.t
  -> updated:Sexplib0.Sexp.t
  -> string

(** [set_sexp_diff ()] makes [Hegel.require_equal] failures print a
    structural [sexp_diff] diff instead of the default both-values rendering.
    Call it once. It stays installed for the rest of the process. *)
val set_sexp_diff : unit -> unit
