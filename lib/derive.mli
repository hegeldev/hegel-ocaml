(** Generated code refers to generators by name. The type [foo] resolves to
    [hegel_generator_foo], and the type [t] in a module [M] resolves to
    [M.hegel_generator].

    [open Hegel] is required in each file that uses [@@deriving hegel_generator]:

    {[
      open Hegel

      type point =
        { x : int
        ; y : int
        }
      [@@deriving hegel_generator]
    ]}

    A different module in scope can shadow these names to change the
    defaults. For example, [Hegel_jane.Derive] resolves the same names to
    [Core]-typed generators. *)

(**/**)

val hegel_generator_int : (int, Generators.printable) Generators.generator
val hegel_generator_bool : (bool, Generators.printable) Generators.generator
val hegel_generator_float : (float, Generators.printable) Generators.generator
val hegel_generator_string : (string, Generators.printable) Generators.generator
val hegel_generator_char : (char, Generators.printable) Generators.generator

val hegel_generator_list
  :  ('a, Generators.printable) Generators.generator
  -> ('a list, Generators.printable) Generators.generator

val hegel_generator_option
  :  ('a, Generators.printable) Generators.generator
  -> ('a option, Generators.printable) Generators.generator

val sexp_of_int : int -> Sexplib0.Sexp.t
val sexp_of_bool : bool -> Sexplib0.Sexp.t
val sexp_of_float : float -> Sexplib0.Sexp.t
val sexp_of_string : string -> Sexplib0.Sexp.t
val sexp_of_char : char -> Sexplib0.Sexp.t
val sexp_of_list : ('a -> Sexplib0.Sexp.t) -> 'a list -> Sexplib0.Sexp.t
val sexp_of_option : ('a -> Sexplib0.Sexp.t) -> 'a option -> Sexplib0.Sexp.t

(**/**)
