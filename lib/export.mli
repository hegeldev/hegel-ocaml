(** Scope-resolved generator names for [@@deriving hegel_generator].

    Code that the deriver generates refers to generators by name: the type
    [foo] resolves to [hegel_generator_foo], the type [t] in a module [M]
    resolves to [M.hegel_generator], and a parameterized type applies the
    arguments. This module supplies those names for the built-in types. 
    Open it in each file that uses [@@deriving hegel_generator]:

    {[
      open Hegel.Export

      type point =
        { x : int
        ; y : int
        }
      [@@deriving hegel_generator]
    ]}

    A different module in scope can shadow these names to change the
    defaults. For example, [Hegel_jane.Export] resolves the same names to
    [Core]-typed generators. *)

(** [hegel_generator_int] generates integers over the full native [int]
    range, as [Generators.integers ()]. *)
val hegel_generator_int : (int, Generators.printable) Generators.generator

(** [hegel_generator_bool] generates booleans, as [Generators.booleans ()]. *)
val hegel_generator_bool : (bool, Generators.printable) Generators.generator

(** [hegel_generator_float] generates finite floats (no NaN, no infinity). *)
val hegel_generator_float : (float, Generators.printable) Generators.generator

(** [hegel_generator_string] generates text strings, as [Generators.text ()]. *)
val hegel_generator_string : (string, Generators.printable) Generators.generator

(** [hegel_generator_char] generates Latin-1 characters, as
    [Generators.char ()]. *)
val hegel_generator_char : (char, Generators.printable) Generators.generator

(** [hegel_generator_list g] generates lists with elements from [g], as
    [Generators.lists g ()]. *)
val hegel_generator_list
  :  ('a, Generators.printable) Generators.generator
  -> ('a list, Generators.printable) Generators.generator

(** [hegel_generator_option g] generates [None] or [Some] values from [g],
    as [Generators.optional g]. *)
val hegel_generator_option
  :  ('a, Generators.printable) Generators.generator
  -> ('a option, Generators.printable) Generators.generator

(** {2 Sexp converters}

    The deriver also emits [sexp_of_<t>] definitions (via ppx_sexp_conv),
    whose generated code refers to the built-in converters by these
    unqualified names. Each is the [Sexplib0.Sexp_conv] converter. *)

val sexp_of_int : int -> Sexplib0.Sexp.t
val sexp_of_bool : bool -> Sexplib0.Sexp.t
val sexp_of_float : float -> Sexplib0.Sexp.t
val sexp_of_string : string -> Sexplib0.Sexp.t
val sexp_of_char : char -> Sexplib0.Sexp.t
val sexp_of_list : ('a -> Sexplib0.Sexp.t) -> 'a list -> Sexplib0.Sexp.t
val sexp_of_option : ('a -> Sexplib0.Sexp.t) -> 'a option -> Sexplib0.Sexp.t
