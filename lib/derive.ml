(** Scope-resolved generator names for [@@deriving hegel_generator]. *)

let hegel_generator_int = Generators.integers ()
let hegel_generator_bool = Generators.booleans ()
let hegel_generator_float = Generators.floats ~allow_nan:false ~allow_infinity:false ()
let hegel_generator_string = Generators.text ()
let hegel_generator_char = Generators.char ()
let hegel_generator_list g = Generators.lists g ()
let hegel_generator_option g = Generators.optional g
let sexp_of_int = Sexplib0.Sexp_conv.sexp_of_int
let sexp_of_bool = Sexplib0.Sexp_conv.sexp_of_bool
let sexp_of_float = Sexplib0.Sexp_conv.sexp_of_float
let sexp_of_string = Sexplib0.Sexp_conv.sexp_of_string
let sexp_of_char = Sexplib0.Sexp_conv.sexp_of_char
let sexp_of_list = Sexplib0.Sexp_conv.sexp_of_list
let sexp_of_option = Sexplib0.Sexp_conv.sexp_of_option
