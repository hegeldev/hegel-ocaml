(** Scope-resolved generator names for [@@deriving hegel_generator]. *)

let hegel_generator_int = Generators.integers ()
let hegel_generator_bool = Generators.booleans ()
let hegel_generator_float = Generators.floats ~allow_nan:false ~allow_infinity:false ()
let hegel_generator_string = Generators.text ()
let hegel_generator_char = Generators.char ()
let hegel_generator_list g = Generators.lists g ()
let hegel_generator_option g = Generators.optional g
