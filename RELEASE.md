RELEASE_TYPE: minor

This release improves the ergonomics of the `[@@deriving hegel_generator]`
PPX deriver and adds more derivable types.

Breaking changes:

- Derived generators have new names. The type `t` now derives `hegel_generator`.
  Any other type `foo` derives `hegel_generator_foo`.
  A generator for the type `t` in a module `M` derives `M.hegel_generator`.
- Files that use the deriver now require an open `Hegel` in scope.
- Derived generators are now printable. A derived generator containing a field 
  whose type has no `sexp_of_*` converter in scope no longer compiles unless the 
  it is annotated with `[@sexp.opaque]`.

New features:

- `char` and inline-record constructors (`C of { ... }`) now derive.
- The `[@hegel.generator expr]` attribute replaces the generator for a
  type occurrence, such as a record field.
- The `[@hegel.do_not_generate]` attribute excludes a variant constructor
  from generation.
- The `Hegel_jane.Derive` module in `hegel.jane` allows the deriver to use
  Core-typed generators.
