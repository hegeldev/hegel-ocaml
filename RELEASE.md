RELEASE_TYPE: patch

This patch adds support for providing the location of a printed draw. `Hegel.draw` now accepts a `loc : Lexing.position` argument.
The OxCaml compiler automatically provides the location when omitted. On standard OCaml, the argument is optional.
A draw with a location is displayed as `name @ filename:line = value`.

The OxCaml warning `Alert unsafe_multidomain: Stdlib.Domain.DLS` has also been resolved.
