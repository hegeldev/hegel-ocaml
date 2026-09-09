RELEASE_TYPE: patch

This patch adds support for providing the location of a printed draw. `Hegel.draw` now takes an optional `loc : Lexing.position`. 
The OxCaml compiler automatically provides the location, but the standard OCaml compiler requires manually creating 
it or using `[%here]` from `ppx_here`. The location of a printed draw is displayed as `<draw> @ <filename>:<line_num>`.

The OxCaml warning `Alert unsafe_multidomain: Stdlib.Domain.DLS` has also been resolved.