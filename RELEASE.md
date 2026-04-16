RELEASE_TYPE: minor

Add support for building and running on the OxCaml compiler (`ocaml-variants.5.2.0+ox`). The `ppx_hegel_generator` PPX deriver now works on both standard OCaml and OxCaml by abstracting over ppxlib AST differences (labeled tuples, constructor modalities) via a platform-selected compatibility module.
