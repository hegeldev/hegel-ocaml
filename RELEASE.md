RELEASE_TYPE: minor

This release reexports many functions from the `Client` module to the `Hegel` module.
The `Client` module has been renamed to `Internal`. Hegel tests should only use the 
`Hegel`, `Hegel.Generators`, and `Hegel.Stateful` modules.