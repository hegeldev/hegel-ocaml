RELEASE_TYPE: minor

This release reexports many functions from the `Internal` module to the `Hegel` module.
Hegel tests should only use the `Hegel`, `Hegel.Generators`, and `Hegel.Stateful` 
modules.