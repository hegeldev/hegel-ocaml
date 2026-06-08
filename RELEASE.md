RELEASE_TYPE: minor

This release replaces the hegel-core Python engine with the `libhegel` Rust engine, 
called in-process via ctypes. There is no longer a subprocess, socket,
or wire protocol. `libhegel` is located (or downloaded) at runtime and `dlopen`ed.

The breaking change is the removal of the `Hegel.Protocol`, `Hegel.Connection`, and
`Hegel.Session` modules. 
