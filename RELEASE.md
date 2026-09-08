RELEASE_TYPE: minor

This release changes how invariants in stateful tests are created. The type of invariants is now `Invariant.t`.
Invariants are created with `create : name:string -> inv:('state -> unit) -> ?always_check:bool -> unit -> 'state t`.

Invariants previously were always sampled between steps, but the user can now set which invariants are always
run with `always_check` in `create`. Invariants are sampled by default.
