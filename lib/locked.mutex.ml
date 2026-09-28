(* Locked, for compilers without capsules: a Mutex around the value. *)

type 'a t =
  { lock : Mutex.t
  ; value : 'a
  }

let create init = { lock = Mutex.create (); value = init () }
let protect t f = Mutex.protect t.lock (fun () -> f t.value)

module Nonportable = struct
  type nonrec 'a t = 'a t

  let create = create
  let protect = protect
end
