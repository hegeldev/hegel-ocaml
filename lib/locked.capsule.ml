(* capsule0 provides Capsule_blocking_sync for libraries that cannot depend on Await *)
[@@@alert "-deprecated"]

type 'a t : value mod portable contended =
  | Locked : 'k Capsule_blocking_sync.Mutex.t * ('a, 'k) Capsule_prim.Data.t -> 'a t

let create (init @ portable) =
  let (P key) = Capsule_prim.create () in
  Locked (Capsule_blocking_sync.Mutex.create key, Capsule_prim.Data.create init)
;;

let protect (type (b : value mod contended portable)) (Locked (mutex, data)) f : b =
  let cell : (b, exn * Printexc.raw_backtrace) result option Atomic.t = Atomic.make None in
  Capsule_blocking_sync.Mutex.with_lock mutex ~f:(fun password ->
    Capsule_prim.Data.iter
      ~password
      ~f:(fun v ->
        Atomic.set
          cell
          (Some
             (match f v with
              | result -> Ok result
              | exception exn -> Error (exn, Printexc.get_raw_backtrace ()))))
      data);
  match Option.get (Atomic.get cell) with
  | Ok result -> result
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace
;;
