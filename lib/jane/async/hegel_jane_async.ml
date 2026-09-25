open Async

let block_on ~execution_context body =
  try
    Thread_safe.block_on_async_exn (fun () ->
      let execution_context =
        Execution_context.create_like execution_context ~monitor:(Monitor.current ())
      in
      match Scheduler.within_context execution_context body with
      | Ok result -> result
      (* [within_context] already sent the exception to this monitor. *)
      | Error () -> Deferred.never ())
  with
  | exn -> raise (Monitor.extract_exn exn)
;;

module Async_io = struct
  type 'a t = 'a Deferred.t
  type body = unit -> unit t
  type wait = body -> unit

  let run_loop loop =
    let execution_context = Scheduler.current_execution_context () in
    In_thread.run (fun () -> loop (fun body -> block_on ~execution_context body))
  ;;
end

include Hegel.Make (Async_io)
