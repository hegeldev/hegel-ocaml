type outcome = (exn * Printexc.raw_backtrace) option
type t = { spawn_join_n : n:int -> f:(int -> outcome) -> outcome list }

(* we use an outcome list because arrays are contended in OxCaml *)
let threads =
  { spawn_join_n =
      (fun ~n ~f ->
        let outcomes = Array.make n None in
        let run i =
          outcomes.(i)
          <- (try f i with
              | exn -> Some (exn, Printexc.get_raw_backtrace ()))
        in
        let workers = List.init n (fun i -> Thread.create run i) in
        List.iter Thread.join workers;
        Array.to_list outcomes)
  }
;;
