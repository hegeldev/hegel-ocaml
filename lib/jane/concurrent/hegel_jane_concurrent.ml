type 'ctx ctx =
  { context : 'ctx
  ; concurrent : 'ctx Concurrent.t
  }

let of_concurrent (c @ local) = exclave_
  { Hegel.Concurrency.spawn_join_n =
      (fun ~n ~f ->
        Concurrent.spawn_join_n c () ~n ~f:(fun _scope context concurrent i ->
          f { context; concurrent } i [@nontail])
        |> Base.Iarray.to_list)
  }
;;
