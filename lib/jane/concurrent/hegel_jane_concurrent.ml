let of_concurrent (c @ local) = exclave_
  { Hegel.Concurrency.spawn_join_n =
      (fun ~n ~f ->
        Concurrent.spawn_join_n c () ~n ~f:(fun _scope _ctx _concurrent i -> f i)
        |> Base.Iarray.to_list)
  }
;;
