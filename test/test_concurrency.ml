module C = Hegel.Concurrency

let threads_preserves_order () =
  let outcomes =
    C.threads.spawn_join_n ~n:5 ~f:(fun i ->
      Some (Failure (string_of_int i), Printexc.get_raw_backtrace ()))
  in
  Alcotest.(check int) "one outcome per body" 5 (List.length outcomes);
  List.iteri
    (fun i outcome ->
       match outcome with
       | Some (Failure msg, _) ->
         Alcotest.(check string) "odd index failed with its index" (string_of_int i) msg
       | _ ->
         Alcotest.failf "unexpected or no exception")
    outcomes
;;

let threads_records_raising_body () =
  let outcomes =
    C.threads.spawn_join_n ~n:2 ~f:(fun i -> if i = 1 then failwith "boom" else None)
  in
  match outcomes with
  | [ None; Some (Failure msg, _) ] -> Alcotest.(check string) "message" "boom" msg
  | _ -> Alcotest.fail "expected the raising body's exception at index 1"
;;

let threads_zero_bodies () =
  Alcotest.(check int)
    "no outcomes"
    0
    (List.length (C.threads.spawn_join_n ~n:0 ~f:(fun _ -> None)))
;;

let tests =
  [ Alcotest.test_case "threads preserves order" `Quick threads_preserves_order
  ; Alcotest.test_case "threads records raising body" `Quick threads_records_raising_body
  ; Alcotest.test_case "threads zero bodies" `Quick threads_zero_bodies
  ]
;;
