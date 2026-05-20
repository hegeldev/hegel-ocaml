let () =
  let mode = if Array.length Sys.argv > 1 then Sys.argv.(1) else "pass" in
  let run =
    match mode with
    | "fail" -> fun () -> failwith "deliberate"
    | _ -> fun () -> ()
  in
  Hegel_test_runtime.register ~name:"demo" ~file:__FILE__ ~line:__LINE__ run;
  Hegel_test_runtime.test_main ()
;;
