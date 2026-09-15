module Pool_gen = Generators.Make_pool (Generators.Int_table)

module Pool = struct
  type 'a t = 'a Pool_gen.t

  let create ?(clone = Fun.id) tc =
    Pool_gen.create tc ~clone ~lock:(Some (Mutex.create ()))
  ;;

  let add = Pool_gen.add
  let is_empty = Pool_gen.is_empty
  let size = Pool_gen.size
  let values_reusable t = Pool_gen.pool_values t ~consume:false
  let values_consumed t = Pool_gen.pool_values t ~consume:true
end

module Rule = struct
  type 'state t = 'state Stateful_seq.Rule.t

  let create ?(group = "<anonymous>") ~name ~step () =
    { (Stateful_seq.Rule.create ~name ~step:(fun tc state ->
         step tc state;
         state))
      with
      group
    }
  ;;

  let name = Stateful_seq.Rule.name
  let group t = t.Stateful_seq.Rule.group
end

let group_names names =
  List.fold_left
    (fun groups name -> if List.mem name groups then groups else name :: groups)
    []
    names
  |> List.rev
  |> Array.of_list
;;

let group_ids names group_names_uniq =
  List.map
    (fun name -> Array.find_index (String.equal name) group_names_uniq |> Option.get)
    names
;;

type round_control =
  { mutex : Mutex.t
  ; condition : Condition.t
  ; mutable round : int
  ; mutable completed : int
  ; mutable stopping : bool
  ; mutable cases : Internal.test_case array
  ; results : (unit, exn * Printexc.raw_backtrace) result array
  }

let worker_loop control ~worker_index ~run_round =
  let rec loop seen_round =
    let next_round =
      Mutex.protect control.mutex (fun () ->
        while (not control.stopping) && control.round = seen_round do
          Condition.wait control.condition control.mutex
        done;
        if control.stopping
        then None
        else Some (control.round, control.cases.(worker_index)))
    in
    match next_round with
    | None -> ()
    | Some (round, tc) ->
      let result =
        try
          ignore (run_round ~worker_index tc : _ * int * bool);
          Ok ()
        with
        | exn -> Error (exn, Printexc.get_raw_backtrace ())
      in
      Mutex.protect control.mutex (fun () ->
        control.results.(worker_index) <- result;
        control.completed <- control.completed + 1;
        Condition.broadcast control.condition);
      loop round
  in
  loop 0
;;

let dispatch_round control tc =
  (* Prepare every clone before waking workers. Setup exceptions propagate on
     the coordinator, so they cannot strand a partially dispatched round. *)
  let cases =
    Array.init (Array.length control.results) (fun worker_index ->
      let worker_tc = Internal.clone tc in
      Internal.set_worker_index worker_tc worker_index;
      worker_tc)
  in
  Mutex.protect control.mutex (fun () ->
    control.cases <- cases;
    control.completed <- 0;
    control.round <- control.round + 1;
    Condition.broadcast control.condition;
    while control.completed < Array.length control.results do
      Condition.wait control.condition control.mutex
    done;
    (* Every worker has replaced its result before the barrier opens. *)
    Array.copy control.results)
;;

let stop_workers control workers =
  Mutex.protect control.mutex (fun () ->
    control.stopping <- true;
    Condition.broadcast control.condition);
  List.iter (fun worker -> Internal.join worker) workers
;;

let reraise_worker_failure results =
  let failures =
    Array.to_list results
    |> List.filter_map (function
      | Ok () -> None
      | Error failure -> Some failure)
  in
  let find predicate = List.find_opt (fun (exn, _) -> predicate exn) failures in
  let is_overrun = function
    | Internal.Data_exhausted -> true
    | _ -> false
  in
  let is_invalid = function
    | Internal.Assume_rejected | Internal.Flaky_strategy -> true
    | _ -> false
  in
  (* Invalidated or exhausted rules can cause secondary failures in other
     workers. Prefer the engine's conclusion to those failures; usage/backend
     errors always take precedence. Within each category, worker order wins. *)
  let selected =
    List.find_map
      find
      [ Internal.is_control_exception; is_overrun; is_invalid; (fun _ -> true) ]
  in
  Option.iter
    (fun (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace)
    selected
;;

let run
      tc
      ~state_machine
      ~concurrency
      ~run_round
      ~group_names
      ~init
      ~print_state
      ~check_invariants
  =
  let control =
    { mutex = Mutex.create ()
    ; condition = Condition.create ()
    ; round = 0
    ; completed = 0
    ; stopping = false
    ; cases = [||]
    ; results = Array.make concurrency (Ok ())
    }
  in
  let rec start_workers worker_index workers =
    if worker_index = concurrency
    then workers
    else (
      match Internal.spawn tc (fun _ -> worker_loop control ~worker_index ~run_round) with
      | worker -> start_workers (worker_index + 1) (worker :: workers)
      | exception exn ->
        stop_workers control workers;
        raise exn)
  in
  let workers = start_workers 0 [] in
  Fun.protect
    ~finally:(fun () -> stop_workers control workers)
    (fun () ->
       let rec loop round =
         match Internal.state_machine_next_group tc ~state_machine with
         | None -> init
         | Some group ->
           Internal.note
             tc
             (Printf.sprintf
                "---------------- Round %d: group %S ----------------"
                round
                group_names.(group));
           dispatch_round control tc |> reraise_worker_failure;
           print_state init;
           check_invariants
             ~where:(Printf.sprintf "after round %d" round)
             ~sample:true
             init;
           loop (round + 1)
       in
       loop 1)
;;
