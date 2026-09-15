(** Concurrent state-machine execution.

    This module starts [num_workers] threads and distributes rules to them

    - Ask libhegel for the next group.
    - Give each worker a clone of the current test case and wake all workers.
    - Each worker pulls and executes rules until libhegel ends its round,
      then records their result.
    - The main thread waits for every worker, propagates failures, and
      runs invariants after the workers complete a round. *)

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

(* get unique array of group names *)
let group_names names =
  List.fold_left
    (fun groups name -> if List.mem name groups then groups else name :: groups)
    []
    names
  |> List.rev
  |> Array.of_list
;;

(* map the group of the ith rule to the index of the group in the group name array *)
let group_ids names group_names_uniq =
  List.map
    (fun name -> Array.find_index (String.equal name) group_names_uniq |> Option.get)
    names
;;

type round_control =
  { mutex : Mutex.t
  ; condition : Condition.t
  ; mutable curr_round : int
    (** the current round number. each worker remembers the last round it handled. *)
  ; mutable num_completed : int
    (** number of workers that have published a result for the current round. *)
  ; mutable stop : bool (** shutdown signal. *)
  ; mutable cases : Internal.test_case array (** one test case clone per worker. *)
  ; results : (exn * Printexc.raw_backtrace) option array
    (** each worker's result after a round. [None] means success. *)
  }

let worker_loop control ~worker_index ~run_round =
  let rec loop last_exec_round =
    let next_round =
      Mutex.protect control.mutex (fun () ->
        while (not control.stop) && control.curr_round = last_exec_round do
          (* wait on main thread to finish preparing all workers *)
          Condition.wait control.condition control.mutex
        done;
        if control.stop
        then None
        else Some (control.curr_round, control.cases.(worker_index)))
    in
    match next_round with
    | None -> ()
    | Some (round, tc) ->
      let result =
        try
          ignore (run_round ~worker_index tc : _ * int * bool);
          None
        with
        | exn -> Some (exn, Printexc.get_raw_backtrace ())
      in
      Mutex.protect control.mutex (fun () ->
        control.results.(worker_index) <- result;
        control.num_completed <- control.num_completed + 1;
        (* final worker lets main thread know everyone is done *)
        if control.num_completed = Array.length control.results
        then Condition.broadcast control.condition);
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
    control.num_completed <- 0;
    control.curr_round <- control.curr_round + 1;
    Condition.broadcast control.condition;
    while control.num_completed < Array.length control.results do
      (* wait on all workers to finish *)
      Condition.wait control.condition control.mutex
    done;
    Array.copy control.results)
;;

(** shut down workers before joining *)
let stop_workers control workers =
  Mutex.protect control.mutex (fun () ->
    control.stop <- true;
    Condition.broadcast control.condition);
  List.iter (fun worker -> Internal.join worker) workers
;;

let reraise_worker_failure results =
  let failures = Array.to_list results |> List.filter_map Fun.id in
  let find predicate = List.find_opt (fun (exn, _) -> predicate exn) failures in
  let is_control_exception = function
    | Internal.Usage_error _ | Internal.Backend_error _ -> true
    | _ -> false
  in
  let is_overrun = function
    | Internal.Data_exhausted -> true
    | _ -> false
  in
  let is_invalid = function
    | Internal.Assume_rejected | Internal.Flaky_strategy -> true
    | _ -> false
  in
  (* invalidated or exhausted rules can cause in other workers.
     error precedence from greatest to least: usage/backend errors, overrun,
     invalidation, actual test failure. within each category, first worker raises. *)
  let selected =
    List.find_map
      find
      [ is_control_exception; is_overrun; is_invalid; (fun _ -> true) ]
  in
  Option.iter
    (fun (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace)
    selected
;;

let run
      tc
      ~state_machine
      ~num_workers
      ~run_round
      ~group_names
      ~state
      ~print_state
      ~check_invariants
  =
  let control =
    { mutex = Mutex.create ()
    ; condition = Condition.create ()
    ; curr_round = 0
    ; num_completed = 0
    ; stop = false
    ; cases = [||]
    ; results = Array.make num_workers None
    }
  in
  let rec start_workers worker_index workers =
    if worker_index = num_workers
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
         | None -> state
         | Some group ->
           Internal.note
             tc
             (Printf.sprintf
                "---------------- Round %d: group %S ----------------"
                round
                group_names.(group));
           dispatch_round control tc |> reraise_worker_failure;
           print_state state;
           check_invariants
             ~where:(Printf.sprintf "after round %d" round)
             ~sample:true
             state;
           loop (round + 1)
       in
       loop 1)
;;
