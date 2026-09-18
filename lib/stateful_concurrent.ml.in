(** Concurrent state-machine execution.

    Each round:

    - ask libhegel for the next rule group
    - clone the test case once per worker and run one body per worker through
      the concurrency capability's [spawn_join_n]. Each body gets rules for
      its worker from libhegel and runs them on its clone until libhegel ends
      the round, then reports its outcome.
    - re-raise the highest-precedence worker failure, then check invariants. *)

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
  Array.fold_left
    (fun groups name -> if List.mem name groups then groups else name :: groups)
    []
    names
  |> List.rev
  |> Array.of_list
;;

(* map the group of the ith rule to the index of the group in the group name array *)
let group_ids names group_names_uniq =
  Array.map
    (fun name -> Array.find_index (String.equal name) group_names_uniq |> Option.get)
    names
;;

(** [dispatch_round concurrency tc ~num_workers ~work] clones [tc] once per
    worker and runs [work] for every worker at once. Each body's exception is
    captured as its outcome. *)
let dispatch_round (concurrency : Concurrency.t) tc ~num_workers ~work =
  let cases =
    Array.init num_workers (fun worker_index ->
      let worker_tc = Internal.clone tc in
      Internal.set_worker_index worker_tc worker_index;
      worker_tc)
  in
  concurrency.spawn_join_n ~n:num_workers ~f:(fun worker_index ->
    match work ~worker_index cases.(worker_index) with
    | (_ : _ * int * bool) -> None
    | exception exn -> Some (exn, Printexc.get_raw_backtrace ()))
;;

let reraise_worker_failure (outcomes : Concurrency.outcome list) =
  let find predicate =
    List.find_map
      (function
        | Some (exn, _) as failure when predicate exn -> failure
        | _ -> None)
      outcomes
  in
  let is_control_exception = function
    | Internal.Usage_error _ | Internal.Internal_error _ -> true
    | _ -> false
  in
  let is_overrun = function
    | Internal.Stop_test -> true
    | _ -> false
  in
  let is_invalid = function
    | Internal.Assume_rejected | Internal.Flaky_strategy -> true
    | _ -> false
  in
  let is_test_failure = Fun.const true in
  (* invalidated or exhausted rules can cause in other workers.
     error precedence from greatest to least: usage/internal errors, overrun,
     invalidation, actual test failure. within each category, first worker raises. *)
  let failure =
    Array.find_map
      find
      [| is_control_exception; is_overrun; is_invalid; is_test_failure |]
  in
  Option.iter
    (fun (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace)
    failure
;;

let run
      tc
      ~concurrency
      ~state_machine
      ~num_workers
      ~run_round
      ~group_names
      ~state
      ~print_state
      ~check_invariants
  =
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
      reraise_worker_failure (dispatch_round concurrency tc ~num_workers ~work:run_round);
      print_state state;
      check_invariants ~where:(Printf.sprintf "after round %d" round) ~sample:true state;
      loop (round + 1)
  in
  loop 1
;;
