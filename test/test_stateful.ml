open Core
module Atomic = Stdlib.Atomic
module Mutex = Caml_threads.Mutex

(* Stateful failure test: the [push] rule pushes an int in [0, 100] onto a
   stack; the [pop] rule fails when the popped value is >= 50. Should shrink to
   [push 50; pop]. Sequential rule bodies are ordinary functions, so the state
   and the bookkeeping are plain references. *)
let stateful_failure_test () =
  let module S = Hegel.Stateful in
  let last_pop = ref None in
  let push_rule =
    S.Rule.create
      ~name:"push"
      ~step:(fun tc stack ->
        let n = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:100 ()) in
        stack := n :: !stack)
      ()
  in
  let pop_rule =
    S.Rule.create
      ~name:"pop"
      ~step:(fun tc stack ->
        match !stack with
        | [] -> Hegel.assume tc false
        | top :: rest ->
          stack := rest;
          last_pop := Some top;
          assert (top < 50))
      ()
  in
  let module Stack = struct
    type state = int list ref

    let rules = [ push_rule; pop_rule ]
    let invariants = []
  end
  in
  (try
     Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~seed:0 ()) (fun tc ->
       S.run tc (module Stack) ~init:(ref []));
     failwith "expected property to fail"
   with
   | Assert_failure _ -> ());
  Alcotest.(check (option int)) "last pop value" (Some 50) !last_pop
;;

(* Stateful variables test: an [alloc]/[free] register allocator. [alloc] draws
   a fresh integer id, deposits it in the variables, and records it in a "live"
   set; [free] consumes an id from the variables and removes it from the set.
   Variables size must match the size of the live set. Empty-variables draws are
   rejected by the pool generator. *)

module Var_state = struct
  module S = Hegel.Stateful

  type t =
    { mutable live : Int.Set.t
    ; variables : int S.Pool.t
    }
end

let var_next_id = ref 0

let var_alloc_rule =
  let module S = Hegel.Stateful in
  S.Rule.create
    ~name:"alloc"
    ~step:(fun tc (state : Var_state.t) ->
      let id = !var_next_id in
      incr var_next_id;
      S.Pool.add state.variables tc id;
      state.live <- Set.add state.live id)
    ()
;;

let var_free_rule =
  let module S = Hegel.Stateful in
  S.Rule.create
    ~name:"free"
    ~step:(fun tc (state : Var_state.t) ->
      let id = Hegel.draw_silent tc (S.Pool.values_consumed state.variables) in
      assert (Set.mem state.live id);
      state.live <- Set.remove state.live id)
    ()
;;

let var_use_rule =
  let module S = Hegel.Stateful in
  S.Rule.create
    ~name:"use"
    ~step:(fun tc (state : Var_state.t) ->
      let size_before = S.Pool.size state.variables in
      let id = Hegel.draw_silent tc (S.Pool.values_reusable state.variables) in
      assert (Set.mem state.live id);
      Alcotest.(check int)
        "reuse preserves pool size"
        size_before
        (S.Pool.size state.variables))
    ()
;;

let stateful_variables_test () =
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:10 ~seed:0 ())
    (fun tc ->
       let module S = Hegel.Stateful in
       var_next_id := 0;
       let module Allocator = struct
         type state = Var_state.t

         let rules = [ var_alloc_rule; var_free_rule ]

         let invariants =
           [ S.Invariant.create
               ~name:"pool_sz"
               ~inv:(fun _tc (state : Var_state.t) ->
                 assert (S.Pool.size state.variables = Set.length state.live))
               ()
           ]
         ;;
       end
       in
       S.run
         tc
         (module Allocator)
         ~init:{ Var_state.live = Int.Set.empty; variables = S.Pool.create tc })
;;

let stateful_variables_draw_test () =
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:5 ~seed:0 ())
    (fun tc ->
       let module S = Hegel.Stateful in
       var_next_id := 0;
       let module Allocator = struct
         type state = Var_state.t

         let rules = [ var_alloc_rule; var_use_rule ]
         let invariants = []
       end
       in
       S.run
         tc
         (module Allocator)
         ~init:{ Var_state.live = Int.Set.empty; variables = S.Pool.create tc })
;;

let stateful_usage_error_test () =
  let module S = Hegel.Stateful in
  let attempts = ref 0 in
  let bad_rule =
    S.Rule.create
      ~name:"bad"
      ~step:(fun tc () ->
        incr attempts;
        ignore
          (Hegel.draw
             tc
             (Hegel.dates
                ~min_date:{ year = 2024; month = 1; day = 2 }
                ~max_date:{ year = 2024; month = 1; day = 1 }
                ())
           : string))
      ()
  in
  let module M = struct
    type state = unit

    let rules = [ bad_rule ]
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:20 ()) (fun tc ->
      S.run tc (module M) ~init:())
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check bool)
      "engine diagnostic"
      true
      (String.is_substring msg ~substring:"generate_date requires min_value <= max_value");
    Alcotest.(check int) "rule attempted once" 1 !attempts
;;

let stateful_rule_accessors_test () =
  let module S = Hegel.Stateful in
  let rule = S.Rule.create ~name:"my_rule" ~weight:2.5 ~step:(fun _tc _state -> ()) () in
  Alcotest.(check string) "name" "my_rule" (S.Rule.name rule);
  Alcotest.(check (float 0.)) "weight" 2.5 (S.Rule.weight rule);
  let unweighted = S.Rule.create ~name:"other" ~step:(fun _tc _state -> ()) () in
  Alcotest.(check (float 0.)) "default weight" 1.0 (S.Rule.weight unweighted)
;;

let stateful_no_rules_test () =
  let module Empty = struct
    type state = unit

    let rules = []
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
      Hegel.Stateful.run tc (module Empty) ~init:())
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check string)
      "engine diagnostic"
      "cannot run a state machine with no rules"
      msg
;;

(* Pins the engine-side contract documented on [Internal.pool_generate]: drawing
   from an empty pool rejects the test case with [Assume_rejected], not
   [Internal.Stop_test]. *)
let empty_pool_draw_rejects_test () =
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
      let pool = Hegel.Internal.new_pool tc in
      ignore (Hegel.Internal.pool_generate tc ~pool () : int))
  with
  | () -> Alcotest.fail "expected Unsatisfiable"
  | exception Failure msg ->
    Alcotest.(check bool)
      "failure msg"
      (String.is_substring_at msg ~pos:0 ~substring:"Unsatisfiable")
      true
;;

let stateful_step_count_forwarded_test () =
  let module S = Hegel.Stateful in
  let steps_this_case = ref 0 in
  let max_steps = ref 0 in
  let count_rule =
    S.Rule.create ~name:"count" ~step:(fun _tc () -> incr steps_this_case) ()
  in
  let module M = struct
    type state = unit

    let rules = [ count_rule ]
    let invariants = []
  end
  in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:20 ~seed:0 ())
    (fun tc ->
       steps_this_case := 0;
       S.run tc (module M) ~init:() ~step_count:5;
       max_steps := max !max_steps !steps_this_case);
  Alcotest.(check bool) "ran at least one step" true (!max_steps > 0);
  Alcotest.(check bool) "no case exceeded the configured cap" true (!max_steps <= 5)
;;

(* A step count below one is a usage error: the engine rejects it
   ([HEGEL_E_INVALID_ARG]) when the state machine is created, and the runner
   propagates it unshrunk, matching hegel-rust. *)
let stateful_step_count_below_one_test () =
  let module S = Hegel.Stateful in
  let module M = struct
    type state = unit

    let rules = [ S.Rule.create ~name:"noop" ~step:(fun _tc () -> ()) () ]
    let invariants = []
  end
  in
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.default ()) (fun tc ->
      S.run tc (module M) ~init:() ~step_count:0)
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check bool)
      "diagnostic names the constraint"
      true
      (String.is_substring msg ~substring:"step count must be at least 1")
;;

let test_stateful_bounded_steps () =
  let module S = Hegel.Stateful in
  let step_count = ref 0 in
  let step_rule =
    S.Rule.create
      ~name:"step"
      ~step:(fun _tc () ->
        incr step_count;
        if !step_count >= 10 then failwith "reached 10 steps")
      ()
  in
  let module M = struct
    type state = unit

    let rules = [ step_rule ]
    let invariants = []
  end
  in
  let raised_msg = ref "" in
  (try
     Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
       step_count := 0;
       S.run tc (module M) ~init:() ~step_count:10)
   with
   | e ->
     raised_msg := Exn.to_string e;
     Printf.printf "%s" !raised_msg);
  Alcotest.(check bool)
    "exception carries the original message"
    true
    (String.is_substring !raised_msg ~substring:"reached 10 steps");
  Alcotest.(check int) "ran exactly 10 steps" 10 !step_count
;;

let test_always_check_invariant () =
  let module S = Hegel.Stateful in
  let stateful_step_count = 10 in
  let always_inv_exec_count = ref 0 in
  let sampled_inv_exec_count = ref 0 in
  let noop = S.Rule.create ~name:"noop" ~step:(fun _tc _state -> ()) () in
  let always_check_invariant =
    S.Invariant.create
      ~name:"always_check"
      ~inv:(fun _tc _ -> incr always_inv_exec_count)
      ~always_check:true
      ()
  in
  let sampled_invariant =
    S.Invariant.create
      ~name:"sampled_check"
      ~inv:(fun _tc _ -> incr sampled_inv_exec_count)
      ()
  in
  let module M = struct
    type state = unit

    let rules = [ noop ]
    let invariants = [ always_check_invariant; sampled_invariant ]
  end
  in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:1 ~seed:1 ())
    (fun tc -> S.run tc (module M) ~init:() ~step_count:stateful_step_count);
  Alcotest.(check int)
    "always exec count = executed steps plus endpoint checks"
    (stateful_step_count + 2)
    !always_inv_exec_count;
  Alcotest.(check bool)
    "sampled exec count < always exec count"
    (* not generally true but is true for the seed *)
    (!sampled_inv_exec_count < !always_inv_exec_count)
    true
;;

let test_swarm_long_single_rule_run () =
  let module S = Hegel.Stateful in
  (* Per-test-case state: the longest run of one identical rule choice in the
     current case. *)
  let case_longest = ref 0 in
  let last_rule = ref None in
  let current_run = ref 0 in
  let long_run_cases = ref 0 in
  let make i =
    S.Rule.create
      ~name:(Printf.sprintf "rule_%d" i)
      ~step:(fun _tc () ->
        (match !last_rule with
         | Some j when j = i -> incr current_run
         | _ -> current_run := 1);
        last_rule := Some i;
        if !current_run > !case_longest then case_longest := !current_run)
      ()
  in
  let module M = struct
    type state = unit

    let rules = List.init 11 ~f:make
    let invariants = []
  end
  in
  Hegel.run_hegel_test
    ~settings:(Hegel.Settings.create ~test_cases:200 ~seed:0 ())
    (fun tc ->
       (* Reset per test case so a run can't bleed across cases. *)
       last_rule := None;
       current_run := 0;
       case_longest := 0;
       S.run tc (module M) ~init:();
       if !case_longest >= 20 then incr long_run_cases);
  Alcotest.(check bool)
    "swarm produces a recurring long single-rule chain"
    true
    (!long_run_cases >= 5)
;;

let stateful_hand_written_machine_test () =
  let module S = Hegel.Stateful in
  let steps = ref 0 in
  let checks = ref 0 in
  let module Counter = struct
    type state = int ref

    let rules =
      [ S.Rule.create
          ~name:"bump"
          ~step:(fun _tc n ->
            incr steps;
            incr n)
          ()
      ]
    ;;

    let invariants =
      [ S.Invariant.create
          ~name:"non_negative"
          ~inv:(fun _tc n ->
            incr checks;
            assert (!n >= 0))
          ~always_check:true
          ()
      ]
    ;;
  end
  in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
    S.run tc (module Counter) ~init:(ref 0) ~step_count:5);
  Alcotest.(check bool) "ran at least one step" true (!steps >= 1);
  Alcotest.(check int)
    "always-check runs per step plus both endpoints"
    (!steps + 2)
    !checks
;;

(* A pool created inside a rule body outlives that step. Regression test against
   a previous use-after-free. *)
let test_pool_created_inside_rule () =
  let used_pool = ref false in
  let module S = Hegel.Stateful in
  let module M = struct
    type state = int S.Pool.t option ref

    let rules =
      [ S.Rule.create
          ~name:"open"
          ~step:(fun tc state ->
            match !state with
            | Some _ -> ()
            | None ->
              let pool = S.Pool.create tc in
              S.Pool.add pool tc 1;
              state := Some pool)
          ()
      ; S.Rule.create
          ~name:"use"
          ~step:(fun tc state ->
            match !state with
            | None -> Hegel.assume tc false
            | Some pool ->
              S.Pool.add pool tc 2;
              let v = Hegel.draw_silent tc (S.Pool.values_reusable pool) in
              used_pool := true;
              assert (v = 1 || v = 2))
          ()
      ]
    ;;

    let invariants = []
  end
  in
  let settings =
    { (Hegel.Settings.create ~test_cases:5 ~seed:0 ()) with
      verbosity = Hegel.Settings.Verbose
    ; database = Hegel.Settings.Disabled
    }
  in
  Hegel.run_hegel_test ~settings (fun tc ->
    S.run tc (module M) ~init:(ref None) ~step_count:20);
  Alcotest.(check bool) "used a pool created by an earlier rule" true !used_pool
;;

let concurrent_rule_accessors_test () =
  let module R = Hegel.Stateful.Concurrent_rule in
  let rule =
    R.create ~name:"read" ~group:"io" ~weight:2.5 ~step:(fun _tc () _state -> ()) ()
  in
  Alcotest.(check string) "name" "read" (R.name rule);
  Alcotest.(check string) "group" "io" (R.group rule);
  Alcotest.(check (float 0.)) "weight" 2.5 (R.weight rule);
  let anonymous = R.create ~name:"write" ~step:(fun _tc () _state -> ()) () in
  Alcotest.(check string) "anonymous group" "<anonymous>" (R.group anonymous);
  Alcotest.(check (float 0.)) "default weight" 1.0 (R.weight anonymous)
;;

let rec try_decrement n =
  let v = Stdlib.Atomic.get n in
  v > 0 && (Stdlib.Atomic.compare_and_set n v (v - 1) || try_decrement n)
;;

let concurrent_smoke_test () =
  let module S = Hegel.Stateful in
  let increment =
    S.Concurrent_rule.create
      ~name:"increment"
      ~step:(fun _tc () (value : int Atomic.t) -> Atomic.incr value)
      ()
  in
  let decrement =
    S.Concurrent_rule.create
      ~name:"decrement"
      ~step:(fun tc () (value : int Atomic.t) -> Hegel.assume tc (try_decrement value))
      ()
  in
  let module M = struct
    type ctx = unit
    type state = int Atomic.t

    let rules = [ increment; decrement ]

    let invariants =
      [ S.Invariant.create
          ~name:"nonnegative"
          ~inv:(fun _tc value -> assert (Atomic.get value >= 0))
          ()
      ]
    ;;
  end
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:5 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       S.run_concurrent
         ~concurrency:Hegel.Concurrency.threads
         tc
         (module M)
         ~step_count:5
         ~init:(Atomic.make 0)
         ~min_concurrency:1
         ~max_concurrency:2)
;;

let concurrent_groups_do_not_overlap_test ~concurrency () =
  let module S = Hegel.Stateful in
  let lock = Mutex.create () in
  let active_group : string option Atomic.t = Atomic.make None in
  let active_workers = Atomic.make 0 in
  let seen_groups : string list Atomic.t = Atomic.make [] in
  let step (group : string) (_tc : Hegel.Internal.test_case) () () =
    Mutex.protect lock (fun () ->
      (match Atomic.get active_group with
       | None -> Atomic.set active_group (Some group)
       | Some active when String.equal active group -> ()
       | Some _ -> failwith "different groups overlapped");
      Atomic.incr active_workers;
      let seen = Atomic.get seen_groups in
      if not (List.mem seen group ~equal:String.equal)
      then Atomic.set seen_groups (group :: seen));
    Fun.protect
      ~finally:(fun () ->
        Mutex.protect lock (fun () ->
          Atomic.decr active_workers;
          if Atomic.get active_workers = 0 then Atomic.set active_group None))
      (fun () -> Caml_unix.sleepf 0.001)
  in
  let alpha =
    S.Concurrent_rule.create ~name:"alpha" ~group:"letters" ~step:(step "letters") ()
  in
  let beta =
    S.Concurrent_rule.create ~name:"beta" ~group:"letters" ~step:(step "letters") ()
  in
  let one =
    S.Concurrent_rule.create ~name:"one" ~group:"numbers" ~step:(step "numbers") ()
  in
  let anonymous =
    S.Concurrent_rule.create ~name:"anonymous" ~step:(step "<anonymous>") ()
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:25 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       S.run_concurrent
         ~concurrency
         tc
         (module struct
           type ctx = unit
           type state = unit

           let rules = [ alpha; beta; one; anonymous ]
           let invariants = []
         end)
         ~step_count:10
         ~init:()
         ~min_concurrency:8
         ~max_concurrency:8);
  Alcotest.(check (list string))
    "every group ran"
    [ "<anonymous>"; "letters"; "numbers" ]
    (Atomic.get seen_groups |> List.sort ~compare:String.compare)
;;

let pool_add_reuse_consume_test () =
  let module P = Hegel.Stateful.Pool in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~seed:0 ()) with database = Hegel.Settings.Disabled }
    (fun tc ->
       let pool = P.create tc in
       Alcotest.(check int) "starts empty" 0 (P.size pool);
       P.add pool tc 10;
       P.add pool tc 20;
       Alcotest.(check int) "two values" 2 (P.size pool);
       let reused = Hegel.draw_silent tc (P.values_reusable pool) in
       Alcotest.(check bool) "reused member" true (reused = 10 || reused = 20);
       Alcotest.(check int) "reuse preserves size" 2 (P.size pool);
       let first = Hegel.draw_silent tc (P.values_consumed pool) in
       Alcotest.(check int) "one remains" 1 (P.size pool);
       let second = Hegel.draw_silent tc (P.values_consumed pool) in
       Alcotest.(check int) "both values consumed" 30 (first + second);
       Alcotest.(check int) "ends empty" 0 (P.size pool))
;;

let pool_reusable_shares_by_default_test () =
  let module P = Hegel.Stateful.Pool in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
    let original = Atomic.make 10 in
    let shared_pool = P.create tc in
    P.add shared_pool tc original;
    let shared = Hegel.draw_silent tc (P.values_reusable shared_pool) in
    Alcotest.(check bool) "default returns original" true (phys_equal shared original);
    let pool =
      P.create ~clone:(fun (value : int Atomic.t) -> Atomic.make (Atomic.get value)) tc
    in
    P.add pool tc original;
    let generator = P.values_reusable pool in
    let first = Hegel.draw_silent tc generator in
    Atomic.set first 99;
    let second = Hegel.draw_silent tc generator in
    Alcotest.(check int) "second draw is independent" 10 (Atomic.get second);
    Alcotest.(check int) "reuse preserves size" 1 (P.size pool);
    let consumed = Hegel.draw_silent tc (P.values_consumed pool) in
    Alcotest.(check bool) "consume returns original" true (phys_equal consumed original);
    Alcotest.(check int) "consume empties pool" 0 (P.size pool))
;;

let pool_reusable_clones_test () =
  let module P = Hegel.Stateful.Pool in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
    let original = Atomic.make 10 in
    let clone_count = Atomic.make 0 in
    let pool =
      P.create
        ~clone:(fun (value : int Atomic.t) ->
          Atomic.incr clone_count;
          Atomic.make (Atomic.get value))
        tc
    in
    P.add pool tc original;
    let generator = P.values_reusable pool in
    let first = Hegel.draw_silent tc generator in
    Atomic.set first 99;
    let second = Hegel.draw_silent tc generator in
    Alcotest.(check int) "each draw clones" 2 (Atomic.get clone_count);
    Alcotest.(check int) "second draw is independent" 10 (Atomic.get second);
    Alcotest.(check int) "reuse preserves size" 1 (P.size pool);
    let consumed = Hegel.draw_silent tc (P.values_consumed pool) in
    Alcotest.(check bool) "consume returns original" true (phys_equal consumed original);
    Alcotest.(check int) "consume does not clone" 2 (Atomic.get clone_count);
    Alcotest.(check int) "consume empties pool" 0 (P.size pool))
;;

let pool_empty_draw_rejects_test () =
  let module P = Hegel.Stateful.Pool in
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
      let pool = P.create tc in
      ignore (Hegel.draw_silent tc (P.values_consumed pool) : int))
  with
  | () -> Alcotest.fail "expected Unsatisfiable"
  | exception Failure msg ->
    Alcotest.(check bool)
      "empty draw rejects"
      true
      (String.is_substring_at msg ~pos:0 ~substring:"Unsatisfiable")
;;

let pool_parallel_adds_test ~concurrency () =
  let module S = Hegel.Stateful in
  let add =
    S.Concurrent_rule.create
      ~name:"add"
      ~step:(fun tc () ((pool, next) : int S.Pool.t * int Atomic.t) ->
        let value = Atomic.fetch_and_add next 1 in
        S.Pool.add pool tc value)
      ()
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:25 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       let pool = S.Pool.create tc in
       let next = Atomic.make 0 in
       S.run_concurrent
         ~concurrency
         tc
         (module struct
           type ctx = unit
           type state = int S.Pool.t * int Atomic.t

           let rules = [ add ]
           let invariants = []
         end)
         ~step_count:20
         ~init:(pool, next)
         ~min_concurrency:8
         ~max_concurrency:8;
       let value_count = Atomic.get next in
       Alcotest.(check int) "all additions retained" value_count (S.Pool.size pool);
       let consumed =
         List.init value_count ~f:(fun _ ->
           Hegel.draw_silent tc (S.Pool.values_consumed pool))
         |> List.sort ~compare:Int.compare
       in
       Alcotest.(check (list int))
         "pool contains exactly the added values"
         (List.range 0 value_count)
         consumed)
;;

(* [atomic_push cell v] prepends [v] to the list in [cell]. Concurrent rule
   bodies are portable, so shared results go through atomics rather than refs. *)
let rec atomic_push cell v =
  let old = Stdlib.Atomic.get cell in
  if not (Stdlib.Atomic.compare_and_set cell old (v :: old)) then atomic_push cell v
;;

let pool_parallel_consumes_test ~concurrency () =
  let module S = Hegel.Stateful in
  let initial_size = 16 in
  let consume =
    S.Concurrent_rule.create
      ~name:"consume"
      ~step:(fun tc () ((pool, consumed) : int S.Pool.t * int list Atomic.t) ->
        let value = Hegel.draw_silent tc (S.Pool.values_consumed pool) in
        atomic_push consumed value)
      ()
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:25 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       let pool = S.Pool.create tc in
       List.iter (List.range 0 initial_size) ~f:(S.Pool.add pool tc);
       let consumed : int list Atomic.t = Atomic.make [] in
       S.run_concurrent
         ~concurrency
         tc
         (module struct
           type ctx = unit
           type state = int S.Pool.t * int list Atomic.t

           let rules = [ consume ]
           let invariants = []
         end)
         ~step_count:10
         ~init:(pool, consumed)
         ~min_concurrency:4
         ~max_concurrency:4;
       let consumed = Atomic.get consumed in
       let consumed_count = List.length consumed in
       Alcotest.(check int)
         "every successful consume was unique"
         consumed_count
         (Int.Set.of_list consumed |> Set.length);
       Alcotest.(check int)
         "size tracks successful consumes"
         (initial_size - consumed_count)
         (S.Pool.size pool);
       let remaining =
         List.init (initial_size - consumed_count) ~f:(fun _ ->
           Hegel.draw_silent tc (S.Pool.values_consumed pool))
       in
       Alcotest.(check (list int))
         "consumed and remaining values partition the initial pool"
         (List.range 0 initial_size)
         (List.sort (consumed @ remaining) ~compare:Int.compare))
;;

let pool_parallel_adds_and_consumes_test ~concurrency () =
  let module S = Hegel.Stateful in
  let exchange =
    S.Concurrent_rule.create
      ~name:"exchange"
      ~step:
        (fun
          tc
          ()
          ((pool, next, consumed) : int S.Pool.t * int Atomic.t * int list Atomic.t) ->
        let value = Atomic.fetch_and_add next 1 in
        S.Pool.add pool tc value;
        Domain.cpu_relax ();
        let consumed_value = Hegel.draw_silent tc (S.Pool.values_consumed pool) in
        atomic_push consumed consumed_value)
      ()
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:25 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       let pool = S.Pool.create tc in
       let next = Atomic.make 0 in
       let consumed : int list Atomic.t = Atomic.make [] in
       S.run_concurrent
         ~concurrency
         tc
         (module struct
           type ctx = unit
           type state = int S.Pool.t * int Atomic.t * int list Atomic.t

           let rules = [ exchange ]
           let invariants = []
         end)
         ~step_count:10
         ~init:(pool, next, consumed)
         ~min_concurrency:8
         ~max_concurrency:8;
       let consumed = Atomic.get consumed in
       let value_count = Atomic.get next in
       Alcotest.(check int)
         "every addition was consumed"
         value_count
         (List.length consumed);
       Alcotest.(check (list int))
         "each added value was consumed exactly once"
         (List.range 0 value_count)
         (List.sort consumed ~compare:Int.compare);
       Alcotest.(check int) "pool is empty" 0 (S.Pool.size pool))
;;

exception Concurrent_boom of int

let concurrent_worker_exception_is_rethrown_test () =
  let module S = Hegel.Stateful in
  let boom =
    S.Concurrent_rule.create
      ~name:"boom"
      ~step:(fun tc () () ->
        let value = Hegel.draw tc (Hegel.integers ()) in
        raise (Concurrent_boom value))
      ()
  in
  match
    Hegel.run_hegel_test
      ~settings:
        { (Hegel.Settings.create ~test_cases:20 ~seed:0 ()) with
          database = Hegel.Settings.Disabled
        ; verbosity = Hegel.Settings.Quiet
        }
      (fun tc ->
         S.run_concurrent
           ~concurrency:Hegel.Concurrency.threads
           tc
           (module struct
             type ctx = unit
             type state = unit

             let rules = [ boom ]
             let invariants = []
           end)
           ~step_count:5
           ~init:()
           ~min_concurrency:2
           ~max_concurrency:2)
  with
  | () -> Alcotest.fail "expected Concurrent_boom"
  | exception Concurrent_boom _ -> ()
  | exception exn -> raise exn
;;

let concurrent_worker_usage_error_test () =
  let module S = Hegel.Stateful in
  let bad =
    S.Concurrent_rule.create
      ~name:"bad"
      ~step:(fun tc () () ->
        ignore
          (Hegel.draw_silent
             tc
             (Hegel.dates
                ~min_date:{ year = 2024; month = 1; day = 2 }
                ~max_date:{ year = 2024; month = 1; day = 1 }
                ())))
      ()
  in
  match
    Hegel.run_hegel_test
      ~settings:(Hegel.Settings.create ~test_cases:2 ~seed:0 ())
      (fun tc ->
         S.run_concurrent
           ~concurrency:Hegel.Concurrency.threads
           tc
           (module struct
             type ctx = unit
             type state = unit

             let rules = [ bad ]
             let invariants = []
           end)
           ~init:()
           ~min_concurrency:2
           ~max_concurrency:2)
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error message ->
    Alcotest.(check string)
      "worker usage error message"
      "generate_date requires min_value <= max_value, got [Date { year: 2024, month: 1, \
       day: 2 }, Date { year: 2024, month: 1, day: 1 }]"
      message
;;

(* Every worker raises the same control exception, so the round's outcomes all
   carry it and the main thread must re-raise it with the right precedence. *)
let concurrent_worker_control_exceptions_test () =
  let module S = Hegel.Stateful in
  List.iter
    [ Hegel.Internal.Internal_error "worker internal failure"
    ; Hegel.Internal.Stop_test
    ; Hegel.Internal.Flaky_strategy
    ; Hegel.Internal.Assume_rejected
    ]
    ~f:(fun error ->
      let rule =
        S.Concurrent_rule.create
          ~name:"control"
          ~step:(fun tc () () ->
            match error with
            | Hegel.Internal.Assume_rejected ->
              (* exceeding the libhegel nesting limit invalidates the tc *)
              for _ = 1 to 1001 do
                Hegel.Internal.start_span tc
              done;
              ignore (Hegel.draw tc (Hegel.booleans ()) : bool)
            | _ -> raise error)
          ()
      in
      match
        Hegel.run_hegel_test
          ~settings:
            { (Hegel.Settings.create ~test_cases:2 ~seed:0 ()) with
              database = Hegel.Settings.Disabled
            ; verbosity = Hegel.Settings.Quiet
            }
          (fun tc ->
             S.run_concurrent
               ~concurrency:Hegel.Concurrency.threads
               tc
               (module struct
                 type ctx = unit
                 type state = unit

                 let rules = [ rule ]
                 let invariants = []
               end)
               ~init:()
               ~min_concurrency:2
               ~max_concurrency:4)
      with
      | () -> Alcotest.fail "expected worker control exception or health check"
      | exception Hegel.Internal.Internal_error message ->
        (match error with
         | Hegel.Internal.Internal_error expected ->
           Alcotest.(check string) "internal error propagated" expected message
         | _ -> Alcotest.fail "unexpected internal error")
      | exception Failure message ->
        (match error with
         | Hegel.Internal.Flaky_strategy | Hegel.Internal.Assume_rejected ->
           Alcotest.(check bool)
             "invalid worker cases are rejected instead of failing the property"
             true
             (String.is_substring message ~substring:"FilterTooMuch")
         | Hegel.Internal.Stop_test ->
           Alcotest.(check bool)
             "overrun worker cases are reported as overruns"
             true
             (String.is_substring message ~substring:"TestCasesTooLarge")
         | _ -> Alcotest.fail "internal error was converted to a property failure"))
;;

let concurrent_always_check_invariant_test () =
  let module S = Hegel.Stateful in
  let step_count = 10 in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:1 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       let invariants_checked = ref 0 in
       let step = S.Concurrent_rule.create ~name:"step" ~step:(fun _tc () () -> ()) () in
       let invariant =
         S.Invariant.create
           ~name:"check_every_round"
           ~always_check:true
           ~inv:(fun _tc () -> incr invariants_checked)
           ()
       in
       S.run_concurrent
         ~concurrency:Hegel.Concurrency.threads
         tc
         (module struct
           type ctx = unit
           type state = unit

           let rules = [ step ]
           let invariants = [ invariant ]
         end)
         ~step_count
         ~init:()
         ~min_concurrency:2
         ~max_concurrency:2;
       Alcotest.(check int)
         "one check per round plus initial/final states"
         (step_count + 2)
         !invariants_checked)
;;

let concurrent_invariant_waits_for_workers_test ~concurrency () =
  let module S = Hegel.Stateful in
  let module M = struct
    type ctx = unit
    type state = int Atomic.t

    let rules =
      [ S.Concurrent_rule.create
          ~name:"work"
          ~step:(fun _tc () (active : int Atomic.t) ->
            Atomic.incr active;
            Domain.cpu_relax ();
            Atomic.decr active)
          ()
      ]
    ;;

    let invariants =
      [ S.Invariant.create
          ~name:"workers_finished"
          ~always_check:true
          ~inv:(fun _tc active -> assert (Atomic.get active = 0))
          ()
      ]
    ;;
  end
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:10 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       S.run_concurrent
         ~concurrency
         tc
         (module M)
         ~init:(Atomic.make 0)
         ~min_concurrency:4
         ~max_concurrency:4)
;;

let clone_exception_releases_pool_lock_test () =
  let module P = Hegel.Stateful.Pool in
  Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
    let pool = P.create ~clone:(fun _ -> failwith "clone failed") tc in
    P.add pool tc 42;
    (match Hegel.draw_silent tc (P.values_reusable pool) with
     | _ -> Alcotest.fail "expected clone failure"
     | exception Failure message ->
       Alcotest.(check string) "clone error" "clone failed" message);
    Alcotest.(check int) "failed clone leaves value in pool" 1 (P.size pool);
    Alcotest.(check int)
      "consume still works"
      42
      (Hegel.draw_silent tc (P.values_consumed pool)))
;;

let concurrent_invalid_bounds_test () =
  let module S = Hegel.Stateful in
  let noop = S.Concurrent_rule.create ~name:"noop" ~step:(fun _tc () () -> ()) () in
  List.iter
    [ 0, 1, "state machine concurrency bounds must satisfy 1 <= min <= max, got [0, 1]"
    ; 2, 1, "state machine concurrency bounds must satisfy 1 <= min <= max, got [2, 1]"
    ]
    ~f:(fun (min_concurrency, max_concurrency, expected) ->
      match
        Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
          S.run_concurrent
            ~concurrency:Hegel.Concurrency.threads
            tc
            (module struct
              type ctx = unit
              type state = unit

              let rules = [ noop ]
              let invariants = []
            end)
            ~init:()
            ~min_concurrency
            ~max_concurrency)
      with
      | () -> Alcotest.fail "expected Usage_error"
      | exception Hegel.Usage_error msg ->
        Alcotest.(check string) "engine diagnostic" expected msg)
;;

let concurrent_no_rules_test () =
  match
    Hegel.run_hegel_test ~settings:(Hegel.Settings.create ~test_cases:1 ()) (fun tc ->
      Hegel.Stateful.run_concurrent
        ~concurrency:Hegel.Concurrency.threads
        tc
        (module struct
          type ctx = unit
          type state = unit

          let rules = []
          let invariants = []
        end)
        ~init:())
  with
  | () -> Alcotest.fail "expected Usage_error"
  | exception Hegel.Usage_error msg ->
    Alcotest.(check string)
      "engine diagnostic"
      "cannot run a state machine with no rules"
      msg
;;

(* [max_concurrency] defaults to [min_concurrency], so passing only the
   minimum fixes the worker count. Observed through the capability, which
   receives that count as [n]. *)
let concurrent_custom_concurrency_test () =
  let module S = Hegel.Stateful in
  let calls = ref [] in
  let sequential : unit Hegel.Concurrency.t =
    { spawn_join_n =
        (fun ~n ~f ->
          calls := n :: !calls;
          List.init n ~f:(fun i -> f () i))
    }
  in
  let ran = Atomic.make 0 in
  let step =
    S.Concurrent_rule.create ~name:"step" ~step:(fun _tc () () -> Atomic.incr ran) ()
  in
  Hegel.run_hegel_test
    ~settings:
      { (Hegel.Settings.create ~test_cases:3 ~seed:0 ()) with
        database = Hegel.Settings.Disabled
      }
    (fun tc ->
       S.run_concurrent
         ~concurrency:sequential
         tc
         (module struct
           type ctx = unit
           type state = unit

           let rules = [ step ]
           let invariants = []
         end)
         ~step_count:4
         ~init:()
         ~min_concurrency:3);
  Alcotest.(check bool) "capability was used" true (not (List.is_empty !calls));
  Alcotest.(check bool)
    "asked for the drawn concurrency"
    true
    (List.for_all !calls ~f:(fun n -> n = 3));
  Alcotest.(check bool) "rules ran" true (Atomic.get ran > 0)
;;

let tests =
  [ Alcotest.test_case
      "stateful: concurrent always-check invariants"
      `Quick
      concurrent_always_check_invariant_test
  ; Alcotest.test_case
      "stateful: concurrent invariants wait for workers (threads)"
      `Quick
      (concurrent_invariant_waits_for_workers_test ~concurrency:Hegel.Concurrency.threads)
  ; Alcotest.test_case
      "stateful: concurrent invariants wait for workers (parallel)"
      `Quick
      (concurrent_invariant_waits_for_workers_test
         ~concurrency:Test_helpers.parallel_concurrency)
  ; Alcotest.test_case
      "stateful: clone failure releases pool lock"
      `Quick
      clone_exception_releases_pool_lock_test
  ; Alcotest.test_case "stateful: failing property shrinks" `Quick stateful_failure_test
  ; Alcotest.test_case
      "stateful: run drives a hand-written State_machine"
      `Quick
      stateful_hand_written_machine_test
  ; Alcotest.test_case
      "stateful: variables add/consume round-trips"
      `Quick
      stateful_variables_test
  ; Alcotest.test_case
      "stateful: variables draw (non-consuming)"
      `Quick
      stateful_variables_draw_test
  ; Alcotest.test_case
      "stateful: usage error aborts the run"
      `Quick
      stateful_usage_error_test
  ; Alcotest.test_case "stateful: rule name accessor" `Quick stateful_rule_accessors_test
  ; Alcotest.test_case "stateful: empty rules raises" `Quick stateful_no_rules_test
  ; Alcotest.test_case
      "stateful: empty pool draw rejects"
      `Quick
      empty_pool_draw_rejects_test
  ; Alcotest.test_case
      "stateful: step count is forwarded to the engine"
      `Quick
      stateful_step_count_forwarded_test
  ; Alcotest.test_case
      "stateful: step count below one is rejected"
      `Quick
      stateful_step_count_below_one_test
  ; Alcotest.test_case
      "stateful: step_count bounds steps"
      `Quick
      test_stateful_bounded_steps
  ; Alcotest.test_case
      "stateful: always-check invariant runs after every step"
      `Quick
      test_always_check_invariant
  ; Alcotest.test_case
      "stateful: swarm yields a long single-rule chain"
      `Quick
      test_swarm_long_single_rule_run
  ; Alcotest.test_case
      "stateful: pool created inside a rule outlives the step"
      `Quick
      test_pool_created_inside_rule
  ; Alcotest.test_case
      "stateful: concurrent rule accessors"
      `Quick
      concurrent_rule_accessors_test
  ; Alcotest.test_case "stateful: concurrent smoke test" `Quick concurrent_smoke_test
  ; Alcotest.test_case
      "stateful: concurrent groups do not overlap (threads)"
      `Quick
      (concurrent_groups_do_not_overlap_test ~concurrency:Hegel.Concurrency.threads)
  ; Alcotest.test_case
      "stateful: concurrent groups do not overlap (parallel)"
      `Quick
      (concurrent_groups_do_not_overlap_test
         ~concurrency:Test_helpers.parallel_concurrency)
  ; Alcotest.test_case
      "stateful: pool add/reuse/consume"
      `Quick
      pool_add_reuse_consume_test
  ; Alcotest.test_case
      "stateful: pool reusable draws share by default"
      `Quick
      pool_reusable_shares_by_default_test
  ; Alcotest.test_case
      "stateful: pool reusable draws clone"
      `Quick
      pool_reusable_clones_test
  ; Alcotest.test_case
      "stateful: pool empty draw rejects"
      `Quick
      pool_empty_draw_rejects_test
  ; Alcotest.test_case
      "stateful: pool retains parallel additions (threads)"
      `Quick
      (pool_parallel_adds_test ~concurrency:Hegel.Concurrency.threads)
  ; Alcotest.test_case
      "stateful: pool retains parallel additions (parallel)"
      `Quick
      (pool_parallel_adds_test ~concurrency:Test_helpers.parallel_concurrency)
  ; Alcotest.test_case
      "stateful: pool retains parallel additions (sequential)"
      `Quick
      (pool_parallel_adds_test ~concurrency:Test_helpers.sequential_concurrency)
  ; Alcotest.test_case
      "stateful: pool consumes each value once (threads)"
      `Quick
      (pool_parallel_consumes_test ~concurrency:Hegel.Concurrency.threads)
  ; Alcotest.test_case
      "stateful: pool consumes each value once (parallel)"
      `Quick
      (pool_parallel_consumes_test ~concurrency:Test_helpers.parallel_concurrency)
  ; Alcotest.test_case
      "stateful: pool preserves parallel additions and consumes (threads)"
      `Quick
      (pool_parallel_adds_and_consumes_test ~concurrency:Hegel.Concurrency.threads)
  ; Alcotest.test_case
      "stateful: pool preserves parallel additions and consumes (parallel)"
      `Quick
      (pool_parallel_adds_and_consumes_test
         ~concurrency:Test_helpers.parallel_concurrency)
  ; Alcotest.test_case
      "stateful: concurrent worker exception is rethrown"
      `Quick
      concurrent_worker_exception_is_rethrown_test
  ; Alcotest.test_case
      "stateful: concurrent worker usage error"
      `Quick
      concurrent_worker_usage_error_test
  ; Alcotest.test_case
      "stateful: concurrent worker control exceptions"
      `Quick
      concurrent_worker_control_exceptions_test
  ; Alcotest.test_case
      "stateful: concurrent invalid bounds"
      `Quick
      concurrent_invalid_bounds_test
  ; Alcotest.test_case "stateful: concurrent empty rules" `Quick concurrent_no_rules_test
  ; Alcotest.test_case
      "stateful: concurrent custom concurrency"
      `Quick
      concurrent_custom_concurrency_test
  ]
;;
