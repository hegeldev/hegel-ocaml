module type S = sig
  type 'a t
  type body = unit -> unit t
  type wait = body -> unit

  val run_loop : (wait -> unit) -> unit t
end

module Base_stateful = Stateful

module Make (B : S) = struct
  let run_hegel_test ?settings ?test_location ?database_key ?failure_blobs test_fn =
    B.run_loop (fun wait ->
      Internal.run_hegel_test
        ?settings
        ?test_location
        ?database_key
        ?failure_blobs
        (fun tc -> wait (fun () -> test_fn tc)))
  ;;

  let run_hegel_test_ppx ?settings ?test_location ?database_key ?failure_blobs test_fn =
    B.run_loop (fun wait ->
      Internal.run_hegel_test
        ?settings
        ?test_location
        ~from_ppx:true
        ?database_key
        ?failure_blobs
        (fun tc -> wait (fun () -> test_fn tc)))
  ;;

  module Stateful = struct
    module Pool = Base_stateful.Pool

    module Rule = struct
      type 'state t =
        { name : string
        ; weight : float
        ; step : Internal.test_case -> 'state -> unit B.t
        }

      let create ~name ?(weight = 1.0) ~step () = { name; weight; step }
      let name t = t.name
      let weight t = t.weight
    end

    module Invariant = struct
      type 'state t =
        { name : string
        ; inv : Internal.test_case -> 'state -> unit B.t
        ; always_check : bool
        }

      let create ~name ~inv ?(always_check = false) () = { name; inv; always_check }
      let name t = t.name
    end

    module type State_machine = sig
      type state

      val rules : state Rule.t list
      val invariants : state Invariant.t list
    end

    let run_internal ~init ~rules ~invariants ?sexp_of_state ?step_count tc =
      B.run_loop (fun wait ->
        let rules =
          List.map
            (fun { Rule.name; weight; step } ->
               Base_stateful.Rule.create
                 ~name
                 ~weight
                 ~step:(fun tc state -> wait (fun () -> step tc state))
                 ())
            rules
        in
        let invariants =
          List.map
            (fun { Invariant.name; inv; always_check } ->
               Base_stateful.Invariant.create
                 ~name
                 ~inv:(fun tc state -> wait (fun () -> inv tc state))
                 ~always_check
                 ())
            invariants
        in
        Base_stateful.run_internal ~init ~rules ~invariants ?sexp_of_state ?step_count tc)
    ;;

    let run
          (type s)
          ?step_count
          ?sexp_of_state
          tc
          (module M : State_machine with type state = s)
          ~(init : s)
      =
      run_internal
        ~init
        ~rules:M.rules
        ~invariants:M.invariants
        ?sexp_of_state
        ?step_count
        tc
    ;;
  end
end
