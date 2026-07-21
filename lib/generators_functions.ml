(** Generators for functions as test inputs, after Koen Claessen's {i Shrinking 
    and Showing Functions}.

    A drawn function is backed by a per-test-case memo table. The first time it
    is applied to a given argument it draws a fresh result from [returns] and
    stores it, so repeated applications to the same argument return the same
    value.

    On the failing final replay each top-level application of the generated
    function is printed through {!Hegel.note} as [name arg = result] (see
    {!functions}). Applications nested inside a span (draw depth > 0) are
    suppressed, like any nested draw. [name] is the [?name] passed to the
    generator, else the draw-site binding name (via [let%hegel_test]), else
    ["function"]. *)

open! Core
open Generators_core

(* [make ~explicit_name ~sexp_of_arg ~returns ~adapt] builds the shared function
   core behind {!functions}/{!functions2}/{!functions3}. [adapt] is the identity
   function, or currying for the multi-argument variants. [sexp_of_arg] renders
   the argument when a pair is shown. The memo table keys on the argument itself
   via structural hash/equality (a [Hashtbl.Poly]). The shown
   label is [explicit_name] when the caller passed [~name], else
   the draw-site [name] (the binding, via the PPX), else ["function"]. *)
let make
  : type a b c p.
    string option
    -> (a -> Core.Sexp.t)
    -> (b, p) generator
    -> ((a -> b) -> c)
    -> (c, unprintable) generator
  =
  fun explicit_name sexp_of_arg returns adapt ->
  let show_ret =
    match returns with
    | Printable { sexp_of; _ } -> sexp_of
    | _ -> fun _ -> Sexp.Atom "<opaque>"
  in
  let ret = core_of returns in
  let build ~name tc =
    let display =
      match explicit_name with
      | Some n -> n
      | None -> Option.value name ~default:"function"
    in
    let table = Hashtbl.Poly.create () in
    let base arg =
      let ret, is_fresh =
        match Hashtbl.find table arg with
        | Some ret -> ret, false
        | None ->
          let ret = group Labels.function_result tc (fun () -> do_draw ret tc) in
          Hashtbl.set table ~key:arg ~data:ret;
          ret, true
      in
      (* print every function call in verbose/debug verbosity, print only the first
         invocation in normal verbosity. *)
      if Internal.draw_depth tc = 0 && (is_fresh || Internal.is_high_verbosity tc)
      then
        Internal.note
          tc
          (sprintf
             "%s %s = %s"
             display
             (Sexp.to_string_hum (sexp_of_arg arg))
             (Sexp.to_string_hum (show_ret ret)));
      ret
    in
    adapt base
  in
  Unprintable { core = Function { build } }
;;

let sexp_or sexp_of_arg =
  Option.value sexp_of_arg ~default:(fun _ -> Sexp.Atom "<opaque>")
;;

(** [functions ?name ?sexp_of_arg ~returns ()] creates a generator for functions
    ['a -> 'b] whose results are drawn from [returns].

    The result carries no printer (its output type is a function), so draw it
    with {!Hegel.draw_silent}. Applying the drawn function to an argument draws a
    result from [returns] the first time that argument is seen and memoizes it.

    On the failing final replay each top-level application prints as
    [name arg = result] (applications nested inside a span are suppressed).
    [name] is [?name] when you pass it — an explicit label always wins — else the
    draw-site binding name inside a [let%hegel_test], else ["function"].

    The memo table keys on the argument itself, so distinct arguments always get
    independent results. [sexp_of_arg] only renders the argument in the shown
    pair; an omitted [sexp_of_arg] or an unprintable [returns] shows [<opaque>]. *)
let functions ?name ?sexp_of_arg ~returns () =
  make name (sexp_or sexp_of_arg) returns (fun f -> f)
;;

(** [functions2 ?name ?sexp_of_arg1 ?sexp_of_arg2 ~returns ()] creates a
    generator for curried two-argument functions ['a -> 'b -> 'c].

    Sugar over {!functions} keyed on the argument pair: the two arguments form
    one memo key and are shown uncurried as [name (arg1 arg2) = result] (more
    compact than a curried table). Draw it with {!Hegel.draw_silent}. *)
let functions2 ?name ?sexp_of_arg1 ?sexp_of_arg2 ~returns () =
  let sexp_of_arg (a, b) =
    Sexp.List [ (sexp_or sexp_of_arg1) a; (sexp_or sexp_of_arg2) b ]
  in
  make name sexp_of_arg returns (fun f a b -> f (a, b))
;;

(** [functions3 ?name ?sexp_of_arg1 ?sexp_of_arg2 ?sexp_of_arg3 ~returns ()]
    creates a generator for curried three-argument functions
    ['a -> 'b -> 'c -> 'd].

    Like {!functions2}, keyed on the argument triple and shown uncurried as
    [name (arg1 arg2 arg3) = result]. Draw it with {!Hegel.draw_silent}. *)
let functions3 ?name ?sexp_of_arg1 ?sexp_of_arg2 ?sexp_of_arg3 ~returns () =
  let sexp_of_arg (a, b, c) =
    Sexp.List
      [ (sexp_or sexp_of_arg1) a; (sexp_or sexp_of_arg2) b; (sexp_or sexp_of_arg3) c ]
  in
  make name sexp_of_arg returns (fun f a b c -> f (a, b, c))
;;
