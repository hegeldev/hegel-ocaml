(** Generators for functions as test inputs, after Koen Claessen's {i Shrinking 
    and Showing Functions}.

    A drawn function is backed by a per-test-case memo table. The first time it
    is applied to a given argument it draws a fresh result from [returns] and
    stores, so repeated applications to the same argument return the same
    value. 

    On the failing final replay each application of the generated function is 
    printed through {!Hegel.note} as [name arg = result] (see {!functions}). 
    [name] is the supplied to the generator, the draw site under in by 
    [let%hegel_test], or [function] by default *)

open! Core
open Generators_core

(* [make ~default_name ~sexp_of_arg ~returns ~adapt] builds the shared function
   core behind {!functions}/{!functions2}/{!functions3}. [adapt] is the identity function,
   or currying for the multi-argument variants. [sexp_of_arg] both keys the memo
   table and renders the argument when a pair is shown. [name] is the draw-site 
   label, falling back to [default_name]. *)
let make ~default_name ~sexp_of_arg ~returns ~adapt =
  let ret = core_of returns in
  let show_ret = printer returns in
  let build ~name tc =
    let display = Option.value name ~default:default_name in
    let table = Hashtbl.create (module String) in
    let base arg =
      let key = Sexp.to_string (sexp_of_arg arg) in
      match Hashtbl.find table key with
      | Some v -> v
      | None ->
        let v = group Labels.function_result tc (fun () -> do_draw ret tc) in
        Hashtbl.set table ~key ~data:v;
        Internal.note
          tc
          (sprintf
             "%s %s = %s"
             display
             (Sexp.to_string_hum (sexp_of_arg arg))
             (Sexp.to_string_hum (show_ret v)));
        v
    in
    adapt base
  in
  Unprintable { core = Function { build } }
;;

(** [functions ?name ~sexp_of_arg ~returns ()] creates a generator for functions
    ['a -> 'b] whose results are drawn from [returns].

    The result carries no printer (its output type is a function), so draw it
    with {!Hegel.draw_silent}. Applying the drawn function to an argument draws a
    result from [returns] the first time that argument is seen and memoizes it.

    On the failing final replay each function application prints as [name arg = result].
    [name] defaults to ["function"] and is overridden by the draw-site name (the
    binding name inside a [let%hegel_test]). Pass [?name] to set a fallback when
    drawing without the PPX. [sexp_of_arg] both keys the memo table and renders
    arguments; [returns] must be printable so results can be shown. *)
let functions ?(name = "function") ~sexp_of_arg ~returns () =
  make ~default_name:name ~sexp_of_arg ~returns ~adapt:(fun f -> f)
;;

(** [functions2 ?name ~sexp_of_arg1 ~sexp_of_arg2 ~returns ()] creates a
    generator for curried two-argument functions ['a -> 'b -> 'c].

    Sugar over {!functions} keyed on the argument pair: the two arguments form
    one memo key and are shown uncurried as [name (arg1 arg2) = result] (more
    compact than a curried table). Draw it with {!Hegel.draw_silent}. *)
let functions2 ?(name = "function") ~sexp_of_arg1 ~sexp_of_arg2 ~returns () =
  let sexp_of_arg (a, b) = Sexp.List [ sexp_of_arg1 a; sexp_of_arg2 b ] in
  make ~default_name:name ~sexp_of_arg ~returns ~adapt:(fun f a b -> f (a, b))
;;

(** [functions3 ?name ~sexp_of_arg1 ~sexp_of_arg2 ~sexp_of_arg3 ~returns ()]
    creates a generator for curried three-argument functions
    ['a -> 'b -> 'c -> 'd].

    Like {!functions2}, keyed on the argument triple and shown uncurried as
    [name (arg1 arg2 arg3) = result]. Draw it with {!Hegel.draw_silent}. *)
let functions3 ?(name = "function") ~sexp_of_arg1 ~sexp_of_arg2 ~sexp_of_arg3 ~returns () =
  let sexp_of_arg (a, b, c) =
    Sexp.List [ sexp_of_arg1 a; sexp_of_arg2 b; sexp_of_arg3 c ]
  in
  make ~default_name:name ~sexp_of_arg ~returns ~adapt:(fun f a b c -> f (a, b, c))
;;
