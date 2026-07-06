open! Core
open Generators_core
open Generators_primitives

(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values.

    Implemented as an integer index generator: picks an index in [0, n-1] and
    returns [options.(index)]. The output type is the caller's, so the result
    carries no printer; use {!with_printer} to draw it with {!draw}. *)
let sampled_from options =
  let arr = Array.of_list options in
  let n = Array.length arr in
  if n = 0 then raise (Invalid_argument "sampled_from requires at least one element");
  map (fun i -> arr.(i)) (integers ~min_value:0 ~max_value:(n - 1) ())
;;

(** [one_of_core cores] builds the generation structure that picks among
    [cores]: an index is drawn inside a {!Labels.one_of} span and that branch is
    generated compositionally. *)
let one_of_core : type a. a core list -> a core =
  fun cores ->
  let gens = Array.of_list cores in
  let n = Array.length gens in
  Composite
    { label = Labels.one_of
    ; generate_fn =
        (fun data ->
          let idx = Internal.generate_integer data ~min_value:0 ~max_value:(n - 1) in
          do_draw gens.(idx) data)
    }
;;

(** [one_of generators] creates a generator that picks from one of the given
    [generators], all of which must be printable. Requires at least one
    generator. *)
let one_of (generators : ('a, printable) generator list) : ('a, printable) generator =
  match generators with
  | [] -> failwith "one_of requires at least one generator"
  | first :: _ ->
    Printable
      { core = one_of_core (List.map generators ~f:core_of); sexp_of = printer first }
;;

(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element].

    The [None]/[(Some v)] value renders through [Option.sexp_of_t] applied to
    [element]'s printer (the round-trippable form: [()] for [None], [(v)] for
    [Some v]). *)
let optional (element : ('a, printable) generator) : ('a option, printable) generator =
  let core =
    Composite
      { label = Labels.optional
      ; generate_fn =
          (fun data ->
            if Internal.generate_boolean data 0.5 None
            then Some (do_draw (core_of element) data)
            else None)
      }
  in
  Printable { core; sexp_of = Option.sexp_of_t (printer element) }
;;

(** [ip_addresses ?version ()] creates a generator for IP address strings.

    - [version = Some 4]: generates IPv4 addresses (dotted-decimal, RFC 791).
    - [version = Some 6]: generates IPv6 addresses (RFC 5952 canonical form).
    - [version = None] (default): generates either IPv4 or IPv6.

    The engine returns the address's raw network-order bytes, which [ipaddr]
    renders into canonical string form. *)
let rec ip_addresses ?version () =
  match version with
  | Some 4 ->
    leaf
      ~draw:(fun tc ->
        Ipaddr.V4.to_string (Ipaddr.V4.of_octets_exn (Internal.generate_ipv4 tc)))
      ~sexp_of:sexp_of_string
  | Some 6 ->
    leaf
      ~draw:(fun tc ->
        Ipaddr.V6.to_string (Ipaddr.V6.of_octets_exn (Internal.generate_ipv6 tc)))
      ~sexp_of:sexp_of_string
  | None -> one_of [ ip_addresses ~version:4 (); ip_addresses ~version:6 () ]
  | Some v -> failwith (sprintf "ip_addresses: invalid version %d" v)
;;

(** [tuples2 g1 g2] creates a generator for 2-element tuples of printable
    components: each element is drawn in sequence inside a {!Labels.tuple} span. *)
let tuples2 (type a b) (g1 : (a, printable) generator) (g2 : (b, printable) generator)
  : (a * b, printable) generator
  =
  let p1 = printer g1
  and p2 = printer g2 in
  let sexp_of (a, b) = Sexp.List [ p1 a; p2 b ] in
  let core =
    Composite
      { label = Labels.tuple
      ; generate_fn =
          (fun data ->
            let a = do_draw (core_of g1) data in
            let b = do_draw (core_of g2) data in
            a, b)
      }
  in
  Printable { core; sexp_of }
;;

(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples of printable
    components. *)
let tuples3
      (type a b c)
      (g1 : (a, printable) generator)
      (g2 : (b, printable) generator)
      (g3 : (c, printable) generator)
  : (a * b * c, printable) generator
  =
  let p1 = printer g1
  and p2 = printer g2
  and p3 = printer g3 in
  let sexp_of (a, b, c) = Sexp.List [ p1 a; p2 b; p3 c ] in
  let core =
    Composite
      { label = Labels.tuple
      ; generate_fn =
          (fun data ->
            let a = do_draw (core_of g1) data in
            let b = do_draw (core_of g2) data in
            let c = do_draw (core_of g3) data in
            a, b, c)
      }
  in
  Printable { core; sexp_of }
;;

(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples of printable
    components. *)
let tuples4
      (type a b c d)
      (g1 : (a, printable) generator)
      (g2 : (b, printable) generator)
      (g3 : (c, printable) generator)
      (g4 : (d, printable) generator)
  : (a * b * c * d, printable) generator
  =
  let p1 = printer g1
  and p2 = printer g2
  and p3 = printer g3
  and p4 = printer g4 in
  let sexp_of (a, b, c, d) = Sexp.List [ p1 a; p2 b; p3 c; p4 d ] in
  let core =
    Composite
      { label = Labels.tuple
      ; generate_fn =
          (fun data ->
            let a = do_draw (core_of g1) data in
            let b = do_draw (core_of g2) data in
            let c = do_draw (core_of g3) data in
            let d = do_draw (core_of g4) data in
            a, b, c, d)
      }
  in
  Printable { core; sexp_of }
;;
