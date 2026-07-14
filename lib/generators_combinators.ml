open! Core
open Generators_core
open Generators_primitives

(** [sampled_from options] creates a generator that samples from a non-empty
    list of values.

    Implemented as an integer index generator: picks an index in [0, n-1] and
    returns [options.(index)]. The engine's bounded-integer draw is deliberately
    non-uniform (boundary and "interesting" indices are over-weighted), so
    earlier elements — the first in particular — are drawn more often than the
    middle ones. The output type is the caller's, so the result carries no
    printer; use {!with_printer} to draw it with {!draw}. *)
let sampled_from options =
  let arr = Array.of_list options in
  let n = Array.length arr in
  if n = 0 then raise (Invalid_argument "sampled_from requires at least one element");
  map (fun i -> arr.(i)) (integers ~min_value:0 ~max_value:(n - 1) ())
;;

(** [one_of generators] creates a generator that picks from one of the given
    [generators], all of which must be printable. Requires at least one
    generator.

    An index is drawn inside a {!Labels.one_of} span and that branch is
    generated compositionally. Each draw records the chosen branch's printer, so
    the drawn value renders through the printer of the branch it actually came
    from; before any draw, the printer defaults to the first branch's. The
    record is per generator value: if one [one_of] generator is drawn several
    times before printing (e.g. as the element generator of [lists]), every
    value renders through the most recently drawn branch's printer. *)
let one_of (generators : ('a, printable) generator list) : ('a, printable) generator =
  match generators with
  | [] -> failwith "one_of requires at least one generator"
  | first :: _ ->
    let cores = Array.of_list (List.map generators ~f:core_of) in
    let printers = Array.of_list (List.map generators ~f:printer) in
    let n = Array.length cores in
    let drawn_printer = ref (printer first) in
    let core =
      Composite
        { label = Labels.one_of
        ; generate_fn =
            (fun data ->
              let idx = Internal.generate_integer data ~min_value:0 ~max_value:(n - 1) in
              drawn_printer := printers.(idx);
              do_draw cores.(idx) data)
        }
    in
    Printable { core; sexp_of = (fun v -> !drawn_printer v) }
;;

(** [optional_core element] is the generation structure behind {!optional}: a
    {!Labels.optional}-spanned draw that yields [None] or [Some] of [element]
    with equal probability. Shared with [Derive.generate_option] so derived
    option fields go through the same machinery. *)
let optional_core (element : 'a core) : 'a option core =
  Composite
    { label = Labels.optional
    ; generate_fn =
        (fun data ->
          if Internal.generate_boolean data 0.5 None
          then Some (do_draw element data)
          else None)
    }
;;

(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element].

    The [None]/[(Some v)] value renders through [Option.sexp_of_t] applied to
    [element]'s printer (the round-trippable form: [()] for [None], [(v)] for
    [Some v]). *)
let optional (element : ('a, printable) generator) : ('a option, printable) generator =
  Printable
    { core = optional_core (core_of element)
    ; sexp_of = Option.sexp_of_t (printer element)
    }
;;

(** [ip_addresses ?version ()] creates a generator for typed [Ipaddr.t] IP
    addresses.

    - [version = Some `V4]: generates IPv4 addresses (RFC 791).
    - [version = Some `V6]: generates IPv6 addresses (RFC 4291).
    - [version = None] (default): generates either version.

    The engine returns the address's raw network-order bytes, which [ipaddr]
    parses into a typed value; render with [Ipaddr.to_string] (RFC 5952
    canonical form for v6). *)
let ip_addresses ?version () =
  let sexp_of ip = Sexp.Atom (Ipaddr.to_string ip) in
  let v4 tc = Ipaddr.V4 (Ipaddr.V4.of_octets_exn (Internal.generate_ipv4 tc)) in
  let v6 tc = Ipaddr.V6 (Ipaddr.V6.of_octets_exn (Internal.generate_ipv6 tc)) in
  match version with
  | Some `V4 -> leaf ~draw:v4 ~sexp_of
  | Some `V6 -> leaf ~draw:v6 ~sexp_of
  | None -> one_of [ leaf ~draw:v4 ~sexp_of; leaf ~draw:v6 ~sexp_of ]
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
