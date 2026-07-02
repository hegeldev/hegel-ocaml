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
    [cores]. When every core is [Basic], a single [one_of] schema is used and
    the engine's [[index, value]] response selects the branch transform.
    Otherwise an index is drawn inside a {!Labels.one_of} span and that branch
    is generated compositionally. *)
let one_of_core : type a. a core list -> a core =
  fun cores ->
  let basics = List.filter_map cores ~f:as_basic_core in
  if List.length basics <> List.length cores
  then (
    (* Composite path: generate index in ONE_OF span, then delegate. *)
    let gens = Array.of_list cores in
    let n = Array.length gens in
    Composite
      { label = Labels.one_of
      ; generate_fn =
          (fun data ->
            let idx =
              Cbor_helpers.extract_int
                (Internal.generate_from_schema
                   (`Map
                       [ `Text "type", `Text "integer"
                       ; `Text "min_value", `Int 0
                       ; `Text "max_value", `Int (n - 1)
                       ])
                   data)
            in
            do_draw gens.(idx) data)
      })
  else (
    (* Basic path: one_of schema with raw child schemas. The engine returns
       [index, value]; dispatch through the per-branch transform table. *)
    let child_schemas = List.map basics ~f:fst in
    let transforms = Array.of_list (List.map basics ~f:snd) in
    let dispatch raw =
      match raw with
      | `Array [ raw_idx; value ] -> transforms.(Cbor_helpers.extract_int raw_idx) value
      | _ -> failwith "one_of: expected [index, value] from engine"
    in
    (* Distinct raw [index, value] pairs from the engine can map to the same
       OCaml value when two branches produce overlapping outputs, so the
       dispatch transform is not known to preserve uniqueness. *)
    Basic
      { schema =
          `Map [ `Text "type", `Text "one_of"; `Text "generators", `Array child_schemas ]
      ; transform = dispatch
      ; unique_safe = false
      })
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

    Equivalent to [one_of [just None; map (fun x -> Some x) element]]; the
    [None]/[(Some v)] value renders through [Option.sexp_of_t] applied to
    [element]'s printer (the round-trippable form: [()] for [None], [(v)] for
    [Some v]). *)
let optional (element : ('a, printable) generator) : ('a option, printable) generator =
  Printable
    { core = one_of_core [ core_of (just None); core_of (map Option.some element) ]
    ; sexp_of = Option.sexp_of_t (printer element)
    }
;;

(** [ip_addresses ?version ()] creates a generator for IP address strings.

    - [version = Some 4]: generates IPv4 addresses (dotted-decimal, RFC 791).
    - [version = Some 6]: generates IPv6 addresses (colon-hex, RFC 4291).
    - [version = None] (default): generates either IPv4 or IPv6. *)
let rec ip_addresses ?version () =
  match version with
  | Some 4 ->
    basic
      ~schema:(`Map [ `Text "type", `Text "ip_address"; `Text "version", `Int 4 ])
      ~transform:Cbor_helpers.extract_string
      ~sexp_of:sexp_of_string
      ()
  | Some 6 ->
    basic
      ~schema:(`Map [ `Text "type", `Text "ip_address"; `Text "version", `Int 6 ])
      ~transform:Cbor_helpers.extract_string
      ~sexp_of:sexp_of_string
      ()
  | None -> one_of [ ip_addresses ~version:4 (); ip_addresses ~version:6 () ]
  | Some v -> failwith (sprintf "ip_addresses: invalid version %d" v)
;;

(** [tuples2 g1 g2] creates a generator for 2-element tuples of printable
    components. When both are basic, a single [generate] uses a [tuple] schema;
    otherwise elements are generated separately inside a {!Labels.tuple} span. *)
let tuples2 (type a b) (g1 : (a, printable) generator) (g2 : (b, printable) generator)
  : (a * b, printable) generator
  =
  let p1 = printer g1
  and p2 = printer g2 in
  let sexp_of (a, b) = Sexp.List [ p1 a; p2 b ] in
  let core =
    match as_basic_core (core_of g1), as_basic_core (core_of g2) with
    | Some (s1, t1), Some (s2, t2) ->
      Basic
        { schema =
            `Map [ `Text "type", `Text "tuple"; `Text "elements", `Array [ s1; s2 ] ]
        ; transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2 ] -> t1 v1, t2 v2
              | _ -> failwith "tuples2: expected 2-element array from engine")
        ; unique_safe = basic_unique_safe g1 && basic_unique_safe g2
        }
    | _ ->
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
    match
      as_basic_core (core_of g1), as_basic_core (core_of g2), as_basic_core (core_of g3)
    with
    | Some (s1, t1), Some (s2, t2), Some (s3, t3) ->
      Basic
        { schema =
            `Map [ `Text "type", `Text "tuple"; `Text "elements", `Array [ s1; s2; s3 ] ]
        ; transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3 ] -> t1 v1, t2 v2, t3 v3
              | _ -> failwith "tuples3: expected 3-element array from engine")
        ; unique_safe =
            basic_unique_safe g1 && basic_unique_safe g2 && basic_unique_safe g3
        }
    | _ ->
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
    match
      ( as_basic_core (core_of g1)
      , as_basic_core (core_of g2)
      , as_basic_core (core_of g3)
      , as_basic_core (core_of g4) )
    with
    | Some (s1, t1), Some (s2, t2), Some (s3, t3), Some (s4, t4) ->
      Basic
        { schema =
            `Map
              [ `Text "type", `Text "tuple"; `Text "elements", `Array [ s1; s2; s3; s4 ] ]
        ; transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3; v4 ] -> t1 v1, t2 v2, t3 v3, t4 v4
              | _ -> failwith "tuples4: expected 4-element array from engine")
        ; unique_safe =
            basic_unique_safe g1
            && basic_unique_safe g2
            && basic_unique_safe g3
            && basic_unique_safe g4
        }
    | _ ->
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
