open! Core
open Generators_core
open Generators_primitives

(** [sampled_from options] creates a generator that samples uniformly from a
    non-empty list of values.

    Implemented as an integer index generator: picks an index in [0, n-1] and
    returns [options.(index)]. *)
let sampled_from options =
  let arr = Array.of_list options in
  let n = Array.length arr in
  if n = 0 then
    raise (Invalid_argument "sampled_from requires at least one element");
  map (fun i -> arr.(i)) (integers ~min_value:0 ~max_value:(n - 1) ())

(** [one_of generators] creates a generator that picks from one of the given
    [generators].

    Two code paths depending on generator types:
    - All basic: a single [one_of] schema with the raw child schemas. The
      server's response is [\[index, value\]]; the index selects which branch's
      transform to apply.
    - Any non-basic: compositional via [ONE_OF] span.

    Requires at least one generator. *)
let one_of (generators : 'a generator list) =
  if List.length generators = 0 then
    failwith "one_of requires at least one generator";
  let basics = List.filter_map generators ~f:as_basic in
  if List.length basics <> List.length generators then
    (* Composite path: generate index in ONE_OF span, then delegate *)
    let gens = Array.of_list generators in
    let n = Array.length gens in
    Composite
      {
        label = Labels.one_of;
        generate_fn =
          (fun data ->
            let idx =
              Cbor_helpers.extract_int
                (Client.generate_from_schema
                   (`Map
                      [
                        (`Text "type", `Text "integer");
                        (`Text "min_value", `Int 0);
                        (`Text "max_value", `Int (n - 1));
                      ])
                   data)
            in
            do_draw gens.(idx) data);
      }
  else
    (* Basic path: one_of schema with raw child schemas. The server returns
       [index, value]; dispatch through the per-branch transform table. *)
    let child_schemas = List.map basics ~f:fst in
    let transforms = Array.of_list (List.map basics ~f:snd) in
    let dispatch raw =
      match raw with
      | `Array [ raw_idx; value ] ->
          transforms.(Cbor_helpers.extract_int raw_idx) value
      | _ -> failwith "one_of: expected [index, value] from server"
    in
    Basic
      {
        schema =
          `Map
            [
              (`Text "type", `Text "one_of");
              (`Text "generators", `Array child_schemas);
            ];
        transform = dispatch;
      }

(** [optional element] creates a generator that produces either [None] or
    [Some value] from [element].

    Equivalent to [one_of [just None; map (fun x -> Some x) element]]. *)
let optional element = one_of [ just None; map (fun x -> Some x) element ]

(** [ip_addresses ?version ()] creates a generator for IP address strings.

    - [version = Some 4]: generates IPv4 addresses (dotted decimal).
    - [version = Some 6]: generates IPv6 addresses (colon hex).
    - [version = None] (default): generates either IPv4 or IPv6. *)
let rec ip_addresses ?version () =
  match version with
  | Some 4 ->
      Basic
        {
          schema = `Map [ (`Text "type", `Text "ipv4") ];
          transform = Cbor_helpers.extract_string;
        }
  | Some 6 ->
      Basic
        {
          schema = `Map [ (`Text "type", `Text "ipv6") ];
          transform = Cbor_helpers.extract_string;
        }
  | None -> one_of [ ip_addresses ~version:4 (); ip_addresses ~version:6 () ]
  | Some v -> failwith (sprintf "ip_addresses: invalid version %d" v)

(** [tuples2 g1 g2] creates a generator for 2-element tuples.

    When both elements are basic, a single [generate] command is sent using a
    [tuple] schema. Otherwise, elements are generated separately inside a
    {!Labels.tuple} span. *)
let tuples2 (type a b) (g1 : a generator) (g2 : b generator) : (a * b) generator
    =
  match (as_basic g1, as_basic g2) with
  | Some (s1, t1), Some (s2, t2) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple"); (`Text "elements", `Array [ s1; s2 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2 ] -> (t1 v1, t2 v2)
              | _ -> failwith "tuples2: expected 2-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              (a, b));
        }

(** [tuples3 g1 g2 g3] creates a generator for 3-element tuples.

    When all elements are basic, a single [generate] command is sent. Otherwise,
    elements are generated separately inside a {!Labels.tuple} span. *)
let tuples3 (type a b c) (g1 : a generator) (g2 : b generator)
    (g3 : c generator) : (a * b * c) generator =
  match (as_basic g1, as_basic g2, as_basic g3) with
  | Some (s1, t1), Some (s2, t2), Some (s3, t3) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple");
            (`Text "elements", `Array [ s1; s2; s3 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3 ] -> (t1 v1, t2 v2, t3 v3)
              | _ -> failwith "tuples3: expected 3-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              let c = do_draw g3 data in
              (a, b, c));
        }

(** [tuples4 g1 g2 g3 g4] creates a generator for 4-element tuples.

    When all elements are basic, a single [generate] command is sent. Otherwise,
    elements are generated separately inside a {!Labels.tuple} span. *)
let tuples4 (type a b c d) (g1 : a generator) (g2 : b generator)
    (g3 : c generator) (g4 : d generator) : (a * b * c * d) generator =
  match (as_basic g1, as_basic g2, as_basic g3, as_basic g4) with
  | Some (s1, t1), Some (s2, t2), Some (s3, t3), Some (s4, t4) ->
      let combined =
        `Map
          [
            (`Text "type", `Text "tuple");
            (`Text "elements", `Array [ s1; s2; s3; s4 ]);
          ]
      in
      Basic
        {
          schema = combined;
          transform =
            (fun raw ->
              match raw with
              | `Array [ v1; v2; v3; v4 ] -> (t1 v1, t2 v2, t3 v3, t4 v4)
              | _ -> failwith "tuples4: expected 4-element array from server");
        }
  | _ ->
      Composite
        {
          label = Labels.tuple;
          generate_fn =
            (fun data ->
              let a = do_draw g1 data in
              let b = do_draw g2 data in
              let c = do_draw g3 data in
              let d = do_draw g4 data in
              (a, b, c, d));
        }
