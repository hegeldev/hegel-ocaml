# Hegel OCaml SDK

Property-based testing for OCaml, powered by [Hegel](https://github.com/DRMacIver/hegel).

Hegel is a universal property-based testing framework. This SDK communicates
with the Hegel server (powered by Hypothesis) to generate test data and perform
shrinking. When a test fails, Hegel automatically finds the smallest input that
reproduces the failure.

## Installation

### Prerequisites

You need the `hegel` binary on your `PATH`. Install it with pip:

```bash
pip install hegel
```

### Adding to your project

Add `hegel` to your dune project's dependencies:

```dune
(executable
 (name my_tests)
 (libraries hegel))
```

If you want to use the `[@@deriving hegel]` PPX, also add the preprocessor:

```dune
(executable
 (name my_tests)
 (libraries hegel)
 (preprocess (pps ppx_hegel)))
```

## Quick Start

```ocaml
let () =
  Hegel.run (fun () ->
      let x = (Hegel.Gen.int ()).generate () in
      let y = (Hegel.Gen.int ()).generate () in
      assert (x + y = y + x))
```

This tests that integer addition is commutative. Hegel generates random `x` and
`y` values and checks the assertion holds. If it ever fails, Hegel shrinks the
inputs to the smallest reproduction.

## Running Tests

Compile and run with dune:

```bash
dune exec test/my_tests.exe
```

On success, your test prints any output you add (e.g. `Printf.printf "All tests
passed!\n"`). On failure, Hegel prints the shrunk failing inputs and raises
`Failure "Property test failed"`.

## Generators Reference

All generators live in the `Hegel.Gen` module. A generator has type `'a
Hegel.Gen.t` and you draw a value by calling `.generate ()`.

### Primitives

#### `int`

```ocaml
val int : ?min:int -> ?max:int -> unit -> int t
```

Generates integers. Use `~min` and `~max` to constrain the range:

```ocaml
let n = (Gen.int ~min:0 ~max:100 ()).generate ()
(* n is between 0 and 100 inclusive *)
```

#### `float`

```ocaml
val float : ?min:float -> ?max:float -> ?allow_nan:bool -> ?allow_infinity:bool -> unit -> float t
```

Generates floating-point numbers. By default, may include NaN and infinity.
Disable them with `~allow_nan:false` and `~allow_infinity:false`:

```ocaml
let f = (Gen.float ~min:0.0 ~max:1.0 ~allow_nan:false ~allow_infinity:false ()).generate ()
(* f is a finite number between 0.0 and 1.0 *)
```

#### `bool`

```ocaml
val bool : unit -> bool t
```

Generates `true` or `false`.

#### `string`

```ocaml
val string : ?min_size:int -> ?max_size:int -> unit -> string t
```

Generates Unicode strings. Constrain the number of codepoints with `~min_size`
and `~max_size`. Note that `String.length` returns the byte count, which may be
larger than the codepoint count for non-ASCII characters:

```ocaml
let s = (Gen.string ~min_size:1 ~max_size:10 ()).generate ()
(* 1 to 10 Unicode codepoints; String.length s >= 1 *)
```

#### `unit`

```ocaml
val unit : unit -> unit t
```

Always generates `()`. Useful as a placeholder.

### Bounded Numerics

#### `int32`

```ocaml
val int32 : ?min:int32 -> ?max:int32 -> unit -> int32 t
```

Generates 32-bit integers:

```ocaml
let n = (Gen.int32 ~min:(-1000l) ~max:1000l ()).generate ()
```

#### `int64`

```ocaml
val int64 : ?min:int64 -> ?max:int64 -> unit -> int64 t
```

Generates 64-bit integers:

```ocaml
let n = (Gen.int64 ~min:(-1000L) ~max:1000L ()).generate ()
```

### String Variants

#### `from_regex`

```ocaml
val from_regex : ?fullmatch:bool -> string -> string t
```

Generates strings matching a regular expression. Use `~fullmatch:true` to
require the entire string matches (not just a substring):

```ocaml
let s = (Gen.from_regex ~fullmatch:true "[a-z]{3,5}").generate ()
(* s is 3-5 lowercase letters *)
```

#### `binary`

```ocaml
val binary : ?min_size:int -> ?max_size:int -> unit -> string t
```

Generates raw binary data as an OCaml string (arbitrary bytes, not just
printable characters):

```ocaml
let data = (Gen.binary ~min_size:1 ~max_size:256 ()).generate ()
```

### Collections

#### `list`

```ocaml
val list : ?min_size:int -> ?max_size:int -> 'a t -> 'a list t
```

Generates lists of values. Constrain length with `~min_size` and `~max_size`:

```ocaml
let xs = (Gen.list ~min_size:1 ~max_size:10 (Gen.int ())).generate ()
(* List.length xs >= 1 && List.length xs <= 10 *)
```

#### `array`

```ocaml
val array : ?min_size:int -> ?max_size:int -> 'a t -> 'a array t
```

Like `list` but produces an `array`:

```ocaml
let arr = (Gen.array ~max_size:5 (Gen.int ())).generate ()
```

### Tuples

#### `pair`

```ocaml
val pair : 'a t -> 'b t -> ('a * 'b) t
```

Generates 2-tuples:

```ocaml
let (x, y) = (Gen.pair (Gen.int ()) (Gen.string ())).generate ()
```

#### `triple`

```ocaml
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
```

Generates 3-tuples:

```ocaml
let (a, b, c) = (Gen.triple (Gen.int ()) (Gen.bool ()) (Gen.string ())).generate ()
```

### Combinators

#### `map`

```ocaml
val map : ('a -> 'b) -> 'a t -> 'b t
```

Transforms generated values:

```ocaml
let pos = Gen.map abs (Gen.int ())
```

#### `filter`

```ocaml
val filter : ('a -> bool) -> 'a t -> 'a t
```

Keeps only values satisfying a predicate. Use sparingly - if the predicate
rejects too many values, the test case is skipped:

```ocaml
let even = Gen.filter (fun n -> n mod 2 = 0) (Gen.int ~min:0 ~max:100 ())
```

#### `flat_map`

```ocaml
val flat_map : ('a -> 'b t) -> 'a t -> 'b t
```

Chains generators - the output of one generator determines the next:

```ocaml
let sized_string =
  Gen.flat_map
    (fun n -> Gen.string ~min_size:n ~max_size:n ())
    (Gen.int ~min:1 ~max:10 ())
```

#### `one_of`

```ocaml
val one_of : 'a t list -> 'a t
```

Chooses uniformly from a list of generators:

```ocaml
let small_or_large = Gen.one_of [
  Gen.int ~min:0 ~max:10 ();
  Gen.int ~min:1000 ~max:9999 ();
]
```

#### `optional`

```ocaml
val optional : 'a t -> 'a option t
```

Generates `Some value` or `None`:

```ocaml
let maybe_int = (Gen.optional (Gen.int ())).generate ()
```

#### `sampled_from`

```ocaml
val sampled_from : 'a list -> 'a t
```

Chooses uniformly from a list of concrete values:

```ocaml
let color = (Gen.sampled_from ["red"; "green"; "blue"]).generate ()
```

### Constants

#### `just`

```ocaml
val just : Cbor.t -> 'a -> 'a t
```

Always returns the same value. Requires providing the CBOR representation for
the schema:

```ocaml
let always_42 = Gen.just (Cbor.Unsigned 42) 42
```

#### `just_any`

```ocaml
val just_any : 'a -> 'a t
```

Always returns the same value, without a schema (cannot participate in
server-side optimization):

```ocaml
let always_hello = Gen.just_any "hello"
```

### Format Strings

These generators produce strings in common formats:

#### `email`

```ocaml
let e = (Gen.email ()).generate ()
(* e.g. "user@example.com" *)
```

#### `url`

```ocaml
let u = (Gen.url ()).generate ()
(* e.g. "http://example.com/path" *)
```

#### `domain`

```ocaml
val domain : ?max_length:int -> unit -> string t

let d = (Gen.domain ()).generate ()
(* e.g. "example.com" *)
```

#### `ip_address`

```ocaml
val ip_address : ?version:[ `V4 | `V6 ] -> unit -> string t

let v4 = (Gen.ip_address ~version:`V4 ()).generate ()
(* e.g. "192.168.1.1" *)

let v6 = (Gen.ip_address ~version:`V6 ()).generate ()
(* e.g. "2001:db8::1" *)
```

#### `date`, `time`, `datetime`

```ocaml
let d = (Gen.date ()).generate ()     (* e.g. "2024-01-15" *)
let t = (Gen.time ()).generate ()     (* e.g. "14:30:00" *)
let dt = (Gen.datetime ()).generate () (* e.g. "2024-01-15T14:30:00" *)
```

## Test Controls

### `assume`

```ocaml
val assume : bool -> unit
```

Rejects the current test input without counting it as a failure. Use this to
skip inputs that don't meet preconditions:

```ocaml
Hegel.run (fun () ->
    let n = (Gen.int ()).generate () in
    Hegel.assume (n <> 0);
    assert (100 / n = 100 / n))
```

### `note`

```ocaml
val note : string -> unit
```

Records a message that is displayed when a test case fails. Only prints during
the final replay of the failing case:

```ocaml
Hegel.run (fun () ->
    let xs = (Gen.list (Gen.int ())).generate () in
    Hegel.note (Printf.sprintf "Generated list of length %d" (List.length xs));
    assert (List.rev (List.rev xs) = xs))
```

### `target`

```ocaml
val target : ?label:string -> float -> unit
```

Guides the test engine toward higher values of a metric. The engine tries to
maximize the target, which helps find edge cases:

```ocaml
Hegel.run (fun () ->
    let xs = (Gen.list ~max_size:50 (Gen.int ~min:0 ~max:100 ())).generate () in
    let sum = List.fold_left ( + ) 0 xs in
    Hegel.target (Float.of_int sum);
    assert (sum < 10000))
```

### `test_cases`

Control how many test cases to run (default: 100):

```ocaml
Hegel.run ~test_cases:200 (fun () ->
    let n = (Gen.int ()).generate () in
    assert (n = n))
```

## PPX Deriver

The `ppx_hegel` PPX automatically generates generators for your types. Add
`[@@deriving hegel]` to a type definition to get a `gen_<typename>` generator.

### Records

```ocaml
type point = { x: int; y: int } [@@deriving hegel]

(* Generates: val gen_point : point Hegel.Gen.t *)

let () =
  Hegel.run (fun () ->
      let p = gen_point.generate () in
      ignore (p.x + p.y))
```

### Simple Variants

```ocaml
type color = Red | Green | Blue [@@deriving hegel]

(* Generates: val gen_color : color Hegel.Gen.t *)
```

### Variants with Data

```ocaml
type shape =
  | Circle of float
  | Rectangle of int * int
  | Point of point
[@@deriving hegel]

(* Generates: val gen_shape : shape Hegel.Gen.t *)
```

### Variants with Inline Records

```ocaml
type event =
  | Click of { x: int; y: int }
  | KeyPress of { key: string }
[@@deriving hegel]

(* Generates: val gen_event : event Hegel.Gen.t *)
```

### Parametric Types

Type parameters become generator arguments:

```ocaml
type 'a box = { value: 'a } [@@deriving hegel]

(* Generates: val gen_box : 'a Hegel.Gen.t -> 'a box Hegel.Gen.t *)

let () =
  Hegel.run (fun () ->
      let b = (gen_box (Hegel.Gen.int ())).generate () in
      ignore b.value)
```

Multiple type parameters work as well:

```ocaml
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
[@@deriving hegel]

(* Generates: val gen_either : 'a Hegel.Gen.t -> 'b Hegel.Gen.t -> ('a, 'b) either Hegel.Gen.t *)
```

### Recursive Types

The PPX handles recursive types automatically:

```ocaml
type tree =
  | Leaf
  | Node of int * tree * tree
[@@deriving hegel]

(* Generates: val gen_tree : tree Hegel.Gen.t *)
```

### Type Aliases

```ocaml
type int_pair = int * int [@@deriving hegel]

(* Generates: val gen_int_pair : int_pair Hegel.Gen.t *)
```

### Nested Derived Types

Types that reference other derived types compose automatically:

```ocaml
type name = { first: string; last: string } [@@deriving hegel]
type person = { name: name; age: int } [@@deriving hegel]

(* gen_person generates a person with a randomly generated name *)
```

## Building Custom Generators

The `Gen.t` type wraps a generation function:

```ocaml
type 'a t = {
  generate : unit -> 'a;
  as_basic : unit -> 'a basic option;
}
```

Compose generators using `map` and `flat_map`:

```ocaml
(* Generate non-empty sorted lists *)
let sorted_list =
  Gen.map List.sort (Gen.list ~min_size:1 (Gen.int ()))
  |> Gen.map (List.sort compare)

(* Generate pairs where snd > fst *)
let ordered_pair =
  Gen.flat_map
    (fun lo -> Gen.map (fun hi -> (lo, hi)) (Gen.int ~min:lo ~max:1000 ()))
    (Gen.int ~min:0 ~max:999 ())
```

## Debugging

Set `HEGEL_DEBUG=1` to see the raw CBOR requests and responses between the SDK
and the Hegel server:

```bash
HEGEL_DEBUG=1 dune exec test/my_tests.exe
```
