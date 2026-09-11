(** Configuration for a Hegel test run.

    Create a {!Settings.t} with {!Settings.create} or {!Settings.default}.
    Use record update syntax to add settings. Pass settings to a [let%hegel_test]
    with the [[@@settings ...]] attribute:

    {[
    let%hegel_test many_cases tc =
      let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
      assert (n < 100)
    [@@settings
      { (Settings.create ~test_cases:500 ()) with verbosity = Settings.Verbose }]
    ;;
    ]}

    Examples in this documentation assume [open Hegel]. *)

(** Health checks that can be suppressed via the [suppress_health_check]
    field. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(**/**)

(** [health_check_to_string hc] returns the canonical name for [hc]. *)
val health_check_to_string : health_check -> string

(**/**)

(** How much output Hegel produces during a run. *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** Where Hegel stores and replays failing examples. *)
type database =
  | Unset
  | Disabled
  | Path of string

(** Phases of a test run that can be enabled or disabled via the
    [phases] field. *)
type phase =
  | Explicit
  (** Reserved for future use: hegel-ocaml has no explicit-examples facility
      yet, so selecting this phase currently has no effect. *)
  | Reuse (** replay previously failing examples from the {!type:database} *)
  | Generate (** generate new test cases *)
  | Target (** targeted search guided by [target] observations *)
  | Shrink (** shrink discovered counterexamples *)

(**/**)

(** [phase_to_string p] returns the lowercase name for [p]. *)
val phase_to_string : phase -> string

(**/**)

(** Configuration for a test run. Build one with {!default} and refine it with
    record update syntax:

    {[
    let s =
      { (Settings.default ()) with
        verbosity = Settings.Verbose
      ; database = Settings.Path "_hegel_db"
      }
    ;;
    ]} *)
type t =
  { test_cases : int (** Number of test cases to run. Defaults to 100. *)
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
    (** Make the run reproducible by deriving its seed from the test's identity
        instead of fresh randomness. *)
  ; database : database
    (** Where failing examples are stored. When set, Hegel replays test cases
        from previous failed runs and records new failures as they occur. *)
  ; suppress_health_check : health_check list
  ; phases : phase list option
    (** [None] uses the engine's default phase list (all phases); [Some xs]
        restricts execution to [xs]. *)
  ; print_blob : bool
    (** Print a [rerun with:] line whose base64 blob
        encodes the engine choices that led to a failure. [true] by default. *)
  ; report_multiple_failures : bool
    (** Report every distinct failure the run found rather than just the first.
        [false] by default. *)
  ; show_statistics : bool
    (** Print an end-of-run statistics block aggregating [event] /
        [event_value] observations. [false] by default. *)
  }

(** [default ()] creates default test settings, auto-detecting CI. In CI,
    [derandomize] is [true] and the [database] is [Disabled]. *)
val default : unit -> t

(** [create ?test_cases ?seed ()] is {!default} with the two most commonly
    overridden fields applied. [seed] is an [int] here (the field is an
    [int option]). Use record update syntax for any other field:

    {[
      let s = Settings.create ~test_cases:500 ~seed:42 () in
      let s = { s with verbosity = Settings.Verbose }
    ]} *)
val create : ?test_cases:int -> ?seed:int -> unit -> t

(**/**)

(** [is_in_ci ()] returns [true] if a CI environment is detected. *)
val is_in_ci : unit -> bool

(**/**)
