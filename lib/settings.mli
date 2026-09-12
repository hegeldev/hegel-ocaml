(** Configuration for a Hegel test run.

    Create a {!Settings.t} with {!Settings.create}, {!Settings.default}, or
    {!Settings.from_profile}. Use record update syntax to add settings. Pass
    settings to a [let%hegel_test] with the [[@@settings ...]] attribute:

    {[
    let%hegel_test many_cases tc =
      let n = draw tc (integers ~min_value:0 ~max_value:99 ()) in
      assert (n < 100)
    [@@settings
      { (Settings.create ~test_cases:500 ()) with verbosity = Settings.Verbose }]
    ;;
    ]}

    {2:profiles Profiles}

    Defaults come from the named settings profiles. [base] and [default] are
    reserved names. [base] is the immutable base settings (100 test cases,
    all phases, [Normal] verbosity, no seed, the on-disk database under
    [.hegel/]). [default] is the profile when there is no named profile.
    The following profiles come with Hegel: [development] (the base settings,
    what local runs get), [ci] (selected automatically on CI servers:
    [derandomize] on, the database disabled, the [Too_slow] health check
    suppressed), and [workload] (selected automatically inside Antithesis:
    the database disabled and every health check suppressed).

    Modify an included profile or define a custom one in a [hegel.toml] at your
    project root or the directory [HEGEL_CONFIG] is set to:

    {v
    default = "nightly"   # optional: the default profile for this project

    [profiles.ci]
    test_cases = 1000

    [profiles.nightly]
    test_cases = 10000
    v}

    A custom profile overrides an automatically selected profile. Profiles can
    also extend other profiles with [extends = ...]. Select a profile with
    {!from_profile}. The test-wide default profile can be set with the [default]
    entry in [hegel.toml], the [HEGEL_DEFAULT_PROFILE] environment variable, or
    {!set_default_profile}. Profiles can also be registered from code with
    {!register_profile}.

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
  { test_cases : int (** Number of test cases to run. 100 in the [base] profile. *)
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
    (** Make the run reproducible by deriving its seed from the test's identity
        instead of fresh randomness. [true] in the [ci] profile. *)
  ; database : database
    (** Where failing examples are stored. When set, Hegel replays test cases
        from previous failed runs and records new failures as they occur.
        Disabled in the [ci] and [workload] profiles. *)
  ; suppress_health_check : health_check list
  ; phases : phase list
    (** The phases to run. All phases are run with the [base] profile. *)
  ; print_blob : bool
    (** Print a [rerun with:] line whose base64 blob encodes the choices that
        led to a failure. [true] by default. *)
  ; report_multiple_failures : bool
    (** Report every distinct failure the run found rather than just the first.
        [false] by default. *)
  ; show_statistics : bool
    (** Print an end-of-run statistics block aggregating [event] /
        [event_value] observations. [false] by default. *)
  }

(** [default ()] is the [default] settings profile named by
    {!set_default_profile}, [HEGEL_DEFAULT_PROFILE], or the [default] entry in
    [hegel.toml], else [workload] inside Antithesis, [ci] on a CI server, and
    [development] otherwise. Raises [Usage_error] when a default-profile
    setting names an unknown profile or a discovered [hegel.toml] is
    malformed. *)
val default : unit -> t

(** [from_profile name] is the profile [name] as the libhegel resolves it:
    reserved ([base], [default]), shipped ([development], [ci], [workload]),
    defined in [hegel.toml], or registered with {!register_profile}. A named
    profile still layers over the environment's default (except [base]).
    Raises [Usage_error] for an unknown profile or a malformed [hegel.toml].

    {[
      let%hegel_test thorough tc = ...
      [@@settings Settings.from_profile "nightly"]
    ]} *)
val from_profile : string -> t

(** [create ?test_cases ?seed ()] is {!default} with the two most commonly
    overridden fields applied. [seed] is an [int] here (the field is an
    [int option]). Use record update syntax for any other field:

    {[
      let s = Settings.create ~test_cases:500 ~seed:42 () in
      let s = { s with verbosity = Settings.Verbose }
    ]} *)
val create : ?test_cases:int -> ?seed:int -> unit -> t

(** [register_profile name t] registers a snapshot of [t] as the profile [name]
    for the whole process, replacing any earlier registration of the named profile.
    Settings already created keep their values. [name] may contain ASCII letters,
    digits, [-] and [_]; [base] and [default] are reserved. Raises [Usage_error]
    for an invalid or reserved name. *)
val register_profile : string -> t -> unit

(** [set_default_profile (Some name)] makes the [default] profile resolve to
    [name] for the whole process, taking precedence over [HEGEL_DEFAULT_PROFILE],
    the [default] entry in [hegel.toml], and environment detection. [None]
    clears an earlier call. [name] need not exist yet. Not retroactive. *)
val set_default_profile : string option -> unit

(**/**)

(** [to_ffi ctx t ~database_key] allocates an libhegel settings handle with
    [t] and [database_key]. The caller frees it. *)
val to_ffi
  :  Hegel_ffi.Ffi.context
  -> t
  -> database_key:string option
  -> Hegel_ffi.Ffi.settings

(** [of_ffi ctx s] reads an libhegel settings handle into a record. *)
val of_ffi : Hegel_ffi.Ffi.context -> Hegel_ffi.Ffi.settings -> t

(**/**)
