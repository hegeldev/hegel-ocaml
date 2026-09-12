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

    All examples below assume [open Hegel].

    {2:layers Settings layers}

    A run's settings are built in layers. Each layer overrides what the
    layers below it set. The layers from lowest to highest are:

    + {b The base settings}: the base values noted on {!t}'s fields.
      Immutable, and available by name as the reserved [base] profile.
    + {b The profile}: a named set of overrides resolved by libhegel from
      {{!builtin}built-in profiles}, [hegel.toml], and {!register_profile}.
    + {b Settings in the test code}: {!create}'s arguments and record
      updates on the value the profile produced.

    [HEGEL_DEFAULT_PROFILE] and [HEGEL_CONFIG] come from the environment but
    act on the profile layer, choosing the default profile and the config
    file.

    {2:profiles Profiles}

    A profile is a named set of setting overrides and the profile it
    extends.

    {3 Reserved profile names}

    - {b [base]} is the immutable base settings, and the root of the profile
      inheritance chain.
    - {b [default]} is the profile a test uses when no profile is named.
      Custom profiles extend [default] when they do not explicitly extend
      another profile. See {{!choosing}Choosing the default profile}.

    Neither can be modified or registered. It is an error to name [default] as
    its own target ([default = "default"] or [HEGEL_DEFAULT_PROFILE=default]).

    {3:builtin Built-in profiles}

    Three profiles come with Hegel. [ci] and [workload] extend [base]
    directly. [development] is registered by hegel-ocaml as the base settings
    with [print_blob = true], so it has no parent.

    {t
      | Profile       | Overrides                                                                                                  | When it is the environment's profile                                |
      |---------------|------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|
      | [development] | [print_blob = true]                                                                                        | Whenever neither of the others applies (usually local development). |
      | [ci]          | [derandomize = true], [database = "disabled"], [suppress_health_check = ["too_slow"]], [print_blob = true] | On a CI server, detected from the variables common CI servers set.  |
      | [workload]    | [backend = "urandom"], [database = "disabled"], [suppress_health_check = ["all"]]                          | Inside Antithesis, detected from [ANTITHESIS_OUTPUT_DIR].           |
    }

    {3 Custom profiles and inheritance}

    Custom profiles are defined in [hegel.toml] or registered with
    {!register_profile}. A [hegel.toml] profile can choose its parent with
    [extends]. Without it, a custom profile extends [default] and a built-in
    profile extends [base]. If the default profile is a custom profile without
    an explicit [extends], then its parent is the environment's profile.

    Example:

    {v
    [profiles.nightly]
    test_cases = 10000

    [profiles.pinned]
    extends = "base"
    test_cases = 10000

    [profiles.deep-ci]
    extends = "ci"
    test_cases = 100000
    v}

    - [nightly] resolves as [nightly] -> [ci] -> [base] on a CI server and
      [nightly] -> [development] -> [base] locally.
    - [pinned] resolves as [pinned] -> [base] everywhere.
    - [deep-ci] resolves as [deep-ci] -> [ci] -> [base] everywhere.

    A [hegel.toml] section for a built-in profile only overrides the set
    fields. For example, a [[profiles.ci]] with [print_blob = false] keeps
    derandomization and the disabled database. [ci] and [workload] may
    set [extends] to change its parent, but [development] may not. This
    will be fixed in the near future.

    {3:ascode Profiles as code}

    {!register_profile} stores a copy of a {!Settings.t} under a name
    process-wide:

    {[
    Settings.register_profile
      "nightly"
      { (Settings.from_profile "ci") with test_cases = 10_000 }
    ]}

    A registered profile has no parent. A [hegel.toml] section of
    the same name still overrides the set fields, but may not set [extends]
    on it. Registering the profile again replaces the existing profile.

    {3:choosing Choosing the default profile}

    The [default] alias resolves to one of the following from highest to
    least precedence:

    + {!set_default_profile}.
    + The [HEGEL_DEFAULT_PROFILE] environment variable.
    + The top-level [default = "<profile>"] entry in [hegel.toml].
    + The environment's profile: [workload] inside Antithesis, [ci] on
      a CI server, else [development].

    {!from_profile} resolves a profile by name for one test without changing
    what [default] is.

    {2 [hegel.toml]}
    {3 Discovery}

    libhegel looks for [hegel.toml] in the test process's working directory
    and then in each ancestor up to the filesystem root, stopping at the
    first one found. The file is read once then parsed into a [Settings.t]
    value. Editing the file or changing [HEGEL_CONFIG] after this happens has
    no effect.

    Set [HEGEL_CONFIG] to the file's path to load it directly. A non-empty
    [HEGEL_CONFIG] that cannot be read is an error.

    {3 Format}

    [hegel.toml] consists of an optional top-level [default = "<profile>"]
    entry and [[profiles.<name>]] tables whose entries are the settings keys. An unknown
    key, a value of the wrong type, or any other top-level key is an error.
    Profile names use ASCII letters, digits, [-] and [_]. [base] and [default]
    are reserved and cannot be sections. See the {!types} section for valid
    [hegel.toml] keys and values.

    {v
    default = "nightly"      # optional: the default profile for this project

    [profiles.development]
    test_cases = 200

    [profiles.ci]            # merges onto the built-in ci profile
    test_cases = 1000
    print_blob = false

    [profiles.nightly]
    extends = "ci"
    test_cases = 10000
    seed = "none"
    suppress_health_check = ["too_slow", "filter_too_much"]
    phases = ["explicit", "reuse", "generate", "target", "shrink"]
    database = "default"
    backend = "default"
    v}

    [seed = "none"] clears an inherited seed, and [database = "default"]
    restores the default database after a parent disabled it or set a path.

    {2:env Environment variables}

    {t
      | Variable                    | Read by               | Effect                                                                         |
      |-----------------------------|-----------------------|--------------------------------------------------------------------------------|
      | [HEGEL_DEFAULT_PROFILE]     | profile resolution    | The default profile, unless {!set_default_profile} set one.                    |
      | [HEGEL_CONFIG]              | config loading        | Path of the [hegel.toml] to load                                               |
      | [ANTITHESIS_OUTPUT_DIR]     | environment detection | Selects the [workload] profile. A run fails if the named directory is missing. |
      | [CI], [GITHUB_ACTIONS], ... | environment detection | Selects the [ci] profile.                                                      |
    } *)

(** {2:types Types} *)

(** Health checks that can be suppressed via the [suppress_health_check]
    field. In [hegel.toml]: [filter_too_much], [too_slow],
    [test_cases_too_large], [large_initial_test_case], or [all]. *)
type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

(**/**)

(** [health_check_to_string hc] returns the canonical name for [hc]. *)
val health_check_to_string : health_check -> string

(**/**)

(** How much output Hegel produces during a run. In [hegel.toml]: [quiet],
    [normal], [verbose], or [debug]. *)
type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

(** Where Hegel stores and replays failing examples. In [hegel.toml]:
    [default], [disabled], or a path. *)
type database =
  | Unset (** Defaults to [.hegel/examples] under the working directory *)
  | Disabled (** No database. *)
  | Path of string (** A string representing the database path. *)

(** The source of randomness. In [hegel.toml]: [default] or [urandom]. *)
type backend =
  | Default
  (** A seeded PRNG: runs are reproducible from the seed, and shrinking and
      replay work as usual. The base setting. *)
  | Urandom
  (** Reads bytes from [/dev/urandom] on every draw, so Antithesis's fuzzer
      controls every choice. Selected by the [workload] profile. You almost
      certainly don't want it otherwise. *)

(** Phases of a test run that can be enabled or disabled via the [phases]
    field. In [hegel.toml]: [explicit], [reuse], [generate], [target], or [shrink]. *)
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

(** Configuration for a test run. Create with {!default} and refine it with
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
  ; verbosity : verbosity (** How much Hegel prints. [Normal] in the [base] profile. *)
  ; seed : int option
    (** A fixed seed for reproducibility; [None]/[seed = "none"] in [hegel.toml]
        draws a new random seed per run. *)
  ; derandomize : bool
    (** Make the run reproducible by deriving its seed from the test's identity
        instead of fresh randomness. [true] in the [ci] profile. *)
  ; database : database
    (** Where failing examples are stored. When set, Hegel replays test cases
        from previous failed runs and records new failures as they occur.
        Disabled in the [ci] and [workload] profiles. *)
  ; suppress_health_check : health_check list
    (** Health checks that should not fail the run. [[Too_slow]] in the [ci]
        profile, all four in [workload]. *)
  ; phases : phase list
    (** The phases to run. All phases are run with the [base] profile. *)
  ; print_blob : bool
    (** Print a [rerun with:] line whose base64 blob encodes the choices that
        led to a failure. [true] in the [development] and [ci] profiles, [false]
        in [base] and [workload]. *)
  ; report_multiple_failures : bool
    (** Report every distinct failure the run found rather than just the first.
        [false] by default. *)
  ; show_statistics : bool
    (** Print an end-of-run statistics block aggregating [event] /
        [event_value] observations. [false] by default. *)
  ; backend : backend
    (** The source of randomness. [Default] in the [base] profile, [Urandom]
        in the [workload] profile. *)
  }

(** {2:functions Functions} *)

(** [default ()] is the [default] settings profile named by
    {!set_default_profile}, [HEGEL_DEFAULT_PROFILE], or the [default] entry in
    [hegel.toml], else [workload] inside Antithesis, [ci] on a CI server, and
    [development] otherwise. Raises [Usage_error] when a default-profile
    setting names an unknown profile or a discovered [hegel.toml] is
    malformed. *)
val default : unit -> t

(** [from_profile name] is the profile [name] as libhegel resolves it:
    reserved ([base], [default]), built in ([development], [ci], [workload]),
    defined in [hegel.toml], or registered with {!register_profile}. A custom
    profile without [extends] still extends [default], so it inherits the
    environment's profile wherever it is selected from. Raises [Usage_error]
    for an unknown profile or a malformed [hegel.toml].

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
    clears an earlier call. [name] need not exist yet. It is checked at the
    next profile resolution. *)
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
