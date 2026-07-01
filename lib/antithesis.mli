(** Antithesis integration for Hegel.

    This module emits a single [always]-typed assertion per Hegel test whose
    [condition] is [true] iff the property test passed. Each Hegel test pass/fail 
    appears in the triage report.

    Outside of Antithesis, [emit_assertion loc ~passed] is a no-op.

    See <https://antithesis.com/docs/reference/sdk/fallback/assert/>
    for the assertion schema. *)

(** A source location identifying a single Hegel test. Used to build the
    Antithesis assertion JSON. *)
type test_location =
  { function_name : string
  ; file : string (** Full source path as captured by [__FILE__]. *)
  ; begin_line : int (** 1-based line number of the test's [let] binding. *)
  }

(** [is_running_in_antithesis ()] returns [true] iff [ANTITHESIS_OUTPUT_DIR]
    is set in the environment and points to an existing directory. Fails if
    [ANTITHESIS_OUTPUT_DIR] does not point to a valid directory. *)
val is_running_in_antithesis : unit -> bool

(** [extract_file_base path] returns the basename of [path] with the extension
    removed (e.g. ["tests/test_basic.ml"] -> ["test_basic"]). *)
val extract_file_base : string -> string

(** [assertion_json loc ~hit ~condition] builds the
    [{ "antithesis_assert": { ... } }] JSON object. Both ID and message are
    [test_function in test_file passes properties]; [must_hit] is always [true];
    [assert_type] is ["always"]; [display_type] is ["Always"]. *)
val assertion_json : test_location -> hit:bool -> condition:bool -> Yojson.Safe.t

(** [write_jsonl_line path json] appends [json] to [path] as a single JSONL
    line (one [\n]-terminated line). Opens with [O_APPEND] *)
val write_jsonl_line : string -> Yojson.Safe.t -> unit

(** [emit_assertion loc ~passed] writes the declaration
    (hit:false, condition:false) and the evaluation
    (hit:true, condition:passed) lines to [$ANTITHESIS_OUTPUT_DIR/sdk.jsonl]
    in a single call. Pre-condition: [is_running_in_antithesis ()] must be
    [true]. *)
val emit_assertion : test_location -> passed:bool -> unit
