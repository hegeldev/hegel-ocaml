(** Source-file splicing for [[@@blobs]] failure-blob recording.

    When a [let%hegel_test] carries a [[@@blobs [...]]] attribute, the PPX
    captures the byte range of the attribute's list-literal payload and
    threads it through to the runtime as a {!t}. On a recording-mode
    failure, the runtime calls {!write_corrected} to produce a sibling
    [<file>.ml.corrected] with the new blob list spliced in. The user
    then accepts the diff (via [dune promote] under the expect-style
    backend, or via [mv] under the standalone backend).

    The recorded strings are the base64 payloads the hegel server returns
    from [encode_failure] — printable ASCII, stored verbatim. *)

(** Source location + currently-recorded blobs for one [[@@blobs ...]]
    attribute occurrence. *)
type t =
  { recorded : string list
  ; file : string
  ; payload_start : int
  ; payload_end : int
  }

(** [write_corrected t ~blobs] writes [<t.file>.corrected] (relative paths
    resolved against [Sys.getcwd ()]) containing the contents of [t.file]
    with the byte range [\[t.payload_start, t.payload_end)] replaced by an
    OCaml list literal whose elements are [blobs]. Each blob is rendered
    via [String.escaped] so non-printable bytes survive a round-trip.
    Calling this also flips the {!any_corrected_written} flag for the
    benefit of inline-tests evaluators. *)
val write_corrected : t -> blobs:string list -> unit

(** [any_corrected_written ()] returns [true] if {!write_corrected} has
    been called at least once in this process. Used by the expect-style
    backend's evaluator: when a corrected file was written, the runner
    must exit 0 so dune's [(diff?)] action runs and stages the file;
    otherwise the evaluator's normal failure-exit signals to dune. *)
val any_corrected_written : unit -> bool
