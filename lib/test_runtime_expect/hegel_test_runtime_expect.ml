(** Register Hegel's test registry as an evaluator for [ppx_inline_test].
    See the accompanying [.mli] for the high-level description. *)

(** [list_partitions_target ()] returns [Some out] when the runner was
    invoked in partition-listing mode (`-list-partitions` writes to
    [stdout]; `-list-partitions-into-file <path>` writes to [path]).
    Returns [None] for normal partition execution. [Ppx_inline_test_lib]
    keeps this state private, so we sniff [Sys.argv]. *)
let list_partitions_target () =
  let argv = Sys.argv in
  let n = Array.length argv in
  let rec find i =
    if i >= n
    then None
    else if String.equal argv.(i) "-list-partitions"
    then Some `Stdout
    else if String.equal argv.(i) "-list-partitions-into-file" && i + 1 < n
    then Some (`File argv.(i + 1))
    else find (i + 1)
  in
  find 0
;;

(** Emit one entry for the default (unnamed) partition so dune knows there
    is a partition to invoke us against. Hegel doesn't use partitions for
    parallelism, so a single empty-string entry is enough. *)
let emit_default_partition = function
  | `Stdout -> print_endline ""
  | `File path ->
    let ch = open_out path in
    output_string ch "\n";
    close_out ch
;;

let () =
  Ppx_inline_test_lib.add_evaluator ~f:(fun () ->
    match list_partitions_target () with
    | Some target ->
      emit_default_partition target;
      Ppx_inline_test_lib.Test_result.Success
    | None ->
      (* Don't chdir. The sandbox's per-library cwd is exactly where
         dune's auto-generated [(diff? <source>.ml <source>.ml.corrected)]
         action looks; [Blobs.resolve_path] uses the basename of
         [pos_fname] against this cwd, landing the [.corrected] file
         alongside the sandbox copy of the source. *)
      let failures = Hegel_test_runtime.run_all () in
      (* If we wrote any [.corrected] file, exit 0 so dune's [(diff?)]
         action runs and stages the file — a non-zero exit would short-
         circuit dune's [progn] and hide the recording. Otherwise,
         propagate the test outcome normally so real failures (replay-
         mode reproductions, stale blobs, non-blob test failures) make
         [dune runtest] report failure. *)
      if Hegel.Blobs.any_corrected_written ()
      then Ppx_inline_test_lib.Test_result.Success
      else if failures > 0
      then Ppx_inline_test_lib.Test_result.Failure
      else Ppx_inline_test_lib.Test_result.Success)
;;
