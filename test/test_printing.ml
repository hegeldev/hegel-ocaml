open! Core
open Hegel
open Generators
module Unix = Core_unix

(* End-to-end tests for draw-time printing and the explain phase: the
   [name = value] document a failing test's final replay writes to stderr,
   with explain-phase annotations attached as [(* … *)] comments to the
   printed regions whose value is irrelevant to the failure. *)

(* Run [f], capturing everything it writes to stderr. *)
let capture_stderr f =
  Out_channel.flush Out_channel.stderr;
  let saved = Unix.dup Unix.stderr in
  let filename, fd = Unix.mkstemp (Core.Filename.temp_dir_name ^/ "hegel_stderr") in
  Unix.dup2 ~src:fd ~dst:Unix.stderr ();
  Unix.close fd;
  let restore () =
    Out_channel.flush Out_channel.stderr;
    Unix.dup2 ~src:saved ~dst:Unix.stderr ();
    Unix.close saved
  in
  let result = Result.try_with f in
  restore ();
  let output = In_channel.read_all filename in
  Unix.unlink filename;
  output, result
;;

(* Run the failing test [body] and return the final replay's stderr output.
   [phases] restricts the run (the default includes the explain phase). *)
let failing_output ?phases ?verbosity ?(test_cases = 50) body =
  let settings = settings ~test_cases ~seed:0 () in
  let settings =
    match phases with
    | None -> settings
    | Some phases -> with_phases phases settings
  in
  let settings =
    match verbosity with
    | None -> settings
    | Some verbosity -> with_verbosity verbosity settings
  in
  let output, result = capture_stderr (fun () -> run_hegel_test ~settings body) in
  (match result with
   | Ok () -> Alcotest.fail "expected the property to fail"
   | Error _ -> ());
  output
;;

let check_contains output needle =
  Alcotest.(check bool)
    (sprintf "output contains %S in %S" needle output)
    true
    (Test_helpers.contains_substring output needle)
;;

let check_absent output needle =
  Alcotest.(check bool)
    (sprintf "output lacks %S in %S" needle output)
    true
    (not (Test_helpers.contains_substring output needle))
;;

let explain_note = "(* or any other generated value *)"

(* An irrelevant leaf draw is annotated; the relevant one is not. *)
let test_leaf_annotations () =
  let output =
    failing_output (fun tc ->
      let ignored =
        draw ~label:"ignored" tc (integers ~min_value:(-100) ~max_value:100 ())
      in
      let b = draw ~label:"b" tc (integers ~min_value:(-100) ~max_value:100 ()) in
      ignore (ignored : int);
      assert (b < 0))
  in
  check_contains output (sprintf "ignored = 0  %s" explain_note);
  check_contains output "b = 0\n";
  check_absent output "b = 0  (*"
;;

(* Disabling the explain phase suppresses the annotations. *)
let test_explain_disabled () =
  let output =
    failing_output ~phases:[ Explicit; Reuse; Generate; Target; Shrink ] (fun tc ->
      let ignored = draw ~label:"ignored" tc (integers ~min_value:0 ~max_value:100 ()) in
      ignore (ignored : int);
      assert false)
  in
  check_contains output "ignored = 0\n";
  check_absent output explain_note
;;

(* A single list element carries its own annotation: the failure needs the
   first element non-negative and the third at least one, so only the middle
   element can vary freely. Comment-forced groups break, one element per
   line, with the close delimiter on its own line. *)
let test_list_element_annotation () =
  let output =
    failing_output (fun tc ->
      let xs =
        draw
          ~label:"xs"
          tc
          (lists
             (integers ~min_value:(-100) ~max_value:100 ())
             ~min_size:3
             ~max_size:3
             ())
      in
      assert (not (List.nth_exn xs 0 >= 0 && List.nth_exn xs 2 >= 1)))
  in
  check_contains output "xs = [ 0\n";
  check_contains output (sprintf "; 0  %s\n" explain_note);
  check_contains output "; 1\n]"
;;

(* Tuple components are annotated individually. *)
let test_tuple_component_annotation () =
  let output =
    failing_output (fun tc ->
      let pair =
        draw
          ~label:"pair"
          tc
          (tuples2
             (integers ~min_value:(-100) ~max_value:100 ())
             (integers ~min_value:(-100) ~max_value:100 ()))
      in
      assert (snd pair < 0))
  in
  check_contains output (sprintf "pair = (0  %s\n, 0\n)" explain_note)
;;

(* Every printable shape draws-and-prints through the document: three- and
   four-tuples, one_of, optional (both arms), and dictionaries. *)
let test_compositional_shapes_print () =
  let int_gen = integers ~min_value:7 ~max_value:7 () in
  let output =
    failing_output (fun tc ->
      let t3 = draw ~label:"t3" tc (tuples3 int_gen int_gen int_gen) in
      ignore (t3 : int * int * int);
      assert false)
  in
  check_contains output "t3 = (7, 7, 7)";
  let output =
    failing_output (fun tc ->
      let t4 = draw ~label:"t4" tc (tuples4 int_gen int_gen int_gen int_gen) in
      ignore (t4 : int * int * int * int);
      assert false)
  in
  check_contains output "t4 = (7, 7, 7, 7)";
  let output =
    failing_output (fun tc ->
      let choice = draw ~label:"choice" tc (one_of [ int_gen ]) in
      ignore (choice : int);
      assert false)
  in
  check_contains output "choice = 7";
  let output =
    failing_output (fun tc ->
      let opt = draw ~label:"opt" tc (optional int_gen) in
      assert (Option.is_some opt))
  in
  check_contains output "opt = None";
  let output =
    failing_output (fun tc ->
      let opt = draw ~label:"opt" tc (optional int_gen) in
      assert (Option.is_none opt))
  in
  check_contains output (sprintf "opt = Some (7)  %s" explain_note);
  let output =
    failing_output (fun tc ->
      let m =
        draw ~label:"m" tc (hashmaps int_gen (booleans ()) ~min_size:1 ~max_size:1 ())
      in
      ignore (m : (int * bool) list);
      assert false)
  in
  check_contains output "m = [ (7, false) ]"
;;

(* Values render as OCaml source: strings quote and escape, floats print
   through {!Generators.float_literal}, and an empty list is [[]]. *)
let test_values_render_as_ocaml_source () =
  let output =
    failing_output (fun tc ->
      let s = draw ~label:"s" tc (text ~min_size:1 ~max_size:1 ~alphabet:"a" ()) in
      ignore (s : string);
      assert false)
  in
  check_contains output {|s = "a"|};
  let output =
    failing_output (fun tc ->
      let f = draw ~label:"f" tc (floats ~min_value:1.5 ~max_value:1.5 ()) in
      ignore (f : float);
      assert false)
  in
  check_contains output "f = 1.5";
  let output =
    failing_output (fun tc ->
      let xs =
        draw ~label:"xs" tc (lists (integers ~min_value:0 ~max_value:0 ()) ~max_size:0 ())
      in
      ignore (xs : int list);
      assert false)
  in
  check_contains output "xs = []"
;;

(* The OCaml-source rendering of every float class, including the special
   values no draw can be pinned to deterministically. *)
let test_float_literal_covers_special_values () =
  Alcotest.(check string) "plain" "1.5" (float_literal 1.5);
  Alcotest.(check string) "nan" "nan" (float_literal Float.nan);
  Alcotest.(check string) "infinity" "infinity" (float_literal Float.infinity);
  Alcotest.(check string) "neg_infinity" "neg_infinity" (float_literal Float.neg_infinity);
  Alcotest.(check string) "escaped string" {|"a\"b"|} (string_literal {|a"b|})
;;

(* A duplicate element rejected while printing leaves no text behind: keys
   come from a single-value domain, so a two-entry unique list must reject at
   least one duplicate on the way. *)
let test_rejected_duplicates_leave_no_text () =
  let output =
    failing_output ~verbosity:Verbose (fun tc ->
      let xs =
        draw
          ~label:"xs"
          tc
          (lists (integers ~min_value:0 ~max_value:1 ()) ~min_size:2 ~unique:true ())
      in
      ignore (xs : int list);
      assert false)
  in
  check_contains output "xs = [ 0; 1 ]"
;;

(* Duplicate dictionary keys likewise retract cleanly while printing. *)
let test_rejected_duplicate_keys_leave_no_text () =
  let output =
    failing_output ~verbosity:Verbose (fun tc ->
      let m =
        draw
          ~label:"m"
          tc
          (hashmaps (integers ~min_value:0 ~max_value:1 ()) (booleans ()) ~min_size:2 ())
      in
      ignore (m : (int * bool) list);
      assert false)
  in
  check_contains output "m = [ (0, false); (1, false) ]"
;;

(* A filter's rejected attempts print nothing; only the accepted value shows.
   Verbose output exercises the retraction while exploration cases print. *)
let test_filtered_draws_print_accepted_value () =
  let output =
    failing_output ~verbosity:Verbose (fun tc ->
      let n =
        draw
          ~label:"n"
          tc
          (filter (fun n -> n % 2 = 1) (integers ~min_value:0 ~max_value:100 ()))
      in
      ignore (n : int);
      assert false)
  in
  check_contains output "n = 1"
;;

(* A filter whose predicate never accepts exhausts its attempts while the
   draw is printing: the partial line is retracted and the case is rejected,
   until the run gives up with a filter-too-much health check. *)
let test_filter_exhausted_while_printing () =
  let output, result =
    capture_stderr (fun () ->
      run_hegel_test
        ~settings:(settings ~test_cases:5 ~seed:0 () |> with_verbosity Verbose)
        (fun tc ->
           let n =
             draw
               ~label:"n"
               tc
               (filter (fun _ -> false) (integers ~min_value:0 ~max_value:100 ()))
           in
           ignore (n : int)))
  in
  (match result with
   | Ok () -> Alcotest.fail "expected the run to be rejected"
   | Error exn ->
     Alcotest.(check bool)
       "reports filtering"
       true
       (Test_helpers.contains_substring (Exn.to_string exn) "filter"));
  check_absent output "n ="
;;

(* Explicitly enabling the explain phase matches the default behavior. *)
let test_explain_enabled_explicitly () =
  let output =
    failing_output
      ~phases:[ Explicit; Reuse; Generate; Target; Shrink; Explain ]
      (fun tc ->
         let free = draw ~label:"free" tc (integers ~min_value:0 ~max_value:100 ()) in
         ignore (free : int);
         assert false)
  in
  check_contains output (sprintf "free = 0  %s" explain_note)
;;

(* When every commented part is invisible (silent draws), the whole-test note
   is dropped too: it would reference nothing the user can see. *)
let test_invisible_together_note_dropped () =
  let output =
    failing_output (fun tc ->
      let hidden_a = draw_silent tc (integers ~min_value:0 ~max_value:100 ()) in
      let hidden_b = draw_silent tc (integers ~min_value:0 ~max_value:100 ()) in
      let v = draw ~label:"v" tc (booleans ()) in
      ignore (hidden_a : int);
      ignore (hidden_b : int);
      assert (not v))
  in
  check_contains output "v = true";
  check_absent output "(* The test";
  check_absent output explain_note
;;

(* When every draw can vary freely, each is annotated and the whole-test
   note leads the report. *)
let test_together_note () =
  let output =
    failing_output (fun tc ->
      let a = draw ~label:"a" tc (booleans ()) in
      let b = draw ~label:"b" tc (booleans ()) in
      ignore (a : bool);
      ignore (b : bool);
      assert false)
  in
  check_contains
    output
    "(* The test always failed when commented parts were varied together. *)";
  check_contains output (sprintf "a = false  %s" explain_note);
  check_contains output (sprintf "b = false  %s" explain_note)
;;

(* Annotations for silent draws have no printed region to attach to: they are
   dropped, and so is a whole-test note that would otherwise reference only
   invisible parts. *)
let test_silent_draw_annotations_dropped () =
  let output =
    failing_output (fun tc ->
      let hidden = draw_silent tc (integers ~min_value:0 ~max_value:100 ()) in
      let visible = draw ~label:"visible" tc (booleans ()) in
      ignore (hidden : int);
      ignore (visible : bool);
      assert false)
  in
  check_contains output (sprintf "visible = false  %s" explain_note);
  check_absent output "hidden"
;;

(* A note made from inside a composite while its enclosing draw is printing
   buffers and flushes after the draw's line, keeping the line intact. *)
let test_note_inside_printing_draw () =
  let gen =
    with_printer
      Int.sexp_of_t
      (composite (fun tc ->
         let n = draw_silent tc (integers ~min_value:3 ~max_value:3 ()) in
         note tc "from inside";
         n))
  in
  let output =
    failing_output (fun tc ->
      let n = draw ~label:"n" tc gen in
      note tc "after";
      ignore (n : int);
      assert false)
  in
  check_contains output (sprintf "n = 3  %s\nfrom inside\nafter" explain_note)
;;

(* A final replay that draws beyond the recorded counterexample unwinds out
   of the printing draw; the partial line is retracted and the run reports
   the nondeterminism. *)
let test_unwinding_printing_draw_is_retracted () =
  let first_run = ref true in
  let output, result =
    capture_stderr (fun () ->
      run_hegel_test ~settings:(settings ~test_cases:5 ~seed:0 ()) (fun tc ->
        let a = draw ~label:"a" tc (booleans ()) in
        ignore (a : bool);
        if !first_run
        then (
          first_run := false;
          assert false)
        else (
          let b = draw ~label:"b" tc (booleans ()) in
          ignore (b : bool);
          assert false)))
  in
  (match result with
   | Ok () -> Alcotest.fail "expected the run to fail"
   | Error exn ->
     Alcotest.(check bool)
       "reports the flaky diagnostic"
       true
       (Test_helpers.contains_substring (Exn.to_string exn) "Flaky test detected"));
  check_absent output "b ="
;;

(* Only the final replay of a failing case prints anything at normal
   verbosity: a passing run leaves stderr untouched. *)
let test_passing_run_prints_nothing () =
  let output, result =
    capture_stderr (fun () ->
      run_hegel_test ~settings:(settings ~test_cases:5 ~seed:0 ()) (fun tc ->
        let n = draw ~label:"n" tc (integers ~min_value:0 ~max_value:9 ()) in
        ignore (n : int)))
  in
  (match result with
   | Ok () -> ()
   | Error exn -> Alcotest.fail (Exn.to_string exn));
  Alcotest.(check string) "no output" "" output
;;

let tests =
  [ Alcotest.test_case "leaf annotations" `Quick test_leaf_annotations
  ; Alcotest.test_case "explain disabled" `Quick test_explain_disabled
  ; Alcotest.test_case "list element annotation" `Quick test_list_element_annotation
  ; Alcotest.test_case "tuple component annotation" `Quick test_tuple_component_annotation
  ; Alcotest.test_case "compositional shapes" `Quick test_compositional_shapes_print
  ; Alcotest.test_case
      "values render as OCaml source"
      `Quick
      test_values_render_as_ocaml_source
  ; Alcotest.test_case
      "float literal special values"
      `Quick
      test_float_literal_covers_special_values
  ; Alcotest.test_case
      "rejected duplicates leave no text"
      `Quick
      test_rejected_duplicates_leave_no_text
  ; Alcotest.test_case
      "rejected duplicate keys leave no text"
      `Quick
      test_rejected_duplicate_keys_leave_no_text
  ; Alcotest.test_case
      "filtered draws print the accepted value"
      `Quick
      test_filtered_draws_print_accepted_value
  ; Alcotest.test_case
      "filter exhausted while printing"
      `Quick
      test_filter_exhausted_while_printing
  ; Alcotest.test_case "explain enabled explicitly" `Quick test_explain_enabled_explicitly
  ; Alcotest.test_case
      "invisible together note dropped"
      `Quick
      test_invisible_together_note_dropped
  ; Alcotest.test_case "together note" `Quick test_together_note
  ; Alcotest.test_case
      "silent draw annotations dropped"
      `Quick
      test_silent_draw_annotations_dropped
  ; Alcotest.test_case "note inside printing draw" `Quick test_note_inside_printing_draw
  ; Alcotest.test_case
      "unwinding printing draw is retracted"
      `Quick
      test_unwinding_printing_draw_is_retracted
  ; Alcotest.test_case "passing run prints nothing" `Quick test_passing_run_prints_nothing
  ]
;;
