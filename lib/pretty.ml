open! Core
module Ffi = Hegel_ffi.Ffi

(** OCaml-facing wrapper over the engine's pretty-printer documents
    ([hegel_printer_*]).

    A document is built from literal {!text}, {!breakable} points that render
    as their separator when the enclosing group fits on one line and as a
    newline plus indentation when it does not, and {!begin_group} /
    {!end_group} pairs delimiting the groups those decisions are made over.
    The engine owns layout only; everything printed — including the
    {!comment} syntax — is chosen here, in OCaml.

    Two facilities support printing values while generating them:
    {!deferred} opens a hole whose content is written later and spliced in
    when the document renders, and {!begin_speculative} buffers output that a
    rejected draw (a filter retry, a duplicate collection element) can
    retract. *)

(** A handle onto a document (or one of its deferred slots). *)
type t =
  { context : Ffi.context
  ; handle : Ffi.printer
  }

(** The width documents are laid out to, matching the engine's default. *)
let max_width = 79

(** [of_test_case context test_case] fetches (creating on first use) the
    document shared by [test_case]'s family. *)
let of_test_case context test_case =
  { context; handle = Ffi.test_case_printer context test_case ~max_width }
;;

(** [free t] releases this handle onto the document. Content already printed
    stays in the shared document. *)
let free t = Ffi.printer_free t.context t.handle

(** [text t s] emits literal, unbreakable text. Must not contain newlines;
    multi-line content enters the document through {!Ffi.note}, which splits
    it engine-side. *)
let text t s = Ffi.printer_text t.context t.handle s

let breakable t sep = Ffi.printer_breakable t.context t.handle sep

(** [if_break t s] emits [s] only if the innermost open group renders broken;
    a group that fits on one line renders nothing here, and [s] never counts
    toward width. Paired with a {!breakable}, this expresses a separator that
    leads its line in the broken form — [breakable t "; "] then
    [if_break t "; "] renders ["; "] inline and ["\n; "] broken. *)
let if_break t s = Ffi.printer_if_break t.context t.handle s

let hard_break t = Ffi.printer_hard_break t.context t.handle
let begin_group t ~indent s = Ffi.printer_begin_group t.context t.handle ~indent s
let end_group t ~dedent s = Ffi.printer_end_group t.context t.handle ~dedent s

(** [comment t s] attaches [s] to the line being written, rendered in OCaml
    comment syntax as [(* s *)] at the end of that line. Every group open at
    this position breaks — nothing else may share a line with a comment — and
    the comment is excluded from line-width accounting. *)
let comment t s = Ffi.printer_comment t.context t.handle (sprintf "  (* %s *)" s)

let begin_speculative t = Ffi.printer_begin_speculative t.context t.handle
let commit_speculative t = Ffi.printer_commit_speculative t.context t.handle
let abort_speculative t = Ffi.printer_abort_speculative t.context t.handle

(** [deferred t] opens a hole at the current position and returns a handle
    onto its slot; content written to the slot later appears at the hole's
    position when the document renders. *)
let deferred t = { context = t.context; handle = Ffi.printer_deferred t.context t.handle }

(** [value t] splices in any outstanding deferred content, lays the document
    out, and returns everything printed so far. *)
let value t =
  (try Ffi.printer_resolve t.context t.handle with
   | Ffi.Backend_error _ -> ());
  Ffi.printer_value t.context t.handle
;;

(** [sexp t s] renders an S-expression through the document's group machinery:
    atoms print in their escaped form, lists as [(…)] groups whose elements
    separate with a breakable space, so a value that does not fit on one line
    wraps with the layout the rest of the document uses. Inline, the format
    matches [Sexp.to_string_hum]. *)
let rec sexp t (s : Sexp.t) =
  match s with
  | Atom _ -> text t (Sexp.to_string s)
  | List children ->
    begin_group t ~indent:1 "(";
    List.iteri children ~f:(fun i child ->
      if i > 0 then breakable t " ";
      sexp t child);
    end_group t ~dedent:1 ")"
;;
