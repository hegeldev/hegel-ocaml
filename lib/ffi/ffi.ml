(* Low-level ctypes bindings to libhegel, the native property-based testing
   engine exposed as a C library by hegel-rust (hegel-c/include/hegel.h).

   This module is a thin, mechanical 1:1 wrapper over the C ABI: it locates and
   [dlopen]s the shared library, declares each exported function, and exposes
   OCaml-native wrappers that copy borrowed C buffers into OCaml strings and
   translate negative status codes into exceptions. *)

open Ctypes

(* ------------------------------------------------------------------ *)
(* Locating and opening the shared library                            *)
(* ------------------------------------------------------------------ *)

(* [memoize] is a thread-safe lazy value, used to avoid an eager dlopen in code
   that wants to derive hegel generators but doesn't run the hegel engine and
   hence doesn't require libhegel. *)
let memoize f =
  let mutex = Mutex.create () in
  let value = lazy (f ()) in
  let forced = Atomic.make None in
  fun () ->
    match Atomic.get forced with
    | Some v -> v
    | None ->
      Mutex.protect mutex (fun () ->
        let v = Lazy.force value in
        Atomic.set forced (Some v);
        v)
;;

let lib =
  memoize (fun () ->
    let path = Loader.locate () in
    try Dl.dlopen ~filename:path ~flags:[ Dl.RTLD_NOW; Dl.RTLD_GLOBAL ] with
    | Dl.DL_error msg ->
      failwith (Printf.sprintf "hegel: failed to load libhegel from %s: %s" path msg))
;;

let binding_initializers = ref []

let foreign ?(release_runtime_lock = false) name typ =
  let binding =
    memoize (fun () -> Foreign.foreign ~from:(lib ()) ~release_runtime_lock name typ)
  in
  let initialize () =
    let (_ : _ -> _) = binding () in
    ()
  in
  binding_initializers := initialize :: !binding_initializers;
  fun arg ->
    let f = binding () in
    f arg
;;

(* [hegel_next_test_case] runs the engine on the calling thread. An engine call
   can take a while, so release the OCaml runtime lock for its duration to let
   other OCaml threads run. *)
let foreign_blocking name typ = foreign ~release_runtime_lock:true name typ

(* ------------------------------------------------------------------ *)
(* C structs returned by the typed draws                              *)
(* ------------------------------------------------------------------ *)

(* [hegel_generate_bytes_result_t]: an engine-allocated byte buffer, freed with
   [hegel_generate_bytes_result_free]. *)
module Bytes_result = struct
  type s

  let t : s structure typ = structure "hegel_generate_bytes_result_t"
  let data = field t "data" (ptr uint8_t)
  let len = field t "len" size_t
  let () = seal t
end

(* [hegel_generate_string_result_t]: an engine-allocated UTF-8 buffer (not
   NUL-terminated), freed with [hegel_generate_string_result_free]. *)
module String_result = struct
  type s

  let t : s structure typ = structure "hegel_generate_string_result_t"
  let data = field t "data" (ptr char)
  let len = field t "len" size_t
  let () = seal t
end

(* [hegel_printer_value_result_t]: an engine-allocated UTF-8 buffer (not
   NUL-terminated), freed with [hegel_printer_value_result_free]. *)
module Printer_value_result = struct
  type s

  let t : s structure typ = structure "hegel_printer_value_result_t"
  let data = field t "data" (ptr char)
  let len = field t "len" size_t
  let () = seal t
end

(* [hegel_date_t]: proleptic Gregorian date. [year] in [-999999, 999999]
   (bounded by the range passed to [hegel_generate_date]), [month] in [1, 12],
   [day] in [1, 31]. *)
module Date_struct = struct
  type s

  let t : s structure typ = structure "hegel_date_t"
  let year = field t "year" int32_t
  let month = field t "month" uint8_t
  let day = field t "day" uint8_t
  let () = seal t
end

(* [hegel_time_t]: hour in [0, 23], minute/second in [0, 59], nanosecond in [0, 999999999]. *)
module Time_struct = struct
  type s

  let t : s structure typ = structure "hegel_time_t"
  let hour = field t "hour" uint8_t
  let minute = field t "minute" uint8_t
  let second = field t "second" uint8_t
  let nanosecond = field t "nanosecond" uint32_t
  let () = seal t
end

(* [hegel_datetime_t]: a [hegel_date_t] plus a [hegel_time_t]. *)
module Datetime_struct = struct
  type s

  let t : s structure typ = structure "hegel_datetime_t"
  let date = field t "date" Date_struct.t
  let time = field t "time" Time_struct.t
  let () = seal t
end

(* ------------------------------------------------------------------ *)
(* Test context                                                        *)
(* ------------------------------------------------------------------ *)

let c_context_new = foreign "hegel_context_new" (void @-> returning (ptr void))
let c_context_free = foreign "hegel_context_free" (ptr void @-> returning int)

(* ------------------------------------------------------------------ *)
(* Raw bindings                                                        *)
(* ------------------------------------------------------------------ *)

let c_settings_new =
  foreign "hegel_settings_new" (ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_settings_free =
  foreign "hegel_settings_free" (ptr void @-> ptr void @-> returning int)
;;

let c_settings_backend =
  foreign "hegel_settings_set_backend" (ptr void @-> ptr void @-> int @-> returning int)
;;

let c_settings_test_cases =
  foreign
    "hegel_settings_set_test_cases"
    (ptr void @-> ptr void @-> uint64_t @-> returning int)
;;

let c_settings_verbosity =
  foreign "hegel_settings_set_verbosity" (ptr void @-> ptr void @-> int @-> returning int)
;;

let c_settings_seed =
  foreign
    "hegel_settings_set_seed"
    (ptr void @-> ptr void @-> uint64_t @-> bool @-> returning int)
;;

let c_settings_derandomize =
  foreign
    "hegel_settings_set_derandomize"
    (ptr void @-> ptr void @-> bool @-> returning int)
;;

let c_settings_report_multiple_failures =
  foreign
    "hegel_settings_set_report_multiple_failures"
    (ptr void @-> ptr void @-> bool @-> returning int)
;;

let c_settings_show_statistics =
  foreign
    "hegel_settings_set_show_statistics"
    (ptr void @-> ptr void @-> bool @-> returning int)
;;

let c_settings_database =
  foreign
    "hegel_settings_set_database"
    (ptr void @-> ptr void @-> string_opt @-> returning int)
;;

let c_settings_database_key =
  foreign
    "hegel_settings_set_database_key"
    (ptr void @-> ptr void @-> string_opt @-> returning int)
;;

let c_settings_phases =
  foreign
    "hegel_settings_set_phases"
    (ptr void @-> ptr void @-> uint32_t @-> returning int)
;;

let c_settings_suppress_health_check =
  foreign
    "hegel_settings_set_suppress_health_check"
    (ptr void @-> ptr void @-> uint32_t @-> returning int)
;;

(* [hegel_run_start]'s [callback]/[user_data] (the third and fourth arguments)
   redirect the engine's own output off stderr. We always pass NULL for both,
   keeping it on stderr. *)
let c_run_start =
  foreign
    "hegel_run_start"
    (ptr void @-> ptr void @-> ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_next_test_case =
  foreign_blocking
    "hegel_next_test_case"
    (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_run_result =
  foreign "hegel_run_result" (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_run_free = foreign "hegel_run_free" (ptr void @-> ptr void @-> returning int)

let c_run_result_free =
  foreign "hegel_run_result_free" (ptr void @-> ptr void @-> returning int)
;;

let c_failure_free = foreign "hegel_failure_free" (ptr void @-> ptr void @-> returning int)

let c_test_case_free =
  foreign "hegel_test_case_free" (ptr void @-> ptr void @-> returning int)
;;

let c_test_case_clone =
  foreign
    "hegel_test_case_clone"
    (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_test_case_block =
  foreign
    "hegel_test_case_block"
    (ptr void @-> ptr void @-> uint64_t @-> ptr (ptr void) @-> returning int)
;;

let c_generate_boolean =
  foreign
    "hegel_generate_boolean"
    (ptr void @-> ptr void @-> double @-> bool @-> bool @-> ptr bool @-> returning int)
;;

let c_generate_integer =
  foreign
    "hegel_generate_integer"
    (ptr void @-> ptr void @-> int64_t @-> int64_t @-> ptr int64_t @-> returning int)
;;

let c_generate_float =
  foreign
    "hegel_generate_float"
    (ptr void
     @-> ptr void
     @-> uint32_t
     @-> double
     @-> double
     @-> bool
     @-> bool
     @-> bool
     @-> bool
     @-> double
     @-> ptr double
     @-> returning int)
;;

let c_generate_bytes =
  foreign
    "hegel_generate_bytes"
    (ptr void
     @-> ptr void
     @-> uint64_t
     @-> uint64_t
     @-> ptr Bytes_result.t
     @-> returning int)
;;

let c_generate_bytes_result_free =
  foreign
    "hegel_generate_bytes_result_free"
    (ptr void @-> ptr Bytes_result.t @-> returning int)
;;

let c_string_generator_text =
  foreign
    "hegel_string_generator_text"
    (ptr void
     @-> uint64_t
     (* min_size *) @-> uint64_t (* max_size *)
     @-> string_opt
     (* codec *) @-> uint32_t (* min_codepoint *)
     @-> uint32_t (* max_codepoint *)
     @-> ptr (ptr char) (* categories *)
     @-> size_t
     @-> ptr (ptr char) (* exclude_categories *)
     @-> size_t
     @-> ptr char (* include_characters *)
     @-> size_t
     @-> ptr char (* exclude_characters *)
     @-> size_t
     @-> ptr (ptr void) (* out_generator *)
     @-> returning int)
;;

let c_string_generator_regex =
  foreign
    "hegel_string_generator_regex"
    (ptr void
     @-> string
     (* pattern *) @-> bool (* fullmatch *)
     @-> ptr void (* alphabet (nullable) *)
     @-> ptr (ptr void)
     @-> returning int)
;;

let c_string_generator_email =
  foreign "hegel_string_generator_email" (ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_string_generator_url =
  foreign "hegel_string_generator_url" (ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_string_generator_domain =
  foreign
    "hegel_string_generator_domain"
    (ptr void @-> uint64_t @-> ptr (ptr void) @-> returning int)
;;

let c_string_generator_free =
  foreign "hegel_string_generator_free" (ptr void @-> ptr void @-> returning int)
;;

let c_generate_string =
  foreign
    "hegel_generate_string"
    (ptr void @-> ptr void @-> ptr void @-> ptr String_result.t @-> returning int)
;;

let c_generate_string_result_free =
  foreign
    "hegel_generate_string_result_free"
    (ptr void @-> ptr String_result.t @-> returning int)
;;

let c_generate_date =
  foreign
    "hegel_generate_date"
    (ptr void
     @-> ptr void
     @-> Date_struct.t
     @-> Date_struct.t
     @-> ptr Date_struct.t
     @-> returning int)
;;

let c_generate_time =
  foreign
    "hegel_generate_time"
    (ptr void
     @-> ptr void
     @-> Time_struct.t
     @-> Time_struct.t
     @-> ptr Time_struct.t
     @-> returning int)
;;

let c_generate_datetime =
  foreign
    "hegel_generate_datetime"
    (ptr void
     @-> ptr void
     @-> Datetime_struct.t
     @-> Datetime_struct.t
     @-> ptr Datetime_struct.t
     @-> returning int)
;;

let c_generate_ipv4 =
  foreign "hegel_generate_ipv4" (ptr void @-> ptr void @-> ptr uint8_t @-> returning int)
;;

let c_generate_ipv6 =
  foreign "hegel_generate_ipv6" (ptr void @-> ptr void @-> ptr uint8_t @-> returning int)
;;

let c_test_case_from_blob =
  foreign
    "hegel_test_case_from_blob"
    (ptr void
     @-> ptr void
     @-> string_opt
     @-> ptr void
     @-> ptr void
     @-> ptr (ptr void)
     @-> returning int)
;;

let c_start_span =
  foreign "hegel_start_span" (ptr void @-> ptr void @-> uint64_t @-> returning int)
;;

let c_stop_span =
  foreign "hegel_stop_span" (ptr void @-> ptr void @-> bool @-> returning int)
;;

let c_new_collection =
  foreign
    "hegel_new_collection"
    (ptr void @-> ptr void @-> uint64_t @-> uint64_t @-> ptr (ptr void) @-> returning int)
;;

let c_collection_more =
  foreign
    "hegel_collection_more"
    (ptr void @-> ptr void @-> ptr void @-> ptr bool @-> returning int)
;;

let c_collection_reject =
  foreign
    "hegel_collection_reject"
    (ptr void @-> ptr void @-> ptr void @-> string_opt @-> returning int)
;;

let c_collection_free =
  foreign "hegel_collection_free" (ptr void @-> ptr void @-> returning int)
;;

let c_new_pool =
  foreign "hegel_new_pool" (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_pool_add =
  foreign
    "hegel_pool_add"
    (ptr void @-> ptr void @-> ptr void @-> ptr int64_t @-> returning int)
;;

let c_pool_generate =
  foreign
    "hegel_pool_generate"
    (ptr void @-> ptr void @-> ptr void @-> bool @-> ptr int64_t @-> returning int)
;;

let c_pool_free = foreign "hegel_pool_free" (ptr void @-> ptr void @-> returning int)

let c_new_state_machine =
  foreign
    "hegel_new_state_machine"
    (ptr void
     @-> ptr void
     @-> ptr (ptr char)
     @-> ptr int64_t
     @-> size_t
     @-> ptr (ptr char)
     @-> ptr bool
     @-> size_t
     @-> int64_t
     @-> int64_t
     @-> int64_t
     @-> ptr (ptr void)
     @-> ptr int64_t
     @-> returning int)
;;

let c_state_machine_next_group =
  foreign
    "hegel_state_machine_next_group"
    (ptr void @-> ptr void @-> ptr void @-> ptr int64_t @-> returning int)
;;

let c_state_machine_next_rule =
  foreign
    "hegel_state_machine_next_rule"
    (ptr void @-> ptr void @-> ptr void @-> int64_t @-> ptr int64_t @-> returning int)
;;

let c_state_machine_rule_rejected =
  foreign
    "hegel_state_machine_rule_rejected"
    (ptr void @-> ptr void @-> ptr void @-> int64_t @-> returning int)
;;

let c_state_machine_should_check_invariant =
  foreign
    "hegel_state_machine_should_check_invariant"
    (ptr void @-> ptr void @-> ptr void @-> int64_t @-> ptr bool @-> returning int)
;;

let c_state_machine_free =
  foreign "hegel_state_machine_free" (ptr void @-> ptr void @-> returning int)
;;

let c_target =
  foreign "hegel_target" (ptr void @-> ptr void @-> double @-> string @-> returning int)
;;

let c_event = foreign "hegel_event" (ptr void @-> ptr void @-> string @-> returning int)

let c_event_value =
  foreign
    "hegel_event_value"
    (ptr void @-> ptr void @-> double @-> string @-> returning int)
;;

let text_len_typ = ptr void @-> ptr void @-> string @-> size_t @-> returning int

let c_printer_options_new =
  foreign "hegel_printer_options_new" (ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_printer_options_free =
  foreign "hegel_printer_options_free" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_options_set_max_width =
  foreign
    "hegel_printer_options_set_max_width"
    (ptr void @-> ptr void @-> uint64_t @-> returning int)
;;

let c_printer_new =
  foreign "hegel_printer_new" (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_printer_free = foreign "hegel_printer_free" (ptr void @-> ptr void @-> returning int)
let c_printer_text = foreign "hegel_printer_text" text_len_typ
let c_printer_breakable = foreign "hegel_printer_breakable" text_len_typ
let c_printer_if_break = foreign "hegel_printer_if_break" text_len_typ
let c_printer_comment = foreign "hegel_printer_comment" text_len_typ

let c_printer_hard_break =
  foreign "hegel_printer_hard_break" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_begin_group =
  foreign
    "hegel_printer_begin_group"
    (ptr void @-> ptr void @-> uint64_t @-> string @-> size_t @-> returning int)
;;

let c_printer_end_group = foreign "hegel_printer_end_group" text_len_typ

let c_printer_shift_indent =
  foreign
    "hegel_printer_shift_indent"
    (ptr void @-> ptr void @-> int64_t @-> returning int)
;;

let c_printer_deferred =
  foreign
    "hegel_printer_deferred"
    (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_printer_begin_speculative =
  foreign "hegel_printer_begin_speculative" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_commit_speculative =
  foreign "hegel_printer_commit_speculative" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_abort_speculative =
  foreign "hegel_printer_abort_speculative" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_resolve =
  foreign "hegel_printer_resolve" (ptr void @-> ptr void @-> returning int)
;;

let c_printer_is_live =
  foreign "hegel_printer_is_live" (ptr void @-> ptr void @-> ptr bool @-> returning int)
;;

let c_printer_value =
  foreign
    "hegel_printer_value"
    (ptr void @-> ptr void @-> ptr Printer_value_result.t @-> returning int)
;;

let c_printer_value_result_free =
  foreign
    "hegel_printer_value_result_free"
    (ptr void @-> ptr Printer_value_result.t @-> returning int)
;;

let c_test_case_printer =
  foreign
    "hegel_test_case_printer"
    (ptr void @-> ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
;;

let c_note = foreign "hegel_note" text_len_typ

let c_mark_complete =
  foreign
    "hegel_mark_complete"
    (ptr void @-> ptr void @-> int @-> string_opt @-> returning int)
;;

let c_result_status =
  foreign "hegel_run_result_status" (ptr void @-> ptr void @-> ptr int @-> returning int)
;;

let c_result_error =
  foreign
    "hegel_run_result_error"
    (ptr void @-> ptr void @-> ptr (ptr char) @-> returning int)
;;

let c_result_failure_count =
  foreign
    "hegel_run_result_failure_count"
    (ptr void @-> ptr void @-> ptr size_t @-> returning int)
;;

let c_result_failure =
  foreign
    "hegel_run_result_failure"
    (ptr void @-> ptr void @-> size_t @-> ptr (ptr void) @-> returning int)
;;

let c_failure_blob =
  foreign
    "hegel_failure_reproduction_blob"
    (ptr void @-> ptr void @-> ptr (ptr char) @-> returning int)
;;

let c_failure_origin =
  foreign
    "hegel_failure_origin"
    (ptr void @-> ptr void @-> ptr (ptr char) @-> returning int)
;;

let c_last_error_message =
  foreign "hegel_context_last_error" (ptr void @-> returning string)
;;

let c_version = foreign "hegel_version" (ptr void @-> ptr (ptr char) @-> returning int)

(* ------------------------------------------------------------------ *)
(* Public types                                                        *)
(* ------------------------------------------------------------------ *)

type context = unit Ctypes.ptr
type settings = unit Ctypes.ptr
type run = unit Ctypes.ptr
type test_case = unit Ctypes.ptr
type run_result = unit Ctypes.ptr
type failure = unit Ctypes.ptr
type string_generator = unit Ctypes.ptr
type collection = unit Ctypes.ptr
type pool = unit Ctypes.ptr
type state_machine = unit Ctypes.ptr
type printer = unit Ctypes.ptr
type printer_options = unit Ctypes.ptr

type backend =
  | Default
  | Urandom

type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

type status =
  | Valid
  | Invalid
  | Overrun
  | Interesting

type run_status =
  | Run_passed
  | Run_failed
  | Run_error

exception Stop_test
exception Assume_rejected
exception Backend_error of string
exception Usage_error of string

(* Status codes returned by the C primitives [HEGEL_OK] / [HEGEL_E_*]. *)
let ok = 0
let e_stop_test = -1
let e_assume = -2
let e_backend = -3
let e_invalid_handle = -4
let e_invalid_arg = -5
let e_already_complete = -6
let e_not_complete = -7
let e_internal = -8
let e_concurrent_use = -9

(* Phase bitmask values [HEGEL_PHASE_*]. *)
let phase_explicit = 1
let phase_reuse = 1 lsl 1
let phase_generate = 1 lsl 2
let phase_target = 1 lsl 3
let phase_shrink = 1 lsl 4

(* [HEGEL_PHASE_ALL]: all five phases enabled (the engine default). *)
let phase_all = 31

(* Health-check bitmask values [HEGEL_HC_*]. *)
let hc_filter_too_much = 1
let hc_too_slow = 1 lsl 1
let hc_test_cases_too_large = 1 lsl 2
let hc_large_initial_test_case = 1 lsl 3

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let backend_to_int = function
  | Default -> 1
  | Urandom -> 2
;;

let verbosity_to_int = function
  | Normal -> 0
  | Quiet -> 1
  | Verbose -> 2
  | Debug -> 3
;;

let status_to_int = function
  | Valid -> 0
  | Invalid -> 1
  | Overrun -> 2
  | Interesting -> 3
;;

(* Translate a libhegel return code into success or an exception. *)
let check_rc ctx rc =
  if rc = ok
  then ()
  else if rc = e_stop_test
  then raise Stop_test
  else if rc = e_assume
  then raise Assume_rejected
  else if rc = e_invalid_arg
  then raise (Usage_error (c_last_error_message ctx))
  else (
    let label =
      if rc = e_backend
      then "backend error"
      else if rc = e_invalid_handle
      then "invalid handle"
      else if rc = e_already_complete
      then "test case already complete"
      else if rc = e_not_complete
      then "previous test case not complete"
      else if rc = e_internal
      then "internal error"
      else if rc = e_concurrent_use
      then "concurrent use of a test-case or collection handle"
      else Printf.sprintf "unknown error code %d" rc
    in
    let msg = c_last_error_message ctx in
    let detail = if String.length msg = 0 then "" else ": " ^ msg in
    raise (Backend_error (label ^ detail)))
;;

(* ------------------------------------------------------------------ *)
(* Diagnostics                                                         *)
(* ------------------------------------------------------------------ *)

let version ctx =
  let out = allocate (ptr char) (from_voidp char null) in
  check_rc ctx (c_version ctx out);
  coerce (ptr char) string !@out
;;

let last_error_message ctx = c_last_error_message ctx

(* ------------------------------------------------------------------ *)
(* Test context                                                       *)
(* ------------------------------------------------------------------ *)

let context_free ctx = ignore (c_context_free ctx : int)

let initialize =
  memoize (fun () ->
    (* Resolve the full ABI before exposing a context. *)
    List.iter (fun initialize -> initialize ()) (List.rev !binding_initializers);
    binding_initializers := [])
;;

let context_new () =
  initialize ();
  c_context_new ()
;;

(* ------------------------------------------------------------------ *)
(* Settings                                                            *)
(* ------------------------------------------------------------------ *)

let settings_new ctx =
  let out = allocate (ptr void) null in
  check_rc ctx (c_settings_new ctx out);
  !@out
;;

let settings_free ctx s = check_rc ctx (c_settings_free ctx s)
let settings_backend ctx s b = check_rc ctx (c_settings_backend ctx s (backend_to_int b))

let settings_test_cases ctx s n =
  check_rc ctx (c_settings_test_cases ctx s (Unsigned.UInt64.of_int n))
;;

let settings_verbosity ctx s v =
  check_rc ctx (c_settings_verbosity ctx s (verbosity_to_int v))
;;

let settings_seed ctx s = function
  | Some seed -> check_rc ctx (c_settings_seed ctx s (Unsigned.UInt64.of_int seed) true)
  | None -> check_rc ctx (c_settings_seed ctx s Unsigned.UInt64.zero false)
;;

let settings_derandomize ctx s b = check_rc ctx (c_settings_derandomize ctx s b)

let settings_report_multiple_failures ctx s b =
  check_rc ctx (c_settings_report_multiple_failures ctx s b)
;;

let settings_show_statistics ctx s b = check_rc ctx (c_settings_show_statistics ctx s b)
let settings_database ctx s d = check_rc ctx (c_settings_database ctx s d)
let settings_database_key ctx s k = check_rc ctx (c_settings_database_key ctx s k)

let settings_phases ctx s mask =
  check_rc ctx (c_settings_phases ctx s (Unsigned.UInt32.of_int mask))
;;

let settings_suppress_health_check ctx s mask =
  check_rc ctx (c_settings_suppress_health_check ctx s (Unsigned.UInt32.of_int mask))
;;

(* ------------------------------------------------------------------ *)
(* Run lifecycle                                                       *)
(* ------------------------------------------------------------------ *)

let run_start ctx s =
  let out = allocate (ptr void) null in
  check_rc ctx (c_run_start ctx s null null out);
  !@out
;;

let next_test_case ctx run =
  let out = allocate (ptr void) null in
  check_rc ctx (c_next_test_case ctx run out);
  if is_null !@out then None else Some !@out
;;

let test_case_from_blob ctx s b =
  let out = allocate (ptr void) null in
  check_rc ctx (c_test_case_from_blob ctx s b null null out);
  !@out
;;

let run_result ctx run =
  let out = allocate (ptr void) null in
  check_rc ctx (c_run_result ctx run out);
  !@out
;;

let run_free ctx run = check_rc ctx (c_run_free ctx run)
let run_result_free ctx r = check_rc ctx (c_run_result_free ctx r)
let failure_free ctx f = check_rc ctx (c_failure_free ctx f)
let test_case_free ctx tc = check_rc ctx (c_test_case_free ctx tc)

let test_case_clone ctx tc =
  let out = allocate (ptr void) null in
  check_rc ctx (c_test_case_clone ctx tc out);
  !@out
;;

let test_case_block ctx tc ~indent =
  let out = allocate (ptr void) null in
  check_rc ctx (c_test_case_block ctx tc (Unsigned.UInt64.of_int indent) out);
  !@out
;;

(* ------------------------------------------------------------------ *)
(* Per-test-case primitives                                            *)
(* ------------------------------------------------------------------ *)

(* Marshal an OCaml string list into a [const char *const *] paired with a GC
   root that pins its backing memory. The caller MUST {!Ctypes.Root.release} the
   returned root once the C side has copied the names; until then the root keeps
   the name buffers and the pointer table alive.

   The explicit root is necessary because [CArray.of_list string] stores only
   the raw [char *] pointers and leaves each name's buffer unrooted, so the GC
   may free the names out from under the engine and cause flaky tests *)
let to_string_array names =
  match names with
  | [] -> from_voidp (ptr char) null, Root.create ()
  | _ ->
    let buffers = List.map CArray.of_string names in
    let table = CArray.of_list (ptr char) (List.map CArray.start buffers) in
    CArray.start table, Root.create (buffers, table)
;;

let generate_boolean ctx tc p forced =
  let out_ptr = allocate bool false in
  let rc =
    match forced with
    | Some b -> c_generate_boolean ctx tc p b true out_ptr
    | None -> c_generate_boolean ctx tc p false false out_ptr
  in
  check_rc ctx rc;
  !@out_ptr
;;

let generate_integer ctx tc ~min_value ~max_value =
  let out = allocate int64_t 0L in
  check_rc
    ctx
    (c_generate_integer ctx tc (Int64.of_int min_value) (Int64.of_int max_value) out);
  Int64.to_int !@out
;;

let generate_float
      ctx
      tc
      ~min_value
      ~max_value
      ~allow_nan
      ~allow_infinity
      ~exclude_min
      ~exclude_max
      ~smallest_nonzero_magnitude
  =
  let out = allocate double 0.0 in
  check_rc
    ctx
    (c_generate_float
       ctx
       tc
       (Unsigned.UInt32.of_int 64)
       min_value
       max_value
       allow_nan
       allow_infinity
       exclude_min
       exclude_max
       smallest_nonzero_magnitude
       out);
  !@out
;;

let generate_bytes ctx tc ~min_size ~max_size =
  let result = make Bytes_result.t in
  let max_u =
    match max_size with
    | Some m -> Unsigned.UInt64.of_int m
    | None -> Unsigned.UInt64.max_int
  in
  check_rc
    ctx
    (c_generate_bytes ctx tc (Unsigned.UInt64.of_int min_size) max_u (addr result));
  let n = Unsigned.Size_t.to_int (getf result Bytes_result.len) in
  let data = getf result Bytes_result.data in
  let s = string_from_ptr (coerce (ptr uint8_t) (ptr char) data) ~length:n in
  ignore (c_generate_bytes_result_free ctx (addr result) : int);
  s
;;

(* Marshal an optional OCaml string into a [char *] + byte length. Returns a
   null pointer and length 0 for [None]. The returned [CArray] (via the pointer)
   is kept alive by the caller for the duration of the C call. *)
let optional_bytes_arg = function
  | None -> from_voidp char null, Unsigned.Size_t.of_int 0
  | Some s -> CArray.start (CArray.of_string s), Unsigned.Size_t.of_int (String.length s)
;;

(* Marshal an optional string list into a [const char *const *] + length + GC
   root, distinguishing three cases the text-generator API cares about: [None] →
   NULL (no restriction); [Some []] → a non-NULL pointer with length 0 (an
   explicit *empty* set); [Some names] → the names. *)
let optional_string_array = function
  | None -> from_voidp (ptr char) null, Root.create (), Unsigned.Size_t.of_int 0
  | Some [] ->
    (* A non-NULL pointer with length 0: the C side treats NULL and non-NULL
       empty differently (empty alphabet vs no restriction). *)
    let dummy = CArray.make (ptr char) 1 in
    CArray.start dummy, Root.create dummy, Unsigned.Size_t.of_int 0
  | Some names ->
    let ptr, root = to_string_array names in
    ptr, root, Unsigned.Size_t.of_int (List.length names)
;;

let string_generator_text
      ctx
      ~min_size
      ~max_size
      ~codec
      ~min_codepoint
      ~max_codepoint
      ~categories
      ~exclude_categories
      ~include_characters
      ~exclude_characters
  =
  let max_u =
    match max_size with
    | Some m -> Unsigned.UInt64.of_int m
    | None -> Unsigned.UInt64.max_int
  in
  let cats_ptr, cats_root, cats_len = optional_string_array categories in
  let excats_ptr, excats_root, excats_len = optional_string_array exclude_categories in
  let inc_ptr, inc_len = optional_bytes_arg include_characters in
  let exc_ptr, exc_len = optional_bytes_arg exclude_characters in
  let out = allocate (ptr void) null in
  let rc =
    c_string_generator_text
      ctx
      (Unsigned.UInt64.of_int min_size)
      max_u
      codec
      (Unsigned.UInt32.of_int min_codepoint)
      (Unsigned.UInt32.of_int max_codepoint)
      cats_ptr
      cats_len
      excats_ptr
      excats_len
      inc_ptr
      inc_len
      exc_ptr
      exc_len
      out
  in
  Root.release cats_root;
  Root.release excats_root;
  check_rc ctx rc;
  !@out
;;

let string_generator_regex ctx ~pattern ~fullmatch =
  let out = allocate (ptr void) null in
  check_rc ctx (c_string_generator_regex ctx pattern fullmatch null out);
  !@out
;;

let string_generator_email ctx =
  let out = allocate (ptr void) null in
  check_rc ctx (c_string_generator_email ctx out);
  !@out
;;

let string_generator_url ctx =
  let out = allocate (ptr void) null in
  check_rc ctx (c_string_generator_url ctx out);
  !@out
;;

let string_generator_domain ctx ~max_length =
  let out = allocate (ptr void) null in
  check_rc ctx (c_string_generator_domain ctx (Unsigned.UInt64.of_int max_length) out);
  !@out
;;

let string_generator_free ctx sg = ignore (c_string_generator_free ctx sg : int)

let generate_string ctx tc sg =
  let result = make String_result.t in
  check_rc ctx (c_generate_string ctx tc sg (addr result));
  let n = Unsigned.Size_t.to_int (getf result String_result.len) in
  let data = getf result String_result.data in
  let s = string_from_ptr data ~length:n in
  ignore (c_generate_string_result_free ctx (addr result) : int);
  s
;;

type date =
  { year : int
  ; month : int
  ; day : int
  }

type time =
  { hour : int
  ; minute : int
  ; second : int
  ; nanosecond : int
  }

(* Build the by-value bound structs from [date]/[time] records. *)
let make_date { year; month; day } =
  let d = make Date_struct.t in
  setf d Date_struct.year (Int32.of_int year);
  setf d Date_struct.month (Unsigned.UInt8.of_int month);
  setf d Date_struct.day (Unsigned.UInt8.of_int day);
  d
;;

let make_time { hour; minute; second; nanosecond } =
  let t = make Time_struct.t in
  setf t Time_struct.hour (Unsigned.UInt8.of_int hour);
  setf t Time_struct.minute (Unsigned.UInt8.of_int minute);
  setf t Time_struct.second (Unsigned.UInt8.of_int second);
  setf t Time_struct.nanosecond (Unsigned.UInt32.of_int nanosecond);
  t
;;

let make_datetime (date, time) =
  let dt = make Datetime_struct.t in
  setf dt Datetime_struct.date (make_date date);
  setf dt Datetime_struct.time (make_time time);
  dt
;;

let read_date d =
  { year = Int32.to_int (getf d Date_struct.year)
  ; month = Unsigned.UInt8.to_int (getf d Date_struct.month)
  ; day = Unsigned.UInt8.to_int (getf d Date_struct.day)
  }
;;

let read_time t =
  { hour = Unsigned.UInt8.to_int (getf t Time_struct.hour)
  ; minute = Unsigned.UInt8.to_int (getf t Time_struct.minute)
  ; second = Unsigned.UInt8.to_int (getf t Time_struct.second)
  ; nanosecond = Unsigned.UInt32.to_int (getf t Time_struct.nanosecond)
  }
;;

let generate_date ctx tc ~min_value ~max_value =
  let result = make Date_struct.t in
  check_rc
    ctx
    (c_generate_date ctx tc (make_date min_value) (make_date max_value) (addr result));
  read_date result
;;

let generate_time ctx tc ~min_value ~max_value =
  let result = make Time_struct.t in
  check_rc
    ctx
    (c_generate_time ctx tc (make_time min_value) (make_time max_value) (addr result));
  read_time result
;;

let generate_datetime ctx tc ~min_value ~max_value =
  let result = make Datetime_struct.t in
  check_rc
    ctx
    (c_generate_datetime
       ctx
       tc
       (make_datetime min_value)
       (make_datetime max_value)
       (addr result));
  ( read_date (getf result Datetime_struct.date)
  , read_time (getf result Datetime_struct.time) )
;;

let generate_ip_bytes ctx tc c_fn n =
  let buf = CArray.make uint8_t n in
  check_rc ctx (c_fn ctx tc (CArray.start buf));
  string_from_ptr (coerce (ptr uint8_t) (ptr char) (CArray.start buf)) ~length:n
;;

let generate_ipv4 ctx tc = generate_ip_bytes ctx tc c_generate_ipv4 4
let generate_ipv6 ctx tc = generate_ip_bytes ctx tc c_generate_ipv6 16

let start_span ctx tc label =
  check_rc ctx (c_start_span ctx tc (Unsigned.UInt64.of_int label))
;;

let stop_span ctx tc discard = check_rc ctx (c_stop_span ctx tc discard)

let new_collection ctx tc ~min_size ~max_size =
  let out = allocate (ptr void) null in
  let max_u =
    match max_size with
    | Some m -> Unsigned.UInt64.of_int m
    | None -> Unsigned.UInt64.max_int
  in
  check_rc ctx (c_new_collection ctx tc (Unsigned.UInt64.of_int min_size) max_u out);
  !@out
;;

let collection_more ctx tc collection =
  let out = allocate bool false in
  check_rc ctx (c_collection_more ctx tc collection out);
  !@out
;;

let collection_reject ctx tc collection why =
  check_rc ctx (c_collection_reject ctx tc collection why)
;;

let collection_free ctx collection = check_rc ctx (c_collection_free ctx collection)

let new_pool ctx tc =
  let out = allocate (ptr void) null in
  check_rc ctx (c_new_pool ctx tc out);
  !@out
;;

let pool_add ctx tc ~pool =
  let out = allocate int64_t 0L in
  check_rc ctx (c_pool_add ctx tc pool out);
  Int64.to_int !@out
;;

let pool_generate ctx tc ~pool ~consume =
  let out = allocate int64_t 0L in
  check_rc ctx (c_pool_generate ctx tc pool consume out);
  Int64.to_int !@out
;;

let pool_free ctx pool = check_rc ctx (c_pool_free ctx pool)

let new_state_machine
      ctx
      tc
      ~rule_names
      ~rule_groups
      ~invariant_names
      ~invariants_always_check
      ~min_concurrency
      ~max_concurrency
      ~step_count
  =
  let rules_ptr, rules_root = to_string_array rule_names in
  let groups = CArray.of_list int64_t (List.map Int64.of_int rule_groups) in
  let invariants_always_check = CArray.of_list bool invariants_always_check in
  let invs_ptr, invs_root = to_string_array invariant_names in
  let out = allocate (ptr void) null in
  let out_concurrency = allocate int64_t 0L in
  let rc =
    c_new_state_machine
      ctx
      tc
      rules_ptr
      (CArray.start groups)
      (Unsigned.Size_t.of_int (List.length rule_names))
      invs_ptr
      (CArray.start invariants_always_check)
      (Unsigned.Size_t.of_int (List.length invariant_names))
      (Int64.of_int min_concurrency)
      (Int64.of_int max_concurrency)
      (Int64.of_int step_count)
      out
      out_concurrency
  in
  Root.release rules_root;
  Root.release invs_root;
  check_rc ctx rc;
  !@out, Int64.to_int !@out_concurrency
;;

(* [HEGEL_STATE_MACHINE_DONE]: written to the out parameter by
   [hegel_state_machine_next_group] when the state machine has terminated and by
   [hegel_state_machine_next_rule] when the worker's round is over. *)
let state_machine_done = Int64.min_int

let read_index out =
  let v = !@out in
  if Int64.equal v state_machine_done then None else Some (Int64.to_int v)
;;

let state_machine_next_group ctx tc ~state_machine =
  let out = allocate int64_t 0L in
  check_rc ctx (c_state_machine_next_group ctx tc state_machine out);
  read_index out
;;

let state_machine_next_rule ctx tc ~state_machine ~worker_index =
  let out = allocate int64_t 0L in
  check_rc
    ctx
    (c_state_machine_next_rule ctx tc state_machine (Int64.of_int worker_index) out);
  read_index out
;;

let state_machine_rule_rejected ctx tc ~state_machine ~worker_index =
  check_rc
    ctx
    (c_state_machine_rule_rejected ctx tc state_machine (Int64.of_int worker_index))
;;

let state_machine_should_check_invariant ctx tc ~state_machine ~invariant_index =
  let out = allocate bool false in
  check_rc
    ctx
    (c_state_machine_should_check_invariant
       ctx
       tc
       state_machine
       (Int64.of_int invariant_index)
       out);
  !@out
;;

let state_machine_free ctx state_machine =
  check_rc ctx (c_state_machine_free ctx state_machine)
;;

let target ctx tc value label = check_rc ctx (c_target ctx tc value label)
let event ctx tc label = check_rc ctx (c_event ctx tc label)
let event_value ctx tc value label = check_rc ctx (c_event_value ctx tc value label)

(* ------------------------------------------------------------------ *)
(* Pretty printer                                                      *)
(* ------------------------------------------------------------------ *)

let byte_len s = Unsigned.Size_t.of_int (String.length s)

let opt_handle = function
  | Some p -> p
  | None -> null
;;

let printer_options_new ctx =
  let out = allocate (ptr void) null in
  check_rc ctx (c_printer_options_new ctx out);
  !@out
;;

let printer_options_free ctx options = check_rc ctx (c_printer_options_free ctx options)

let printer_options_set_max_width ctx options max_width =
  check_rc
    ctx
    (c_printer_options_set_max_width ctx options (Unsigned.UInt64.of_int max_width))
;;

let printer_new ctx options =
  let out = allocate (ptr void) null in
  check_rc ctx (c_printer_new ctx (opt_handle options) out);
  !@out
;;

let printer_free ctx printer = check_rc ctx (c_printer_free ctx printer)
let printer_text ctx p s = check_rc ctx (c_printer_text ctx p s (byte_len s))

let printer_breakable ctx p sep =
  check_rc ctx (c_printer_breakable ctx p sep (byte_len sep))
;;

let printer_if_break ctx p s = check_rc ctx (c_printer_if_break ctx p s (byte_len s))
let printer_comment ctx p s = check_rc ctx (c_printer_comment ctx p s (byte_len s))
let printer_hard_break ctx p = check_rc ctx (c_printer_hard_break ctx p)

let printer_begin_group ctx p ~indent open_ =
  check_rc
    ctx
    (c_printer_begin_group ctx p (Unsigned.UInt64.of_int indent) open_ (byte_len open_))
;;

let printer_end_group ctx p close =
  check_rc ctx (c_printer_end_group ctx p close (byte_len close))
;;

let printer_shift_indent ctx p delta =
  check_rc ctx (c_printer_shift_indent ctx p (Int64.of_int delta))
;;

let printer_deferred ctx p =
  let out = allocate (ptr void) null in
  check_rc ctx (c_printer_deferred ctx p out);
  !@out
;;

let printer_begin_speculative ctx p = check_rc ctx (c_printer_begin_speculative ctx p)
let printer_commit_speculative ctx p = check_rc ctx (c_printer_commit_speculative ctx p)
let printer_abort_speculative ctx p = check_rc ctx (c_printer_abort_speculative ctx p)
let printer_resolve ctx p = check_rc ctx (c_printer_resolve ctx p)

let printer_is_live ctx p =
  let out = allocate bool false in
  check_rc ctx (c_printer_is_live ctx p out);
  !@out
;;

let printer_value ctx p =
  let result = make Printer_value_result.t in
  check_rc ctx (c_printer_value ctx p (addr result));
  let n = Unsigned.Size_t.to_int (getf result Printer_value_result.len) in
  let data = getf result Printer_value_result.data in
  let s = string_from_ptr data ~length:n in
  ignore (c_printer_value_result_free ctx (addr result) : int);
  s
;;

let test_case_printer ctx tc options =
  let out = allocate (ptr void) null in
  check_rc ctx (c_test_case_printer ctx tc (opt_handle options) out);
  !@out
;;

let note ctx tc text = check_rc ctx (c_note ctx tc text (byte_len text))

let mark_complete ctx tc status origin =
  check_rc ctx (c_mark_complete ctx tc (status_to_int status) origin)
;;

(* ------------------------------------------------------------------ *)
(* Result inspection                                                   *)
(* ------------------------------------------------------------------ *)

(* [HEGEL_RUN_STATUS_*] values. The catch-all maps any unknown future status to
   [Run_error]. *)
let result_status ctx r =
  let out = allocate int 0 in
  check_rc ctx (c_result_status ctx r out);
  match !@out with
  | 0 -> Run_passed
  | 1 -> Run_failed
  | _ -> Run_error
;;

let result_error ctx r =
  let out = allocate (ptr char) (from_voidp char null) in
  check_rc ctx (c_result_error ctx r out);
  coerce (ptr char) string_opt !@out
;;

let result_failure_count ctx r =
  let out = allocate size_t (Unsigned.Size_t.of_int 0) in
  check_rc ctx (c_result_failure_count ctx r out);
  Unsigned.Size_t.to_int !@out
;;

let result_failure ctx r i =
  let out = allocate (ptr void) null in
  check_rc ctx (c_result_failure ctx r (Unsigned.Size_t.of_int i) out);
  if is_null !@out then None else Some !@out
;;

let result_failures ctx r =
  let n = result_failure_count ctx r in
  List.init n (fun i ->
    match result_failure ctx r i with
    | Some f -> f
    | None -> raise (Backend_error "hegel: failure disappeared mid-iteration"))
;;

let failure_origin ctx f =
  let out = allocate (ptr char) (from_voidp char null) in
  check_rc ctx (c_failure_origin ctx f out);
  coerce (ptr char) string_opt !@out
;;

let failure_blob ctx f =
  let out = allocate (ptr char) (from_voidp char null) in
  check_rc ctx (c_failure_blob ctx f out);
  coerce (ptr char) string_opt !@out
;;
