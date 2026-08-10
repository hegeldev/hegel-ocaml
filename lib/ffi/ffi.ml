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

(* {!Loader.locate} resolves (and, if necessary, downloads) the library path;
   we open it here. The library is loaded on module init — i.e. the first time
   anything in the process touches the Hegel engine. *)
let lib =
  let path = Loader.locate () in
  try Dl.dlopen ~filename:path ~flags:[ Dl.RTLD_NOW; Dl.RTLD_GLOBAL ] with
  | Dl.DL_error msg ->
    failwith (Printf.sprintf "hegel: failed to load libhegel from %s: %s" path msg)
;;

let foreign name typ = Foreign.foreign ~from:lib name typ

(* [hegel_next_test_case] runs the engine on the calling thread. An engine call
   can take a while, so release the OCaml runtime lock for its duration to let 
   other OCaml threads run. *)
let foreign_blocking name typ =
  Foreign.foreign ~from:lib ~release_runtime_lock:true name typ
;;

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

(* [hegel_date_t]: proleptic Gregorian date. [year] in [-999999, 999999]
   (bounded by the range passed to [hegel_generate_date]; this binding requests
   [1, 9999]), [month] in [1, 12], [day] in [1, 31]. *)
module Date_struct = struct
  type s

  let t : s structure typ = structure "hegel_date_t"
  let year = field t "year" int32_t
  let month = field t "month" uint8_t
  let day = field t "day" uint8_t
  let () = seal t
end

(* [hegel_time_t]: hour in [0, 23], minute/second in [0, 59], microsecond in
   [0, 999999]. *)
module Time_struct = struct
  type s

  let t : s structure typ = structure "hegel_time_t"
  let hour = field t "hour" uint8_t
  let minute = field t "minute" uint8_t
  let second = field t "second" uint8_t
  let microsecond = field t "microsecond" uint32_t
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

let c_settings_mode =
  foreign "hegel_settings_set_mode" (ptr void @-> ptr void @-> int @-> returning int)
;;

let c_settings_backend =
  foreign "hegel_settings_set_backend" (ptr void @-> ptr void @-> int @-> returning int)
;;

let c_settings_test_cases =
  foreign
    "hegel_settings_set_test_cases"
    (ptr void @-> ptr void @-> uint64_t @-> returning int)
;;

let c_settings_stateful_step_count =
  foreign
    "hegel_settings_set_stateful_step_count"
    (ptr void @-> ptr void @-> int64_t @-> returning int)
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
     @-> uint64_t (* min_size *)
     @-> uint64_t (* max_size *)
     @-> string_opt (* codec *)
     @-> uint32_t (* min_codepoint *)
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
     @-> string (* pattern *)
     @-> bool (* fullmatch *)
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
     @-> size_t
     @-> ptr (ptr char)
     @-> size_t
     @-> ptr (ptr void)
     @-> returning int)
;;

let c_state_machine_next_rule =
  foreign
    "hegel_state_machine_next_rule"
    (ptr void @-> ptr void @-> ptr void @-> ptr int64_t @-> returning int)
;;

let c_state_machine_rule_rejected =
  foreign
    "hegel_state_machine_rule_rejected"
    (ptr void @-> ptr void @-> ptr void @-> returning int)
;;

let c_state_machine_free =
  foreign "hegel_state_machine_free" (ptr void @-> ptr void @-> returning int)
;;

let c_target =
  foreign "hegel_target" (ptr void @-> ptr void @-> double @-> string @-> returning int)
;;

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

type mode =
  | Test_run
  | Single_test_case

type backend =
  | Auto
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

let mode_to_int = function
  | Test_run -> 0
  | Single_test_case -> 1
;;

let backend_to_int = function
  | Auto -> 0
  | Default -> 1
  | Urandom -> 2
;;

let verbosity_to_int = function
  | Quiet -> 0
  | Normal -> 1
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
  else (
    let label =
      if rc = e_backend
      then "backend error"
      else if rc = e_invalid_handle
      then "invalid handle"
      else if rc = e_invalid_arg
      then "invalid argument"
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

let context_new () = c_context_new ()
let context_free ctx = ignore (c_context_free ctx : int)

(* ------------------------------------------------------------------ *)
(* Settings                                                            *)
(* ------------------------------------------------------------------ *)

let settings_new ctx =
  let out = allocate (ptr void) null in
  check_rc ctx (c_settings_new ctx out);
  !@out
;;

let settings_free ctx s = check_rc ctx (c_settings_free ctx s)
let settings_mode ctx s m = check_rc ctx (c_settings_mode ctx s (mode_to_int m))
let settings_backend ctx s b = check_rc ctx (c_settings_backend ctx s (backend_to_int b))

let settings_test_cases ctx s n =
  check_rc ctx (c_settings_test_cases ctx s (Unsigned.UInt64.of_int n))
;;

let settings_stateful_step_count ctx s n =
  check_rc ctx (c_settings_stateful_step_count ctx s (Int64.of_int n))
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
   root, distinguishing three cases the text-generator API cares about:
   [None] → NULL (no restriction); [Some []] → a non-NULL pointer with length 0
   (an explicit *empty* set); [Some names] → the names. *)
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

(* Build the by-value bound structs. The OCaml [dates]/[times]/[datetimes]
   generators expose no bounds, so the wrappers always pass the conventional
   full range: dates {1,1,1}..{9999,12,31}, times {0,0,0,0}..{23,59,59,999999}. *)
let make_date ~year ~month ~day =
  let d = make Date_struct.t in
  setf d Date_struct.year (Int32.of_int year);
  setf d Date_struct.month (Unsigned.UInt8.of_int month);
  setf d Date_struct.day (Unsigned.UInt8.of_int day);
  d
;;

let make_time ~hour ~minute ~second ~microsecond =
  let t = make Time_struct.t in
  setf t Time_struct.hour (Unsigned.UInt8.of_int hour);
  setf t Time_struct.minute (Unsigned.UInt8.of_int minute);
  setf t Time_struct.second (Unsigned.UInt8.of_int second);
  setf t Time_struct.microsecond (Unsigned.UInt32.of_int microsecond);
  t
;;

let make_datetime date time =
  let dt = make Datetime_struct.t in
  setf dt Datetime_struct.date date;
  setf dt Datetime_struct.time time;
  dt
;;

let date_min = make_date ~year:1 ~month:1 ~day:1
let date_max = make_date ~year:9999 ~month:12 ~day:31
let time_min = make_time ~hour:0 ~minute:0 ~second:0 ~microsecond:0
let time_max = make_time ~hour:23 ~minute:59 ~second:59 ~microsecond:999999
let datetime_min = make_datetime date_min time_min
let datetime_max = make_datetime date_max time_max

let generate_date ctx tc =
  let result = make Date_struct.t in
  check_rc ctx (c_generate_date ctx tc date_min date_max (addr result));
  ( Int32.to_int (getf result Date_struct.year)
  , Unsigned.UInt8.to_int (getf result Date_struct.month)
  , Unsigned.UInt8.to_int (getf result Date_struct.day) )
;;

let generate_time ctx tc =
  let result = make Time_struct.t in
  check_rc ctx (c_generate_time ctx tc time_min time_max (addr result));
  ( Unsigned.UInt8.to_int (getf result Time_struct.hour)
  , Unsigned.UInt8.to_int (getf result Time_struct.minute)
  , Unsigned.UInt8.to_int (getf result Time_struct.second)
  , Unsigned.UInt32.to_int (getf result Time_struct.microsecond) )
;;

let generate_datetime ctx tc =
  let result = make Datetime_struct.t in
  check_rc ctx (c_generate_datetime ctx tc datetime_min datetime_max (addr result));
  let d = getf result Datetime_struct.date in
  let t = getf result Datetime_struct.time in
  ( ( Int32.to_int (getf d Date_struct.year)
    , Unsigned.UInt8.to_int (getf d Date_struct.month)
    , Unsigned.UInt8.to_int (getf d Date_struct.day) )
  , ( Unsigned.UInt8.to_int (getf t Time_struct.hour)
    , Unsigned.UInt8.to_int (getf t Time_struct.minute)
    , Unsigned.UInt8.to_int (getf t Time_struct.second)
    , Unsigned.UInt32.to_int (getf t Time_struct.microsecond) ) )
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

let new_state_machine ctx tc ~rule_names ~invariant_names =
  let rules_ptr, rules_root = to_string_array rule_names in
  let invs_ptr, invs_root = to_string_array invariant_names in
  let out = allocate (ptr void) null in
  let rc =
    c_new_state_machine
      ctx
      tc
      rules_ptr
      (Unsigned.Size_t.of_int (List.length rule_names))
      invs_ptr
      (Unsigned.Size_t.of_int (List.length invariant_names))
      out
  in
  Root.release rules_root;
  Root.release invs_root;
  check_rc ctx rc;
  !@out
;;

(* [HEGEL_STATE_MACHINE_DONE]: written to the out parameter by
   [hegel_state_machine_next_rule] when the engine's step budget for the test
   case is exhausted and the caller should stop running rules. *)
let state_machine_done = -1

let state_machine_next_rule ctx tc ~state_machine =
  let out = allocate int64_t 0L in
  check_rc ctx (c_state_machine_next_rule ctx tc state_machine out);
  let index = Int64.to_int !@out in
  if index = state_machine_done then None else Some index
;;

let state_machine_rule_rejected ctx tc ~state_machine =
  check_rc ctx (c_state_machine_rule_rejected ctx tc state_machine)
;;

let state_machine_free ctx state_machine =
  check_rc ctx (c_state_machine_free ctx state_machine)
;;

let target ctx tc value label = check_rc ctx (c_target ctx tc value label)

let mark_complete ctx tc status origin =
  check_rc ctx (c_mark_complete ctx tc (status_to_int status) origin)
;;

(* ------------------------------------------------------------------ *)
(* Result inspection                                                   *)
(* ------------------------------------------------------------------ *)

(* [HEGEL_RUN_STATUS_*] values. The catch-all maps any unknown future status
   to [Run_error]. *)
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
