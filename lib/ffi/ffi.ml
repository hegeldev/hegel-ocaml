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

(* [hegel_next_test_case] blocks on the engine's worker-thread channel, so it
   must release the OCaml runtime lock while it waits. *)
let foreign_blocking name typ =
  Foreign.foreign ~from:lib ~release_runtime_lock:true name typ
;;

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

let c_run_start =
  foreign "hegel_run_start" (ptr void @-> ptr void @-> ptr (ptr void) @-> returning int)
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

let c_test_case_free =
  foreign "hegel_test_case_free" (ptr void @-> ptr void @-> returning int)
;;

let c_generate =
  foreign
    "hegel_generate"
    (ptr void
     @-> ptr void
     @-> ptr char
     @-> size_t
     @-> ptr (ptr char)
     @-> ptr size_t
     @-> returning int)
;;

let c_primitive_boolean =
  foreign
    "hegel_primitive_boolean"
    (ptr void @-> ptr void @-> double @-> bool @-> bool @-> ptr bool @-> returning int)
;;

let c_test_case_from_blob =
  foreign
    "hegel_test_case_from_blob"
    (ptr void @-> ptr void @-> string_opt @-> ptr (ptr void) @-> returning int)
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
    (ptr void @-> ptr void @-> uint64_t @-> uint64_t @-> ptr int64_t @-> returning int)
;;

let c_collection_more =
  foreign
    "hegel_collection_more"
    (ptr void @-> ptr void @-> int64_t @-> ptr bool @-> returning int)
;;

let c_collection_reject =
  foreign
    "hegel_collection_reject"
    (ptr void @-> ptr void @-> int64_t @-> string_opt @-> returning int)
;;

let c_new_pool =
  foreign "hegel_new_pool" (ptr void @-> ptr void @-> ptr int64_t @-> returning int)
;;

let c_pool_add =
  foreign
    "hegel_pool_add"
    (ptr void @-> ptr void @-> int64_t @-> ptr int64_t @-> returning int)
;;

let c_pool_generate =
  foreign
    "hegel_pool_generate"
    (ptr void @-> ptr void @-> int64_t @-> bool @-> ptr int64_t @-> returning int)
;;

let c_new_state_machine =
  foreign
    "hegel_new_state_machine"
    (ptr void
     @-> ptr void
     @-> ptr (ptr char)
     @-> size_t
     @-> ptr (ptr char)
     @-> size_t
     @-> ptr int64_t
     @-> returning int)
;;

let c_state_machine_next_rule =
  foreign
    "hegel_state_machine_next_rule"
    (ptr void @-> ptr void @-> int64_t @-> ptr int64_t @-> returning int)
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
  check_rc ctx (c_run_start ctx s out);
  !@out
;;

let next_test_case ctx run =
  let out = allocate (ptr void) null in
  check_rc ctx (c_next_test_case ctx run out);
  if is_null !@out then None else Some !@out
;;

let test_case_from_blob ctx s b =
  let out = allocate (ptr void) null in
  check_rc ctx (c_test_case_from_blob ctx s b out);
  !@out
;;

let run_result ctx run =
  let out = allocate (ptr void) null in
  check_rc ctx (c_run_result ctx run out);
  !@out
;;

let run_free ctx run = check_rc ctx (c_run_free ctx run)
let blob_test_case_free ctx tc = check_rc ctx (c_test_case_free ctx tc)

(* ------------------------------------------------------------------ *)
(* Per-test-case primitives                                            *)
(* ------------------------------------------------------------------ *)

let generate ctx tc schema =
  let len = String.length schema in
  let arr = CArray.of_string schema in
  let p = CArray.start arr in
  let out_ptr = allocate (ptr char) (from_voidp char null) in
  let out_len = allocate size_t (Unsigned.Size_t.of_int 0) in
  let rc = c_generate ctx tc p (Unsigned.Size_t.of_int len) out_ptr out_len in
  check_rc ctx rc;
  let n = Unsigned.Size_t.to_int !@out_len in
  string_from_ptr !@out_ptr ~length:n
;;

let primitive_boolean ctx tc p forced =
  let out_ptr = allocate bool false in
  let rc =
    match forced with
    | Some b -> c_primitive_boolean ctx tc p b true out_ptr
    | None -> c_primitive_boolean ctx tc p false false out_ptr
  in
  check_rc ctx rc;
  !@out_ptr
;;

let start_span ctx tc label =
  check_rc ctx (c_start_span ctx tc (Unsigned.UInt64.of_int label))
;;

let stop_span ctx tc discard = check_rc ctx (c_stop_span ctx tc discard)

let new_collection ctx tc ~min_size ~max_size =
  let out = allocate int64_t 0L in
  let max_u =
    match max_size with
    | Some m -> Unsigned.UInt64.of_int m
    | None -> Unsigned.UInt64.max_int
  in
  check_rc ctx (c_new_collection ctx tc (Unsigned.UInt64.of_int min_size) max_u out);
  Int64.to_int !@out
;;

let collection_more ctx tc id =
  let out = allocate bool false in
  check_rc ctx (c_collection_more ctx tc (Int64.of_int id) out);
  !@out
;;

let collection_reject ctx tc id why =
  check_rc ctx (c_collection_reject ctx tc (Int64.of_int id) why)
;;

let new_pool ctx tc =
  let out = allocate int64_t 0L in
  check_rc ctx (c_new_pool ctx tc out);
  Int64.to_int !@out
;;

let pool_add ctx tc ~pool_id =
  let out = allocate int64_t 0L in
  check_rc ctx (c_pool_add ctx tc (Int64.of_int pool_id) out);
  Int64.to_int !@out
;;

let pool_generate ctx tc ~pool_id ~consume =
  let out = allocate int64_t 0L in
  check_rc ctx (c_pool_generate ctx tc (Int64.of_int pool_id) consume out);
  Int64.to_int !@out
;;

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

let new_state_machine ctx tc ~rule_names ~invariant_names =
  let rules_ptr, rules_root = to_string_array rule_names in
  let invs_ptr, invs_root = to_string_array invariant_names in
  let out = allocate int64_t 0L in
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
  Int64.to_int !@out
;;

let state_machine_next_rule ctx tc ~state_machine_id =
  let out = allocate int64_t 0L in
  check_rc ctx (c_state_machine_next_rule ctx tc (Int64.of_int state_machine_id) out);
  Int64.to_int !@out
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
