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
(* Raw bindings                                                        *)
(* ------------------------------------------------------------------ *)

let c_settings_new = foreign "hegel_settings_new" (void @-> returning (ptr void))
let c_settings_free = foreign "hegel_settings_free" (ptr void @-> returning void)
let c_settings_mode = foreign "hegel_settings_mode" (ptr void @-> int @-> returning void)

let c_settings_test_cases =
  foreign "hegel_settings_test_cases" (ptr void @-> uint64_t @-> returning void)
;;

let c_settings_verbosity =
  foreign "hegel_settings_verbosity" (ptr void @-> int @-> returning void)
;;

let c_settings_seed =
  foreign "hegel_settings_seed" (ptr void @-> uint64_t @-> bool @-> returning void)
;;

let c_settings_derandomize =
  foreign "hegel_settings_derandomize" (ptr void @-> bool @-> returning void)
;;

let c_settings_report_multiple_failures =
  foreign "hegel_settings_report_multiple_failures" (ptr void @-> bool @-> returning void)
;;

let c_settings_database =
  foreign "hegel_settings_database" (ptr void @-> string_opt @-> returning void)
;;

let c_settings_database_key =
  foreign "hegel_settings_database_key" (ptr void @-> string_opt @-> returning void)
;;

let c_settings_phases =
  foreign "hegel_settings_phases" (ptr void @-> uint32_t @-> returning void)
;;

let c_settings_suppress_health_check =
  foreign "hegel_settings_suppress_health_check" (ptr void @-> uint32_t @-> returning void)
;;

let c_run_start = foreign "hegel_run_start" (ptr void @-> returning (ptr void))

let c_next_test_case =
  foreign_blocking "hegel_next_test_case" (ptr void @-> returning (ptr void))
;;

let c_run_result = foreign "hegel_run_result" (ptr void @-> returning (ptr void))
let c_run_free = foreign "hegel_run_free" (ptr void @-> returning void)
let c_test_case_free = foreign "hegel_test_case_free" (ptr void @-> returning void)

let c_generate =
  foreign
    "hegel_generate"
    (ptr void @-> ptr char @-> size_t @-> ptr (ptr char) @-> ptr size_t @-> returning int)
;;

let c_primitive_boolean =
  foreign
    "hegel_primitive_boolean"
    (ptr void @-> double @-> bool @-> bool @-> ptr bool @-> returning int)
;;

let c_test_case_from_blob =
  foreign "hegel_test_case_from_blob" (ptr void @-> string_opt @-> returning (ptr void))
;;

let c_start_span = foreign "hegel_start_span" (ptr void @-> uint64_t @-> returning int)
let c_stop_span = foreign "hegel_stop_span" (ptr void @-> bool @-> returning int)

let c_new_collection =
  foreign
    "hegel_new_collection"
    (ptr void @-> uint64_t @-> uint64_t @-> ptr int64_t @-> returning int)
;;

let c_collection_more =
  foreign "hegel_collection_more" (ptr void @-> int64_t @-> ptr bool @-> returning int)
;;

let c_collection_reject =
  foreign "hegel_collection_reject" (ptr void @-> int64_t @-> string_opt @-> returning int)
;;

let c_new_pool = foreign "hegel_new_pool" (ptr void @-> ptr int64_t @-> returning int)

let c_pool_add =
  foreign "hegel_pool_add" (ptr void @-> int64_t @-> ptr int64_t @-> returning int)
;;

let c_pool_generate =
  foreign
    "hegel_pool_generate"
    (ptr void @-> int64_t @-> bool @-> ptr int64_t @-> returning int)
;;

let c_target = foreign "hegel_target" (ptr void @-> double @-> string @-> returning int)

let c_mark_complete =
  foreign "hegel_mark_complete" (ptr void @-> int @-> string_opt @-> returning int)
;;

let c_is_final_replay =
  foreign "hegel_test_case_is_final_replay" (ptr void @-> returning bool)
;;

let c_result_passed = foreign "hegel_run_result_passed" (ptr void @-> returning bool)

let c_result_failure_count =
  foreign "hegel_run_result_failure_count" (ptr void @-> returning size_t)
;;

let c_result_failure =
  foreign "hegel_run_result_failure" (ptr void @-> size_t @-> returning (ptr void))
;;

let c_failure_panic_message =
  foreign "hegel_failure_panic_message" (ptr void @-> returning string_opt)
;;

let c_failure_diagnostic =
  foreign "hegel_failure_diagnostic" (ptr void @-> returning string_opt)
;;

let c_failure_blob =
  foreign "hegel_failure_reproduction_blob" (ptr void @-> returning string_opt)
;;

let c_failure_origin = foreign "hegel_failure_origin" (ptr void @-> returning string_opt)
let c_last_error_message = foreign "hegel_last_error_message" (void @-> returning string)
let c_version = foreign "hegel_version" (void @-> returning string)

(* ------------------------------------------------------------------ *)
(* Public types                                                        *)
(* ------------------------------------------------------------------ *)

type settings = unit Ctypes.ptr
type run = unit Ctypes.ptr
type test_case = unit Ctypes.ptr
type run_result = unit Ctypes.ptr
type failure = unit Ctypes.ptr

type mode =
  | Test_run
  | Single_test_case

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

exception Stop_test
exception Assume_rejected
exception Backend_error of string

(* Phase bitmask values [HEGEL_PHASE_*]. *)
let phase_explicit = 1
let phase_reuse = 1 lsl 1
let phase_generate = 1 lsl 2
let phase_target = 1 lsl 3
let phase_shrink = 1 lsl 4

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

(* Translate a C return code: [HEGEL_OK] succeeds, [HEGEL_E_STOP_TEST]
   raises {!Stop_test}, [HEGEL_E_ASSUME] raises {!Assume_rejected} (the engine
   rejected the case as invalid — e.g. an impossible uniqueness constraint hits
   the collection reject limit), anything else raises {!Backend_error} with the
   thread-local diagnostic. [HEGEL_E_ASSUME] carries no diagnostic, so it must
   be matched before the catch-all. *)
let check_rc rc =
  if rc = 0
  then ()
  else if rc = -1
  then raise Stop_test
  else if rc = -2
  then raise Assume_rejected
  else raise (Backend_error (c_last_error_message ()))
;;

(* ------------------------------------------------------------------ *)
(* Diagnostics                                                         *)
(* ------------------------------------------------------------------ *)

let version () = c_version ()
let last_error_message () = c_last_error_message ()

(* ------------------------------------------------------------------ *)
(* Settings                                                            *)
(* ------------------------------------------------------------------ *)

let settings_new () = c_settings_new ()
let settings_free s = c_settings_free s
let settings_mode s m = c_settings_mode s (mode_to_int m)
let settings_test_cases s n = c_settings_test_cases s (Unsigned.UInt64.of_int n)
let settings_verbosity s v = c_settings_verbosity s (verbosity_to_int v)

let settings_seed s = function
  | Some seed -> c_settings_seed s (Unsigned.UInt64.of_int seed) true
  | None -> c_settings_seed s Unsigned.UInt64.zero false
;;

let settings_derandomize s b = c_settings_derandomize s b
let settings_report_multiple_failures s b = c_settings_report_multiple_failures s b
let settings_database s d = c_settings_database s d
let settings_database_key s k = c_settings_database_key s k
let settings_phases s mask = c_settings_phases s (Unsigned.UInt32.of_int mask)

let settings_suppress_health_check s mask =
  c_settings_suppress_health_check s (Unsigned.UInt32.of_int mask)
;;

(* ------------------------------------------------------------------ *)
(* Run lifecycle                                                       *)
(* ------------------------------------------------------------------ *)

let run_start s =
  let r = c_run_start s in
  if is_null r then raise (Backend_error (c_last_error_message ()));
  r
;;

let next_test_case run =
  let tc = c_next_test_case run in
  if is_null tc
  then (
    let msg = c_last_error_message () in
    if String.length msg = 0 then None else raise (Backend_error msg))
  else Some tc
;;

let test_case_from_blob s b =
  let tc = c_test_case_from_blob s b in
  (* hegel_test_case_from_blob sets a non-empty last error on every null-return
     path (bad pointer, non-UTF-8 blob, undecodable blob), so a failure always
     surfaces as [Backend_error] — it never yields a [None]-like handle. *)
  if is_null tc
  then (
    let msg = c_last_error_message () in
    raise
      (Backend_error
         (if String.length msg = 0 then "hegel_test_case_from_blob failed" else msg)))
  else tc
;;

let run_result run =
  let r = c_run_result run in
  if is_null r then raise (Backend_error (c_last_error_message ()));
  r
;;

let run_free run = c_run_free run
let blob_test_case_free tc = c_test_case_free tc

(* ------------------------------------------------------------------ *)
(* Per-test-case primitives                                            *)
(* ------------------------------------------------------------------ *)

let generate tc schema =
  let len = String.length schema in
  let arr = CArray.of_string schema in
  let p = CArray.start arr in
  let out_ptr = allocate (ptr char) (from_voidp char null) in
  let out_len = allocate size_t (Unsigned.Size_t.of_int 0) in
  let rc = c_generate tc p (Unsigned.Size_t.of_int len) out_ptr out_len in
  check_rc rc;
  let n = Unsigned.Size_t.to_int !@out_len in
  string_from_ptr !@out_ptr ~length:n
;;

let primitive_boolean tc p forced =
  let out_ptr = allocate bool false in
  let rc =
    match forced with
    | Some b -> c_primitive_boolean tc p b true out_ptr
    | None -> c_primitive_boolean tc p false false out_ptr
  in
  check_rc rc;
  !@out_ptr
;;

let start_span tc label = check_rc (c_start_span tc (Unsigned.UInt64.of_int label))
let stop_span tc discard = check_rc (c_stop_span tc discard)

let new_collection tc ~min_size ~max_size =
  let out = allocate int64_t 0L in
  let max_u =
    match max_size with
    | Some m -> Unsigned.UInt64.of_int m
    | None -> Unsigned.UInt64.max_int
  in
  check_rc (c_new_collection tc (Unsigned.UInt64.of_int min_size) max_u out);
  Int64.to_int !@out
;;

let collection_more tc id =
  let out = allocate bool false in
  check_rc (c_collection_more tc (Int64.of_int id) out);
  !@out
;;

let collection_reject tc id why = check_rc (c_collection_reject tc (Int64.of_int id) why)

let new_pool tc =
  let out = allocate int64_t 0L in
  check_rc (c_new_pool tc out);
  Int64.to_int !@out
;;

let pool_add tc ~pool_id =
  let out = allocate int64_t 0L in
  check_rc (c_pool_add tc (Int64.of_int pool_id) out);
  Int64.to_int !@out
;;

let pool_generate tc ~pool_id ~consume =
  let out = allocate int64_t 0L in
  check_rc (c_pool_generate tc (Int64.of_int pool_id) consume out);
  Int64.to_int !@out
;;

let target tc value label = check_rc (c_target tc value label)

let mark_complete tc status origin =
  check_rc (c_mark_complete tc (status_to_int status) origin)
;;

let is_final_replay tc = c_is_final_replay tc

(* ------------------------------------------------------------------ *)
(* Result inspection                                                   *)
(* ------------------------------------------------------------------ *)

let result_passed r = c_result_passed r
let result_failure_count r = Unsigned.Size_t.to_int (c_result_failure_count r)

let result_failure r i =
  let f = c_result_failure r (Unsigned.Size_t.of_int i) in
  if is_null f then None else Some f
;;

let result_failures r =
  let n = result_failure_count r in
  List.init n (fun i ->
    match result_failure r i with
    | Some f -> f
    | None -> raise (Backend_error "hegel: failure disappeared mid-iteration"))
;;

let failure_panic_message f = c_failure_panic_message f
let failure_diagnostic f = c_failure_diagnostic f
let failure_origin f = c_failure_origin f
let failure_blob f = c_failure_blob f
