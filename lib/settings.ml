module Ffi = Hegel_ffi.Ffi

type health_check =
  | Filter_too_much
  | Too_slow
  | Test_cases_too_large
  | Large_initial_test_case

let health_check_to_string = function
  | Filter_too_much -> "filter_too_much"
  | Too_slow -> "too_slow"
  | Test_cases_too_large -> "test_cases_too_large"
  | Large_initial_test_case -> "large_initial_test_case"
;;

type verbosity =
  | Quiet
  | Normal
  | Verbose
  | Debug

type database =
  | Unset
  | Disabled
  | Path of string

type backend =
  | Default
  | Urandom

type phase =
  | Explicit
  | Reuse
  | Generate
  | Target
  | Shrink

let phase_to_string = function
  | Explicit -> "explicit"
  | Reuse -> "reuse"
  | Generate -> "generate"
  | Target -> "target"
  | Shrink -> "shrink"
;;

type t =
  { test_cases : int
  ; verbosity : verbosity
  ; seed : int option
  ; derandomize : bool
  ; database : database
  ; suppress_health_check : health_check list
  ; phases : phase list
  ; print_blob : bool
  ; report_multiple_failures : bool
  ; show_statistics : bool
  ; backend : backend
  }

(* ------------------------------------------------------------------ *)
(* Translation to and from an engine settings handle                   *)
(* ------------------------------------------------------------------ *)

let ffi_verbosity = function
  | Quiet -> Ffi.Quiet
  | Normal -> Ffi.Normal
  | Verbose -> Ffi.Verbose
  | Debug -> Ffi.Debug
;;

let ffi_backend = function
  | Default -> Ffi.Default
  | Urandom -> Ffi.Urandom
;;

let backend_of_ffi = function
  | Ffi.Default -> Default
  | Ffi.Urandom -> Urandom
;;

let verbosity_of_ffi = function
  | Ffi.Quiet -> Quiet
  | Ffi.Normal -> Normal
  | Ffi.Verbose -> Verbose
  | Ffi.Debug -> Debug
;;

let all_phases = [ Explicit; Reuse; Generate; Target; Shrink ]

let phase_bit = function
  | Explicit -> Ffi.phase_explicit
  | Reuse -> Ffi.phase_reuse
  | Generate -> Ffi.phase_generate
  | Target -> Ffi.phase_target
  | Shrink -> Ffi.phase_shrink
;;

let all_health_checks =
  [ Filter_too_much; Too_slow; Test_cases_too_large; Large_initial_test_case ]
;;

let health_check_bit = function
  | Filter_too_much -> Ffi.hc_filter_too_much
  | Too_slow -> Ffi.hc_too_slow
  | Test_cases_too_large -> Ffi.hc_test_cases_too_large
  | Large_initial_test_case -> Ffi.hc_large_initial_test_case
;;

let bitmask bit_of items = List.fold_left (fun acc x -> acc lor bit_of x) 0 items
let of_bitmask bit_of all mask = List.filter (fun x -> mask land bit_of x <> 0) all

let of_ffi ctx s =
  { test_cases = Ffi.settings_get_test_cases ctx s
  ; verbosity = verbosity_of_ffi (Ffi.settings_get_verbosity ctx s)
  ; seed = Ffi.settings_get_seed ctx s
  ; derandomize = Ffi.settings_get_derandomize ctx s
  ; database =
      (match Ffi.settings_get_database ctx s with
       | None -> Unset
       | Some "" -> Disabled
       | Some dir -> Path dir)
  ; suppress_health_check =
      of_bitmask
        health_check_bit
        all_health_checks
        (Ffi.settings_get_suppress_health_check ctx s)
  ; phases = of_bitmask phase_bit all_phases (Ffi.settings_get_phases ctx s)
  ; print_blob = Ffi.settings_get_print_blob ctx s
  ; report_multiple_failures = Ffi.settings_get_report_multiple_failures ctx s
  ; show_statistics = Ffi.settings_get_show_statistics ctx s
  ; backend = backend_of_ffi (Ffi.settings_get_backend ctx s)
  }
;;

let to_ffi ctx t ~database_key =
  let s = Ffi.settings_new ctx in
  Ffi.settings_test_cases ctx s t.test_cases;
  Ffi.settings_verbosity ctx s (ffi_verbosity t.verbosity);
  Ffi.settings_seed ctx s t.seed;
  Ffi.settings_derandomize ctx s t.derandomize;
  Ffi.settings_report_multiple_failures ctx s t.report_multiple_failures;
  Ffi.settings_show_statistics ctx s t.show_statistics;
  Ffi.settings_backend ctx s (ffi_backend t.backend);
  Ffi.settings_print_blob ctx s t.print_blob;
  Ffi.settings_database
    ctx
    s
    (match t.database with
     | Unset -> None
     | Disabled -> Some ""
     | Path p -> Some p);
  Option.iter (fun k -> Ffi.settings_database_key ctx s (Some k)) database_key;
  Ffi.settings_phases ctx s (bitmask phase_bit t.phases);
  Ffi.settings_suppress_health_check
    ctx
    s
    (bitmask health_check_bit t.suppress_health_check);
  s
;;

(* ------------------------------------------------------------------ *)
(* Profiles                                                            *)
(* ------------------------------------------------------------------ *)

let with_context f =
  let ctx = Ffi.context_new () in
  Fun.protect ~finally:(fun () -> Ffi.context_free ctx) (fun () -> f ctx)
;;

(* temp workaround b/c libhegel sets print_blob to false by default *)
let development_registered =
  lazy
    (with_context (fun ctx ->
       let s = Ffi.settings_new_for_profile ctx "base" in
       Fun.protect
         ~finally:(fun () -> Ffi.settings_free ctx s)
         (fun () ->
            Ffi.settings_print_blob ctx s true;
            Ffi.settings_register_profile ctx "development" s)))
;;

let from_profile name =
  Lazy.force development_registered;
  with_context (fun ctx ->
    let s = Ffi.settings_new_for_profile ctx name in
    Fun.protect ~finally:(fun () -> Ffi.settings_free ctx s) (fun () -> of_ffi ctx s))
;;

let default () = from_profile "default"

let create ?test_cases ?seed () =
  let s = default () in
  let s =
    Option.fold ~none:s ~some:(fun test_cases -> { s with test_cases }) test_cases
  in
  Option.fold ~none:s ~some:(fun v -> { s with seed = Some v }) seed
;;

let register_profile name t =
  Lazy.force development_registered;
  with_context (fun ctx ->
    let s = to_ffi ctx t ~database_key:None in
    Fun.protect
      ~finally:(fun () -> Ffi.settings_free ctx s)
      (fun () -> Ffi.settings_register_profile ctx name s))
;;

let set_default_profile name = with_context (fun ctx -> Ffi.set_default_profile ctx name)
