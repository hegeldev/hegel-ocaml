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
  }

(** CI environment variables to check for auto-detection. Each entry is
    [(var_name, expected_value)] where [None] means "any value". *)
let ci_vars =
  [ "CI", None
  ; "TF_BUILD", Some "true"
  ; "BUILDKITE", Some "true"
  ; "CIRCLECI", Some "true"
  ; "CIRRUS_CI", Some "true"
  ; "CODEBUILD_BUILD_ID", None
  ; "GITHUB_ACTIONS", Some "true"
  ; "GITLAB_CI", None
  ; "HEROKU_TEST_RUN_ID", None
  ; "TEAMCITY_VERSION", None
  ]
;;

let is_in_ci () =
  List.exists
    (fun (key, expected) ->
       match Sys.getenv_opt key, expected with
       | Some _, None -> true
       | Some v, Some exp -> String.equal v exp
       | None, _ -> false)
    ci_vars
;;

let default () =
  let in_ci = is_in_ci () in
  { test_cases = 100
  ; verbosity = Normal
  ; seed = None
  ; derandomize = in_ci
  ; database = (if in_ci then Disabled else Unset)
  ; suppress_health_check = []
  ; phases = [ Explicit; Reuse; Generate; Target; Shrink ]
  ; print_blob = true
  ; report_multiple_failures = false
  ; show_statistics = false
  }
;;

let create ?test_cases ?seed () =
  let s = default () in
  let s =
    Option.fold ~none:s ~some:(fun test_cases -> { s with test_cases }) test_cases
  in
  Option.fold ~none:s ~some:(fun v -> { s with seed = Some v }) seed
;;
