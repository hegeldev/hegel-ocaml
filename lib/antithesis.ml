(** Antithesis integration for Hegel.

    See the [.mli] for the full description. *)

open! Core

type test_location =
  { function_name : string
  ; file : string
  ; begin_line : int
  }

(** Environment variable Antithesis injects when a workload runs inside it. *)
let antithesis_output_dir_env = "ANTITHESIS_OUTPUT_DIR"

let is_running_in_antithesis () =
  match Sys.getenv antithesis_output_dir_env with
  | None -> false
  | Some dir ->
    if Stdlib.Sys.file_exists dir && Stdlib.Sys.is_directory dir
    then true
    else
      failwithf
        "Expected %s=%s to exist as a directory when running inside Antithesis"
        antithesis_output_dir_env
        dir
        ()
;;

let chop_file_ext path =
  let base = Filename.basename path in
  try Filename.chop_extension base with
  | Invalid_argument _ -> base
;;

let assertion_json loc ~hit ~condition =
  let id =
    Printf.sprintf "%s.%s passes properties" (chop_file_ext loc.file) loc.function_name
  in
  let location_obj : Yojson.Safe.t =
    `Assoc
      [ "function", `String loc.function_name
      ; "file", `String loc.file
      ; "begin_line", `Int loc.begin_line
      ; "begin_column", `Int 0
      ]
  in
  `Assoc
    [ ( "antithesis_assert"
      , `Assoc
          [ "hit", `Bool hit
          ; "must_hit", `Bool true
          ; "assert_type", `String "always"
          ; "display_type", `String "Always"
          ; "condition", `Bool condition
          ; "id", `String id
          ; "message", `String id
          ; "location", location_obj
          ] )
    ]
;;

let write_jsonl_line path json =
  let line = Yojson.Safe.to_string json ^ "\n" in
  let oc =
    Stdlib.open_out_gen [ Open_wronly; Open_creat; Open_append; Open_binary ] 0o644 path
  in
  Exn.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    ~f:(fun () -> Stdlib.output_string oc line)
;;

let emit_assertion loc ~passed =
  let dir = Sys.getenv_exn antithesis_output_dir_env in
  let path = Filename.concat dir "sdk.jsonl" in
  write_jsonl_line path (assertion_json loc ~hit:false ~condition:false);
  write_jsonl_line path (assertion_json loc ~hit:true ~condition:passed)
;;
