(** Antithesis integration for Hegel.

    See the [.mli] for the full description. *)

type test_location =
  { function_name : string
  ; file : string
  ; begin_line : int
  }

(** Environment variable Antithesis injects when a workload runs inside it. *)
let antithesis_output_dir_env = "ANTITHESIS_OUTPUT_DIR"

let is_running_in_antithesis () =
  match Sys.getenv_opt antithesis_output_dir_env with
  | None -> false
  | Some dir ->
    if Sys.file_exists dir && Sys.is_directory dir
    then true
    else
      failwith
        (Printf.sprintf
           "Expected %s=%s to exist as a directory when running inside Antithesis"
           antithesis_output_dir_env
           dir)
;;

let extract_file_base path =
  let base = Filename.basename path in
  try Filename.chop_extension base with
  | Invalid_argument _ -> base
;;

let assertion_json loc ~hit ~condition =
  let id =
    Printf.sprintf
      "%s in %s passes properties"
      loc.function_name
      (extract_file_base loc.file)
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
  Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.output_string oc line)
;;

let emit_assertion loc ~passed =
  if is_running_in_antithesis ()
  then (
    let dir = Option.get (Sys.getenv_opt antithesis_output_dir_env) in
    let path = Filename.concat dir "sdk.jsonl" in
    write_jsonl_line path (assertion_json loc ~hit:false ~condition:false);
    write_jsonl_line path (assertion_json loc ~hit:true ~condition:passed))
;;
