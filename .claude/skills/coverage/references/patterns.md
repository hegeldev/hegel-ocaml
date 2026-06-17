# Coverage Patterns and Techniques

Detailed patterns for achieving 100% test coverage through better code design.

## Genuinely Unreachable Code

Code that should never execute under any circumstances.

**Fix**: Make it an explicit error.

```ocaml
(* Bad: Silent unreachable code *)
let process state =
  match state with
  | A -> handle_a ()
  | B -> handle_b ()
  | C -> ()  (* "Can't happen" - but coverage sees it *)

(* Good: Explicit unreachable *)
let process state =
  match state with
  | A -> handle_a ()
  | B -> handle_b ()
  | C -> failwith "unreachable: State C is never created"
```

`failwith "unreachable: ..."` documents intent and will raise `Failure` if your
assumption is wrong. This project uses `failwith "..."` for engine-contract
violations that are unreachable under correct behavior — see `cbor_helpers.ml`
for the canonical pattern. Never use `[@coverage off]` to silence these branches.

## Hard-to-Test Dependencies

Code that interacts with external systems (filesystem, network, time, environment).

**Fix**: Extract and inject dependencies.

### Extract Functions

```ocaml
(* Bad: Monolithic function *)
let deploy () =
  let ic = Unix.open_process_in "git push" in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | _ -> Error `Git_push_failed
  (* ... more logic ... *)

(* Good: Extract the testable logic *)
let check_exit_status = function
  | Unix.WEXITED 0 -> Ok ()
  | _ -> Error `Command_failed

let deploy () =
  let ic = Unix.open_process_in "git push" in
  check_exit_status (Unix.close_process_in ic)

(* In test/test_deploy.ml: *)
let test_check_exit_status_ok () =
  Alcotest.(check bool)
    "WEXITED 0 is Ok" true
    (Result.is_ok (check_exit_status (Unix.WEXITED 0)))
```

### Pass Dependencies as Records or Modules

OCaml's idiomatic equivalent of "dependency injection" is to pass a record of
functions (or a first-class module) rather than calling the real implementation
directly.

```ocaml
(* Bad: Hardcoded dependency on system clock *)
let is_expired expiry = Unix.gettimeofday () > expiry

(* Good: Inject the dependency via a record of functions *)
type clock = { now : unit -> float }

let real_clock = { now = Unix.gettimeofday }

let is_expired ~clock expiry = clock.now () > expiry

(* In test/test_clock.ml: *)
let mock_clock t = { now = (fun () -> t) }

let test_is_expired () =
  let clock = mock_clock 100.0 in
  Alcotest.(check bool) "past is expired" true (is_expired ~clock 50.0);
  Alcotest.(check bool) "future is not expired" false (is_expired ~clock 200.0)
```

For richer dependencies, a first-class module or functor works the same way:

```ocaml
module type CLOCK = sig
  val now : unit -> float
end

module Real_clock : CLOCK = struct
  let now = Unix.gettimeofday
end

module Make (C : CLOCK) = struct
  let is_expired expiry = C.now () > expiry
end
```

### Parameterize Over Environment

For functions that read env vars, platform information, or global state, extract
the logic into a parameterized version and leave a thin wrapper:

```ocaml
(* Hard to test — reads env vars directly *)
let cache_dir () =
  match Sys.getenv_opt "XDG_CACHE_HOME" with
  | Some xdg -> Filename.concat xdg "myapp"
  | None ->
      let home = Sys.getenv "HOME" in
      Filename.concat home ".cache/myapp"

(* Testable — takes values as parameters *)
let cache_dir_from ~xdg ~home =
  match xdg with
  | Some xdg -> Filename.concat xdg "myapp"
  | None -> Filename.concat home ".cache/myapp"

(* Thin wrapper calls the testable version *)
let cache_dir () =
  cache_dir_from
    ~xdg:(Sys.getenv_opt "XDG_CACHE_HOME")
    ~home:(Sys.getenv "HOME")
```

### Manipulate PATH to Mock Commands

For code that shells out to external commands, prepend a temp dir containing a
mock script to `PATH`:

```ocaml
(* test/test_deploy.ml *)
let setup_mock_git ~dir ~script =
  let git_path = Filename.concat dir "git" in
  let oc = open_out git_path in
  output_string oc ("#!/bin/sh\n" ^ script ^ "\n");
  close_out oc;
  Unix.chmod git_path 0o755

let with_tempdir f =
  let dir = Filename.temp_file "test" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () ->
      (* recursive remove omitted for brevity *)
      ())
    (fun () -> f dir)

let test_deploy_handles_git_failure () =
  with_tempdir (fun dir ->
      setup_mock_git ~dir ~script:"exit 1";
      let original_path = Sys.getenv "PATH" in
      Unix.putenv "PATH" (dir ^ ":" ^ original_path);
      let result = Fun.protect
        ~finally:(fun () -> Unix.putenv "PATH" original_path)
        (fun () -> deploy ())
      in
      Alcotest.(check bool) "deploy fails on bad git" true
        (Result.is_error result))
```

## Error Handling Branches

Error paths that are hard to trigger.

**Fix**: Design for testability.

```ocaml
(* Bad: Can't test parsing logic without triggering IO *)
let read_and_parse path =
  let ic = open_in path in
  let content = In_channel.input_all ic in
  close_in ic;
  let value = int_of_string (String.trim content) in
  { value }

(* Good: Separate IO from parsing *)
let parse content =
  match int_of_string_opt (String.trim content) with
  | Some value -> Ok { value }
  | None -> Error `Invalid_input

let read_and_parse path =
  let ic = open_in path in
  let content =
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        In_channel.input_all ic)
  in
  parse content

(* In test/test_parse.ml: *)
let test_parse_invalid_input () =
  Alcotest.(check bool) "non-integer is Error" true
    (Result.is_error (parse "not valid"))
```

## Common Anti-Patterns to Avoid

### Don't: Mock Everything

```ocaml
(* Bad: Testing mocks, not code *)
let test_with_all_mocks () =
  let mock_db = Mock_db.create () in
  let mock_http = Mock_http.create () in
  let mock_fs = Mock_fs.create () in
  ignore (mock_db, mock_http, mock_fs);
  (* At this point, what are you even testing? *)
  ()
```
