(* Locates the native libhegel shared library at runtime

   Search order:
   1. [$HEGEL_LIBHEGEL_PATH] — an explicit path to the library file (or a
      directory containing [libhegel.<ext>]).
   2. A prebuilt libhegel bundled into the installed package via the [libhegel]
      dune-site (release tarballs ship the matching-platform binary there).
   3. A sibling [../hegel-rust/target/release/] (then [.../debug/]) checkout
      relative to the current working directory.
   4. A SHA-256-verified copy downloaded from the hegel-rust GitHub release,
      cached under [$XDG_CACHE_HOME|~/.cache]/hegel-ocaml/libhegel/<version>/.
      Set [HEGEL_LIBHEGEL_NO_DOWNLOAD=1] to opt out of the download fallback.

   This module lives in the (uninstrumented) [hegel_ffi] library, so its
   filesystem/network branches are not subject to the coverage gate. *)

(** The libhegel version these bindings target. *)
let version = "0.36.0"

(* Baked-in SHA-256 checksums of the published [libhegel-<os>-<arch>.<ext>]
   artifacts for {!version}, keyed by "<os>-<arch>". Platforms without an
   entry (e.g. macOS amd64 / Intel) are not published upstream.

   Regenerate after bumping {!version} with: scripts/update-checksums.py *)
let checksums =
  [ "darwin-arm64", "4eaefe2ceae677663c6ce8556fd4a440ac351a3b3af963a17c76b5063e1a9a9f"
  ; "linux-amd64", "46ec9ec99773568a277872d97a1bf82a8bcec712536d0ffc5850ce9286d6683c"
  ; "linux-arm64", "a7c1c3d36f886870f01c3dec9498b568ce35e0ff122aec41f111ba03347999f9"
  ; "windows-amd64", "e1cfab1ff7bfbe3682bb2c5da15e28095d97f95091f8b0fe024d02fcb3aecda5"
  ; "windows-arm64", "bbb5d0dbe052a756af1dd448ddf364291c3473db172110a703260c034abb7dd5"
  ]
;;

let release_base = "https://github.com/hegeldev/hegel-rust/releases/download/v" ^ version

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | Some s when String.length s > 0 -> Some s
  | _ -> None
;;

let read_line_cmd cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line =
      try input_line ic with
      | End_of_file -> ""
    in
    ignore (Unix.close_process_in ic);
    String.trim line
  with
  | _ -> ""
;;

(* OS identifier used in the libhegel release-artifact name. The values
   ("darwin" / "linux" / "windows") follow that artifact naming convention. *)
let os_id () =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> "windows"
  | _ ->
    (match read_line_cmd "uname -s" with
     | "Darwin" -> "darwin"
     | "Linux" -> "linux"
     | other -> failwith (Printf.sprintf "hegel: unsupported operating system %S" other))
;;

(* Architecture identifier used in the libhegel release-artifact name
   ("amd64" / "arm64"). *)
let arch_id () =
  let raw =
    match Sys.os_type with
    | "Win32" -> Option.value (getenv_nonempty "PROCESSOR_ARCHITECTURE") ~default:""
    | _ -> read_line_cmd "uname -m"
  in
  match String.lowercase_ascii raw with
  | "x86_64" | "amd64" -> "amd64"
  | "arm64" | "aarch64" -> "arm64"
  | other -> failwith (Printf.sprintf "hegel: unsupported architecture %S" other)
;;

let ext_of_os = function
  | "darwin" -> "dylib"
  | "windows" -> "dll"
  | _ -> "so"
;;

(* Name of the library file as produced by a local [cargo build] and as cached. *)
let local_basename ext = "libhegel." ^ ext

(* Name of the published release artifact for a given platform. *)
let release_artifact key ext = Printf.sprintf "libhegel-%s.%s" key ext

let is_dir p =
  try Sys.is_directory p with
  | Sys_error _ -> false
;;

let is_file p =
  try Sys.file_exists p && not (Sys.is_directory p) with
  | Sys_error _ -> false
;;

let home () =
  match getenv_nonempty "HOME" with
  | Some h -> h
  | None -> Option.value (getenv_nonempty "USERPROFILE") ~default:"."
;;

let cache_dir () =
  let base =
    match getenv_nonempty "XDG_CACHE_HOME" with
    | Some d -> d
    | None -> Filename.concat (home ()) ".cache"
  in
  List.fold_left Filename.concat base [ "hegel-ocaml"; "libhegel"; version ]
;;

let rec mkdir_p dir =
  if not (is_dir dir)
  then (
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then mkdir_p parent;
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
;;

(* The hex SHA-256 of [path], via the system [shasum]/[sha256sum] tool. *)
let sha256_of_file path =
  let q = Filename.quote path in
  let parse cmd =
    let s = read_line_cmd cmd in
    if String.length s >= 64
    then Some (String.lowercase_ascii (String.sub s 0 64))
    else None
  in
  match parse (Printf.sprintf "shasum -a 256 %s 2>/dev/null" q) with
  | Some h -> h
  | None ->
    (match parse (Printf.sprintf "sha256sum %s 2>/dev/null" q) with
     | Some h -> h
     | None ->
       failwith
         "hegel: no SHA-256 utility (shasum or sha256sum) found to verify the downloaded \
          libhegel")
;;

(* 1. Explicit [$HEGEL_LIBHEGEL_PATH] override. *)
let from_env ext =
  Option.map
    (fun p -> if is_dir p then Filename.concat p (local_basename ext) else p)
    (getenv_nonempty "HEGEL_LIBHEGEL_PATH")
;;

(* 2. A libhegel bundled into the installed package. Release tarballs install
   the matching-platform binary into the [libhegel] dune-site under the fixed
   name [libhegel_bundled] (the loader opens it by path, so the missing
   extension is irrelevant to [dlopen]). *)
let from_site () =
  List.find_opt
    is_file
    (List.map
       (fun dir -> Filename.concat dir "libhegel_bundled")
       Hegel_sites.Sites.libhegel)
;;

(* 3. Sibling hegel-rust checkout relative to the working directory. *)
let from_sibling ext =
  let cwd = Sys.getcwd () in
  let candidate sub =
    List.fold_left
      Filename.concat
      cwd
      [ ".."; "hegel-rust"; "target"; sub; local_basename ext ]
  in
  List.find_opt is_file [ candidate "release"; candidate "debug" ]
;;

(* Download [url] into [cache_path], failing unless the payload's SHA-256 is
   [expected]. The payload is fetched to a temporary file in the cache
   directory, verified, and then renamed into place. Exposed (rather than
   inlined in [from_cache_or_download]) so the test suite can exercise the
   download protocol against a stubbed [curl] and its own checksum. *)
let download_verified ~url ~expected ~cache_path =
  mkdir_p (Filename.dirname cache_path);
  (* Concurrent processes (e.g. several test binaries starting with a cold
     cache) may all reach this point at once, so each must download to its
     OWN temporary file: a shared temporary path would let one process rename
     (or delete) a file another is still writing. The PID makes the name
     unique across live processes on this host; the random salt guards
     against PID reuse colliding with a stale file left by a killed process
     (such leftovers are otherwise inert: nothing else ever reads them). The
     temporary file stays in the cache directory so the rename below cannot
     cross filesystems. *)
  let tmp =
    Printf.sprintf
      "%s.%d.%06x.tmp"
      cache_path
      (Unix.getpid ())
      (Random.State.bits (Random.State.make_self_init ()) land 0xFFFFFF)
  in
  let remove_tmp () =
    try Sys.remove tmp with
    | _ -> ()
  in
  let rc =
    Sys.command
      (Printf.sprintf "curl -fsSL %s -o %s" (Filename.quote url) (Filename.quote tmp))
  in
  if rc <> 0
  then (
    remove_tmp ();
    failwith (Printf.sprintf "hegel: failed to download %s (curl exit %d)" url rc));
  let actual = sha256_of_file tmp in
  if not (String.equal actual expected)
  then (
    remove_tmp ();
    failwith
      (Printf.sprintf
         "hegel: SHA-256 mismatch for downloaded libhegel (expected %s, got %s)"
         expected
         actual));
  (* Atomic publish. If a concurrent process won the race, POSIX [rename]
     silently (and atomically) replaces its identical, verified copy. On
     platforms where replacing an in-use file can fail (e.g. Windows), fall
     back to the winner's copy when it verifies. *)
  (try Sys.rename tmp cache_path with
   | Sys_error _ as e ->
     remove_tmp ();
     if not (is_file cache_path && String.equal (sha256_of_file cache_path) expected)
     then raise e);
  cache_path
;;

(* 4. Cached download (fetching + verifying on first use). *)
let from_cache_or_download os_id ext =
  let key = os_id ^ "-" ^ arch_id () in
  let expected =
    match List.assoc_opt key checksums with
    | Some h -> h
    | None ->
      failwith
        (Printf.sprintf
           "hegel: no baked-in libhegel checksum for platform %s (not published upstream \
            for v%s). Build hegel-rust and set HEGEL_LIBHEGEL_PATH to the resulting \
            libhegel.%s."
           key
           version
           ext)
  in
  let cache_path = Filename.concat (cache_dir ()) (local_basename ext) in
  if is_file cache_path && String.equal (sha256_of_file cache_path) expected
  then cache_path
  else (
    (match getenv_nonempty "HEGEL_LIBHEGEL_NO_DOWNLOAD" with
     | Some _ ->
       failwith
         (Printf.sprintf
            "hegel: libhegel not found and downloads are disabled \
             (HEGEL_LIBHEGEL_NO_DOWNLOAD). Looked at $HEGEL_LIBHEGEL_PATH, \
             ../hegel-rust/target/{release,debug}/, and %s. Build hegel-rust or set \
             HEGEL_LIBHEGEL_PATH."
            cache_path)
     | None -> ());
    let url = Printf.sprintf "%s/%s" release_base (release_artifact key ext) in
    download_verified ~url ~expected ~cache_path)
;;

(** [locate ()] returns the path to a usable libhegel shared library, downloading
    and caching it if necessary. Raises [Failure] with a descriptive message if
    no library can be found or fetched. *)
let locate () =
  let os_id = os_id () in
  let ext = ext_of_os os_id in
  match from_env ext with
  | Some p -> p
  | None ->
    (match from_site () with
     | Some p -> p
     | None ->
       (match from_sibling ext with
        | Some p -> p
        | None -> from_cache_or_download os_id ext))
;;
