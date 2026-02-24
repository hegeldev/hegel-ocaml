(** Shared test utilities used across test modules. *)

(** [contains_substring s sub] returns [true] if [sub] appears anywhere in [s].
*)
let contains_substring s sub =
  let slen = String.length s and sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0
