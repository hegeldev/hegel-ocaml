val run : ?test_cases:int -> ?hegel_path:string -> (unit -> unit) -> unit
val find_hegel_path : unit -> string option
val extract_channel_id : Cbor.t -> int
