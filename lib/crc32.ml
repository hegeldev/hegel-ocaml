(* CRC32 with polynomial 0xEDB88320 (reflected) *)

let table =
  let tbl = Array.make 256 0l in
  for i = 0 to 255 do
    let crc = ref (Int32.of_int i) in
    for _ = 0 to 7 do
      if Int32.logand !crc 1l <> 0l then
        crc := Int32.logxor (Int32.shift_right_logical !crc 1) 0xEDB88320l
      else crc := Int32.shift_right_logical !crc 1
    done;
    tbl.(i) <- !crc
  done;
  tbl

let compute s =
  let crc = ref 0xFFFFFFFFl in
  for i = 0 to String.length s - 1 do
    let byte = Char.code (String.get s i) in
    let index =
      Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int byte)) 0xFFl)
    in
    crc := Int32.logxor (Int32.shift_right_logical !crc 8) table.(index)
  done;
  Int32.logxor !crc 0xFFFFFFFFl
