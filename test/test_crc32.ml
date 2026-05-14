(** CRC32 conformance vectors. *)

let rec unroll crc str off len =
  if len = 0
  then crc
  else (
    let crc = Crc32.digest_string str off 1 crc in
    unroll crc str (off + 1) (len - 1))
;;

let check name input expected () =
  let crc = unroll Crc32.default input 0 (String.length input) in
  Alcotest.(check int32) name expected crc
;;

let const x _ = x

let tests =
  [ Alcotest.test_case "empty" `Quick (check "empty" "" 0l)
  ; Alcotest.test_case "zero byte" `Quick (check "zero" "\x00" 0xd202ef8dl)
  ; Alcotest.test_case
      "four 0xFF bytes"
      `Quick
      (check "ffffffff" "\xff\xff\xff\xff" 0xffffffffl)
  ; Alcotest.test_case
      "check value 123456789"
      `Quick
      (check "check" "123456789" 0xcbf43926l)
  ; Alcotest.test_case
      "Donne quote"
      `Quick
      (check "donne" "Thou hast made me, and shall thy work decay?" 0xf1fabe1dl)
  ; Alcotest.test_case
      "1000 segments"
      `Quick
      (check "long" (String.concat "%" (List.init 1000 (const "abcdef"))) 0x0adc436fl)
  ]
;;
