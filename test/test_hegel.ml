let test_addition_commutative () =
  Hegel.run (fun () ->
      let x = (Hegel.Gen.int ~min:(-1000) ~max:1000 ()).generate () in
      let y = (Hegel.Gen.int ~min:(-1000) ~max:1000 ()).generate () in
      assert (x + y = y + x))

let test_string_concat_length () =
  Hegel.run (fun () ->
      let s1 = (Hegel.Gen.string ~max_size:100 ()).generate () in
      let s2 = (Hegel.Gen.string ~max_size:100 ()).generate () in
      assert (String.length (s1 ^ s2) = String.length s1 + String.length s2))

let test_list_reverse_involution () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:20 (Hegel.Gen.int ())).generate () in
      assert (List.rev (List.rev xs) = xs))

let () =
  Alcotest.run "Hegel"
    [
      ( "properties",
        [
          Alcotest.test_case "addition commutative" `Quick
            test_addition_commutative;
          Alcotest.test_case "string concat length" `Quick
            test_string_concat_length;
          Alcotest.test_case "list reverse involution" `Quick
            test_list_reverse_involution;
        ] );
    ]
