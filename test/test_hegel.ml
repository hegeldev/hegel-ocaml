(* Example property test using the Hegel OCaml SDK. *)

let () =
  (* Test: addition is commutative *)
  Hegel.run (fun () ->
      let x = (Hegel.Gen.int ~min:(-1000) ~max:1000 ()).generate () in
      let y = (Hegel.Gen.int ~min:(-1000) ~max:1000 ()).generate () in
      assert (x + y = y + x));

  (* Test: string concatenation length *)
  Hegel.run (fun () ->
      let s1 = (Hegel.Gen.string ~max_size:100 ()).generate () in
      let s2 = (Hegel.Gen.string ~max_size:100 ()).generate () in
      assert (String.length (s1 ^ s2) = String.length s1 + String.length s2));

  (* Test: list reverse is involution *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:20 (Hegel.Gen.int ())).generate () in
      assert (List.rev (List.rev xs) = xs))
