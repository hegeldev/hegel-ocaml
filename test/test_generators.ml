let test_bool () =
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.bool ()).generate () in
      ())

let test_float () =
  Hegel.run (fun () ->
      let f =
        (Hegel.Gen.float ~min:0.0 ~max:1.0 ~allow_nan:false
           ~allow_infinity:false ())
          .generate ()
      in
      assert (f >= 0.0 && f <= 1.0))

let test_string () =
  Hegel.run (fun () ->
      let s = (Hegel.Gen.string ~min_size:1 ~max_size:10 ()).generate () in
      assert (String.length s >= 1))

let test_pair () =
  Hegel.run (fun () ->
      let a, b =
        (Hegel.Gen.pair (Hegel.Gen.int ()) (Hegel.Gen.bool ())).generate ()
      in
      ignore (a, b))

let test_optional () =
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.optional (Hegel.Gen.int ())).generate () in
      ())

let test_sampled_from () =
  Hegel.run (fun () ->
      let color =
        (Hegel.Gen.sampled_from [ "red"; "green"; "blue" ]).generate ()
      in
      assert (color = "red" || color = "green" || color = "blue"))

let test_one_of () =
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.one_of
           [
             Hegel.Gen.int ~min:0 ~max:10 (); Hegel.Gen.int ~min:100 ~max:110 ();
           ])
          .generate
          ()
      in
      assert (n >= 0 && n <= 110))

let test_map () =
  Hegel.run (fun () ->
      let s =
        (Hegel.Gen.map string_of_int (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate ()
      in
      let n = int_of_string s in
      assert (n >= 0 && n <= 100))

let test_email () =
  Hegel.run (fun () ->
      let e = (Hegel.Gen.email ()).generate () in
      assert (String.contains e '@'))

let () =
  Alcotest.run "Hegel Generators"
    [
      ( "generators",
        [
          Alcotest.test_case "bool" `Quick test_bool;
          Alcotest.test_case "float" `Quick test_float;
          Alcotest.test_case "string" `Quick test_string;
          Alcotest.test_case "pair" `Quick test_pair;
          Alcotest.test_case "optional" `Quick test_optional;
          Alcotest.test_case "sampled_from" `Quick test_sampled_from;
          Alcotest.test_case "one_of" `Quick test_one_of;
          Alcotest.test_case "map" `Quick test_map;
          Alcotest.test_case "email" `Quick test_email;
        ] );
    ]
