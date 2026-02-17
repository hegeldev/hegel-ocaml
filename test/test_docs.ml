(* Tests backing specific claims made in README.md *)

let utf8_codepoint_count s =
  let n = ref 0 in
  let i = ref 0 in
  let len = String.length s in
  while !i < len do
    let c = Char.code s.[!i] in
    incr n;
    if c < 0x80 then incr i
    else if c < 0xE0 then i := !i + 2
    else if c < 0xF0 then i := !i + 3
    else i := !i + 4
  done;
  !n

let () =
  (* int ~min ~max respects bounds *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:10 ~max:20 ()).generate () in
      assert (n >= 10);
      assert (n <= 20));

  (* int32 works *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int32 ~min:(-1000l) ~max:1000l ()).generate () in
      assert (Int32.to_int n >= -1000);
      assert (Int32.to_int n <= 1000));

  (* int64 works *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int64 ~min:(-1000L) ~max:1000L ()).generate () in
      assert (Int64.to_int n >= -1000);
      assert (Int64.to_int n <= 1000));

  (* float ~allow_nan:false never produces NaN *)
  Hegel.run (fun () ->
      let f = (Hegel.Gen.float ~allow_nan:false ()).generate () in
      assert (not (Float.is_nan f)));

  (* float ~allow_infinity:false never produces infinity *)
  Hegel.run (fun () ->
      let f =
        (Hegel.Gen.float ~allow_infinity:false ~allow_nan:false ()).generate ()
      in
      assert (Float.is_finite f));

  (* string ~min_size respects minimum (codepoints) *)
  Hegel.run (fun () ->
      let s = (Hegel.Gen.string ~min_size:5 ()).generate () in
      assert (utf8_codepoint_count s >= 5));

  (* string ~max_size respects maximum (codepoints) *)
  Hegel.run (fun () ->
      let s = (Hegel.Gen.string ~max_size:3 ()).generate () in
      assert (utf8_codepoint_count s <= 3));

  (* from_regex generates matching strings *)
  Hegel.run (fun () ->
      let s = (Hegel.Gen.from_regex ~fullmatch:true "[a-z]{3,5}").generate () in
      assert (String.length s >= 3);
      assert (String.length s <= 5);
      String.iter (fun c -> assert (c >= 'a' && c <= 'z')) s);

  (* binary returns data *)
  Hegel.run (fun () ->
      let data = (Hegel.Gen.binary ~min_size:1 ~max_size:100 ()).generate () in
      assert (String.length data >= 1));

  (* list ~min_size ~max_size respects bounds *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:2 ~max_size:5 (Hegel.Gen.int ())).generate ()
      in
      assert (List.length xs >= 2);
      assert (List.length xs <= 5));

  (* array produces arrays *)
  Hegel.run (fun () ->
      let arr = (Hegel.Gen.array ~max_size:10 (Hegel.Gen.int ())).generate () in
      assert (Array.length arr <= 10);
      Array.iter (fun n -> ignore (n + 1)) arr);

  (* triple works *)
  Hegel.run (fun () ->
      let a, b, c =
        (Hegel.Gen.triple (Hegel.Gen.int ()) (Hegel.Gen.bool ())
           (Hegel.Gen.string ()))
          .generate
          ()
      in
      ignore (a, b, c));

  (* filter rejects non-matching values *)
  Hegel.run (fun () ->
      let even =
        (Hegel.Gen.filter
           (fun n -> n mod 2 = 0)
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate ()
      in
      assert (even mod 2 = 0));

  (* flat_map chains generators *)
  Hegel.run (fun () ->
      let s =
        (Hegel.Gen.flat_map
           (fun n -> Hegel.Gen.string ~min_size:n ~max_size:n ())
           (Hegel.Gen.int ~min:1 ~max:5 ()))
          .generate
          ()
      in
      assert (String.length s >= 1));

  (* just always returns same value *)
  Hegel.run (fun () ->
      let v = (Hegel.Gen.just 42).generate () in
      assert (v = 42));

  (* url generates valid-looking URLs *)
  Hegel.run (fun () ->
      let u = (Hegel.Gen.url ()).generate () in
      assert (String.length u > 0);
      let prefix = String.sub u 0 (min 4 (String.length u)) in
      assert (prefix = "http"));

  (* domain generates non-empty strings *)
  Hegel.run (fun () ->
      let d = (Hegel.Gen.domain ()).generate () in
      assert (String.length d > 0));

  (* ip_address ~version:`V4 generates IPv4 *)
  Hegel.run (fun () ->
      let ip = (Hegel.Gen.ip_address ~version:`V4 ()).generate () in
      assert (String.contains ip '.'));

  (* ip_address ~version:`V6 generates IPv6 *)
  Hegel.run (fun () ->
      let ip = (Hegel.Gen.ip_address ~version:`V6 ()).generate () in
      assert (String.contains ip ':'));

  (* date generates date-like strings *)
  Hegel.run (fun () ->
      let d = (Hegel.Gen.date ()).generate () in
      assert (String.length d >= 10);
      assert (d.[4] = '-'));

  (* time generates time-like strings *)
  Hegel.run (fun () ->
      let t = (Hegel.Gen.time ()).generate () in
      assert (String.contains t ':'));

  (* datetime generates datetime-like strings *)
  Hegel.run (fun () ->
      let dt = (Hegel.Gen.datetime ()).generate () in
      assert (String.contains dt 'T'));

  (* assume false skips without failing *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.assume (n > 50);
      assert (n > 50));

  (* note doesn't crash in normal execution *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ()).generate () in
      Hegel.note (Printf.sprintf "got %d" n);
      assert (n = n));

  (* target doesn't crash *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.target (Float.of_int n);
      assert (n >= 0));

  (* test_cases parameter works *)
  Hegel.run ~test_cases:5 (fun () ->
      let n = (Hegel.Gen.int ()).generate () in
      assert (n = n));

  (* Failed property raises Failure "Property test failed" *)
  (try
     Hegel.run (fun () ->
         let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
         assert (n < 0));
     assert false
   with Failure msg -> assert (msg = "Property test failed"))
