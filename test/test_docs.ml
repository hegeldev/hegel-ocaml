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

let test_int_bounds () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:10 ~max:20 ()).generate () in
      assert (n >= 10);
      assert (n <= 20))

let test_int32 () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int32 ~min:(-1000l) ~max:1000l ()).generate () in
      assert (Int32.to_int n >= -1000);
      assert (Int32.to_int n <= 1000))

let test_int64 () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int64 ~min:(-1000L) ~max:1000L ()).generate () in
      assert (Int64.to_int n >= -1000);
      assert (Int64.to_int n <= 1000))

let test_float_no_nan () =
  Hegel.run (fun () ->
      let f = (Hegel.Gen.float ~allow_nan:false ()).generate () in
      assert (not (Float.is_nan f)))

let test_float_no_infinity () =
  Hegel.run (fun () ->
      let f =
        (Hegel.Gen.float ~allow_infinity:false ~allow_nan:false ()).generate ()
      in
      assert (Float.is_finite f))

let test_string_min_size () =
  Hegel.run (fun () ->
      let s = (Hegel.Gen.string ~min_size:5 ()).generate () in
      assert (utf8_codepoint_count s >= 5))

let test_string_max_size () =
  Hegel.run (fun () ->
      let s = (Hegel.Gen.string ~max_size:3 ()).generate () in
      assert (utf8_codepoint_count s <= 3))

let test_from_regex () =
  Hegel.run (fun () ->
      let s = (Hegel.Gen.from_regex ~fullmatch:true "[a-z]{3,5}").generate () in
      assert (String.length s >= 3);
      assert (String.length s <= 5);
      String.iter (fun c -> assert (c >= 'a' && c <= 'z')) s)

let test_binary () =
  Hegel.run (fun () ->
      let data = (Hegel.Gen.binary ~min_size:1 ~max_size:100 ()).generate () in
      assert (String.length data >= 1))

let test_list_bounds () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:2 ~max_size:5 (Hegel.Gen.int ())).generate ()
      in
      assert (List.length xs >= 2);
      assert (List.length xs <= 5))

let test_array () =
  Hegel.run (fun () ->
      let arr = (Hegel.Gen.array ~max_size:10 (Hegel.Gen.int ())).generate () in
      assert (Array.length arr <= 10);
      Array.iter (fun n -> ignore (n + 1)) arr)

let test_triple () =
  Hegel.run (fun () ->
      let a, b, c =
        (Hegel.Gen.triple (Hegel.Gen.int ()) (Hegel.Gen.bool ())
           (Hegel.Gen.string ()))
          .generate
          ()
      in
      ignore (a, b, c))

let test_filter () =
  Hegel.run (fun () ->
      let even =
        (Hegel.Gen.filter
           (fun n -> n mod 2 = 0)
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate ()
      in
      assert (even mod 2 = 0))

let test_flat_map () =
  Hegel.run (fun () ->
      let s =
        (Hegel.Gen.flat_map
           (fun n -> Hegel.Gen.string ~min_size:n ~max_size:n ())
           (Hegel.Gen.int ~min:1 ~max:5 ()))
          .generate
          ()
      in
      assert (String.length s >= 1))

let test_just () =
  Hegel.run (fun () ->
      let v = (Hegel.Gen.just 42).generate () in
      assert (v = 42))

let test_url () =
  Hegel.run (fun () ->
      let u = (Hegel.Gen.url ()).generate () in
      assert (String.length u > 0);
      let prefix = String.sub u 0 (min 4 (String.length u)) in
      assert (prefix = "http"))

let test_domain () =
  Hegel.run (fun () ->
      let d = (Hegel.Gen.domain ()).generate () in
      assert (String.length d > 0))

let test_ip_v4 () =
  Hegel.run (fun () ->
      let ip = (Hegel.Gen.ip_address ~version:`V4 ()).generate () in
      assert (String.contains ip '.'))

let test_ip_v6 () =
  Hegel.run (fun () ->
      let ip = (Hegel.Gen.ip_address ~version:`V6 ()).generate () in
      assert (String.contains ip ':'))

let test_date () =
  Hegel.run (fun () ->
      let d = (Hegel.Gen.date ()).generate () in
      assert (String.length d >= 10);
      assert (d.[4] = '-'))

let test_time () =
  Hegel.run (fun () ->
      let t = (Hegel.Gen.time ()).generate () in
      assert (String.contains t ':'))

let test_datetime () =
  Hegel.run (fun () ->
      let dt = (Hegel.Gen.datetime ()).generate () in
      assert (String.contains dt 'T'))

let test_assume () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.assume (n > 50);
      assert (n > 50))

let test_note () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ()).generate () in
      Hegel.note (Printf.sprintf "got %d" n);
      assert (n = n))

let test_target () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.target (Float.of_int n);
      assert (n >= 0))

let test_test_cases_param () =
  Hegel.run ~test_cases:5 (fun () ->
      let n = (Hegel.Gen.int ()).generate () in
      assert (n = n))

let test_failed_property_raises () =
  match
    Hegel.run (fun () ->
        let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
        assert (n < 0))
  with
  | () -> Alcotest.fail "expected Failure exception"
  | exception Failure msg ->
      Alcotest.(check string) "failure message" "Property test failed" msg

let () =
  Alcotest.run "Hegel Docs"
    [
      ( "numeric",
        [
          Alcotest.test_case "int bounds" `Quick test_int_bounds;
          Alcotest.test_case "int32" `Quick test_int32;
          Alcotest.test_case "int64" `Quick test_int64;
          Alcotest.test_case "float no nan" `Quick test_float_no_nan;
          Alcotest.test_case "float no infinity" `Quick test_float_no_infinity;
        ] );
      ( "string",
        [
          Alcotest.test_case "min size" `Quick test_string_min_size;
          Alcotest.test_case "max size" `Quick test_string_max_size;
          Alcotest.test_case "from_regex" `Quick test_from_regex;
          Alcotest.test_case "binary" `Quick test_binary;
        ] );
      ( "collections",
        [
          Alcotest.test_case "list bounds" `Quick test_list_bounds;
          Alcotest.test_case "array" `Quick test_array;
          Alcotest.test_case "triple" `Quick test_triple;
        ] );
      ( "combinators",
        [
          Alcotest.test_case "filter" `Quick test_filter;
          Alcotest.test_case "flat_map" `Quick test_flat_map;
          Alcotest.test_case "just" `Quick test_just;
        ] );
      ( "format strings",
        [
          Alcotest.test_case "url" `Quick test_url;
          Alcotest.test_case "domain" `Quick test_domain;
          Alcotest.test_case "ip v4" `Quick test_ip_v4;
          Alcotest.test_case "ip v6" `Quick test_ip_v6;
          Alcotest.test_case "date" `Quick test_date;
          Alcotest.test_case "time" `Quick test_time;
          Alcotest.test_case "datetime" `Quick test_datetime;
        ] );
      ( "control flow",
        [
          Alcotest.test_case "assume" `Quick test_assume;
          Alcotest.test_case "note" `Quick test_note;
          Alcotest.test_case "target" `Quick test_target;
          Alcotest.test_case "test_cases param" `Quick test_test_cases_param;
          Alcotest.test_case "failed property raises" `Quick
            test_failed_property_raises;
        ] );
    ]
