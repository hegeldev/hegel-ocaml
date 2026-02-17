let test_composite_list () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:5
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x > 0)) xs)

let test_composite_array () =
  Hegel.run (fun () ->
      let arr =
        (Hegel.Gen.array ~min_size:1 ~max_size:5
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      Array.iter (fun x -> assert (x > 0)) arr)

let test_composite_pair () =
  Hegel.run (fun () ->
      let a, b =
        (Hegel.Gen.pair
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ()))
           (Hegel.Gen.int ()))
          .generate
          ()
      in
      assert (a > 0);
      ignore b)

let test_composite_triple () =
  Hegel.run (fun () ->
      let a, b, c =
        (Hegel.Gen.triple
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ()))
           (Hegel.Gen.int ()) (Hegel.Gen.bool ()))
          .generate
          ()
      in
      assert (a > 0);
      ignore (b, c))

let test_composite_map () =
  Hegel.run (fun () ->
      let s =
        (Hegel.Gen.map string_of_int
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      let n = int_of_string s in
      assert (n > 0))

let test_composite_one_of () =
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.one_of
           [
             Hegel.Gen.filter
               (fun x -> x > 0)
               (Hegel.Gen.int ~min:(-10) ~max:10 ());
             Hegel.Gen.int ~min:1 ~max:100 ();
           ])
          .generate
          ()
      in
      assert (n > 0))

let test_composite_optional () =
  Hegel.run (fun () ->
      let v =
        (Hegel.Gen.optional
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      match v with Some x -> assert (x > 0) | None -> ())

let test_flat_map () =
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.flat_map
           (fun x -> Hegel.Gen.int ~min:x ~max:(x + 10) ())
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate
          ()
      in
      assert (n >= 0 && n <= 110))

let test_note () =
  match
    Hegel.run (fun () ->
        let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
        Hegel.note (Printf.sprintf "Value: %d" n);
        assert (n < 50))
  with
  | () -> Alcotest.fail "expected Failure exception"
  | exception Failure msg ->
      Alcotest.(check string) "failure message" "Property test failed" msg

let test_assume () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.assume (n > 50);
      assert (n > 50))

let test_target () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.target ~label:"maximize" (Float.of_int n);
      assert (n >= 0))

let test_unit () =
  Hegel.run (fun () ->
      let () = (Hegel.Gen.unit ()).generate () in
      ())

let test_list_int32 () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int32 ~min:0l ~max:100l ()))
          .generate ()
      in
      List.iter (fun x -> assert (Int32.to_int x >= 0)) xs)

let test_list_int64 () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int64 ~min:0L ~max:100L ()))
          .generate ()
      in
      List.iter (fun x -> assert (Int64.to_int x >= 0)) xs)

let test_list_float () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.float ~min:0.0 ~max:1.0 ~allow_nan:false
              ~allow_infinity:false ()))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0.0)) xs)

let test_list_from_regex () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.from_regex ~fullmatch:true "[a-z]+"))
          .generate ()
      in
      List.iter (fun s -> assert (String.length s > 0)) xs)

let test_list_binary () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.binary ~min_size:1 ~max_size:10 ()))
          .generate ()
      in
      List.iter (fun s -> assert (String.length s >= 1)) xs)

let test_list_bool () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:5 (Hegel.Gen.bool ())).generate () in
      List.iter (fun b -> ignore b) xs)

let test_list_unit () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:3 (Hegel.Gen.unit ())).generate () in
      List.iter (fun () -> ()) xs)

let test_list_just () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:3 (Hegel.Gen.just 42)).generate () in
      List.iter (fun x -> assert (x = 42)) xs)

let test_list_string () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.string ~max_size:10 ())).generate
          ()
      in
      ignore xs)

let test_list_no_max_size () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:0
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x > 0)) xs)

let test_array_int () =
  Hegel.run (fun () ->
      let arr =
        (Hegel.Gen.array ~max_size:5 (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate ()
      in
      Array.iter (fun x -> assert (x >= 0)) arr)

let test_pair_basic () =
  Hegel.run (fun () ->
      let a, b =
        (Hegel.Gen.pair (Hegel.Gen.int ~min:0 ~max:10 ()) (Hegel.Gen.bool ()))
          .generate ()
      in
      assert (a >= 0);
      ignore b)

let test_triple_basic () =
  Hegel.run (fun () ->
      let a, b, c =
        (Hegel.Gen.triple
           (Hegel.Gen.int ~min:0 ~max:10 ())
           (Hegel.Gen.bool ())
           (Hegel.Gen.string ~max_size:5 ()))
          .generate
          ()
      in
      assert (a >= 0);
      ignore (b, c))

let test_one_of_basic () =
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.one_of
           [
             Hegel.Gen.int ~min:0 ~max:10 (); Hegel.Gen.int ~min:100 ~max:110 ();
           ])
          .generate
          ()
      in
      assert (n >= 0))

let test_list_one_of () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:5
           (Hegel.Gen.one_of
              [
                Hegel.Gen.int ~min:0 ~max:10 ();
                Hegel.Gen.int ~min:100 ~max:110 ();
              ]))
          .generate
          ()
      in
      List.iter (fun n -> assert (n >= 0)) xs)

let test_list_optional () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:5
           (Hegel.Gen.optional (Hegel.Gen.int ~min:0 ~max:100 ())))
          .generate
          ()
      in
      List.iter
        (fun v -> match v with Some x -> assert (x >= 0) | None -> ())
        xs)

let test_list_sampled_from () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:5 (Hegel.Gen.sampled_from [ "a"; "b"; "c" ]))
          .generate ()
      in
      List.iter (fun s -> assert (s = "a" || s = "b" || s = "c")) xs)

let test_list_email () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:2 (Hegel.Gen.email ())).generate () in
      List.iter (fun e -> assert (String.contains e '@')) xs)

let test_list_domain () =
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:2 (Hegel.Gen.domain ())).generate () in
      List.iter (fun d -> assert (String.length d > 0)) xs)

let test_list_ip_v4 () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:2 (Hegel.Gen.ip_address ~version:`V4 ()))
          .generate ()
      in
      List.iter (fun ip -> assert (String.contains ip '.')) xs)

let test_ip_no_version () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:2 (Hegel.Gen.ip_address ())).generate ()
      in
      List.iter (fun ip -> assert (String.length ip > 0)) xs)

let test_nested_list () =
  Hegel.run (fun () ->
      let xss =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter
        (fun xs -> List.iter (fun x -> assert (x >= 0 && x <= 10)) xs)
        xss)

let test_nested_pair () =
  Hegel.run (fun () ->
      let (a, b), c =
        (Hegel.Gen.pair
           (Hegel.Gen.pair (Hegel.Gen.int ()) (Hegel.Gen.bool ()))
           (Hegel.Gen.string ~max_size:5 ()))
          .generate
          ()
      in
      ignore (a, b, c))

let test_map_basic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.map string_of_int (Hegel.Gen.int ~min:0 ~max:100 ())))
          .generate
          ()
      in
      List.iter (fun s -> ignore (int_of_string s)) xs)

let test_flat_map_basic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.flat_map
              (fun x -> Hegel.Gen.int ~min:x ~max:(x + 10) ())
              (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0)) xs)

let test_int_negative_range () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:(-10) ~max:(-1) ()).generate () in
      assert (n >= -10 && n <= -1))

let test_int32_negative_max () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int32 ~min:(-100l) ~max:(-1l) ()).generate () in
      assert (Int32.to_int n >= -100 && Int32.to_int n <= -1))

let test_int64_negative_max () =
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int64 ~min:(-100L) ~max:(-1L) ()).generate () in
      assert (Int64.to_int n >= -100 && Int64.to_int n <= -1))

let test_list_pair_parse () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.pair
              (Hegel.Gen.int ~min:0 ~max:10 ())
              (Hegel.Gen.bool ())))
          .generate
          ()
      in
      List.iter (fun (a, _) -> assert (a >= 0 && a <= 10)) xs)

let test_list_triple_parse () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.triple
              (Hegel.Gen.int ~min:0 ~max:10 ())
              (Hegel.Gen.bool ())
              (Hegel.Gen.string ~max_size:5 ())))
          .generate
          ()
      in
      List.iter (fun (a, _, _) -> assert (a >= 0 && a <= 10)) xs)

let test_list_array_parse () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.array ~max_size:3 (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter
        (fun arr -> Array.iter (fun x -> assert (x >= 0 && x <= 10)) arr)
        xs)

let test_list_map_parse () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.map (fun x -> x * 2) (Hegel.Gen.int ~min:0 ~max:50 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0 && x <= 100)) xs)

let test_float_default_bounds () =
  Hegel.run (fun () ->
      let _f = (Hegel.Gen.float ()).generate () in
      ())

let test_binary_no_max () =
  Hegel.run (fun () ->
      let data = (Hegel.Gen.binary ~min_size:1 ()).generate () in
      assert (String.length data >= 1))

let test_list_no_max_basic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 (Hegel.Gen.int ~min:0 ~max:10 ())).generate
          ()
      in
      assert (List.length xs >= 1))

let test_pair_list_filter () =
  Hegel.run (fun () ->
      let xs, n =
        (Hegel.Gen.pair
           (Hegel.Gen.list ~min_size:1 ~max_size:3
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ())))
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate
          ()
      in
      List.iter (fun x -> assert (x > 0)) xs;
      assert (n >= 0))

let test_pair_array_filter () =
  Hegel.run (fun () ->
      let arr, n =
        (Hegel.Gen.pair
           (Hegel.Gen.array ~min_size:1 ~max_size:3
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ())))
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate
          ()
      in
      Array.iter (fun x -> assert (x > 0)) arr;
      assert (n >= 0))

let test_list_pair_filter () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.pair
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))
              (Hegel.Gen.int ())))
          .generate
          ()
      in
      List.iter (fun (a, _) -> assert (a > 0)) xs)

let test_list_triple_filter () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.triple
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))
              (Hegel.Gen.int ()) (Hegel.Gen.bool ())))
          .generate
          ()
      in
      List.iter (fun (a, _, _) -> assert (a > 0)) xs)

let test_int32_defaults () =
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.int32 ()).generate () in
      ())

let test_int64_defaults () =
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.int64 ()).generate () in
      ())

let test_from_regex_default () =
  Hegel.run (fun () ->
      let s = (Hegel.Gen.from_regex "[a-z]+").generate () in
      assert (String.length s > 0))

let test_binary_defaults () =
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.binary ()).generate () in
      ())

let test_list_no_max_in_pair () =
  Hegel.run (fun () ->
      let xs, b =
        (Hegel.Gen.pair
           (Hegel.Gen.list ~min_size:0 (Hegel.Gen.int ~min:0 ~max:10 ()))
           (Hegel.Gen.bool ()))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0 && x <= 10)) xs;
      ignore b)

let test_list_one_of_nonbasic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.one_of
              [
                Hegel.Gen.filter
                  (fun x -> x > 0)
                  (Hegel.Gen.int ~min:(-10) ~max:10 ());
                Hegel.Gen.int ~min:1 ~max:100 ();
              ]))
          .generate
          ()
      in
      List.iter (fun n -> assert (n > 0)) xs)

let test_list_optional_nonbasic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.optional
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))))
          .generate
          ()
      in
      List.iter
        (fun v -> match v with Some x -> assert (x > 0) | None -> ())
        xs)

let test_list_map_nonbasic () =
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.map string_of_int
              (Hegel.Gen.filter
                 (fun x -> x > 0)
                 (Hegel.Gen.int ~min:(-10) ~max:10 ()))))
          .generate
          ()
      in
      List.iter (fun s -> ignore (int_of_string s)) xs)

let test_protocol_debug () =
  Unix.putenv "HEGEL_PROTOCOL_DEBUG" "1";
  Hegel.run ~test_cases:1 (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:10 ()).generate () in
      assert (n >= 0))

let () =
  Alcotest.run "Hegel Coverage"
    [
      ( "Composite generators",
        [
          Alcotest.test_case "composite list" `Quick test_composite_list;
          Alcotest.test_case "composite array" `Quick test_composite_array;
          Alcotest.test_case "composite pair" `Quick test_composite_pair;
          Alcotest.test_case "composite triple" `Quick test_composite_triple;
          Alcotest.test_case "composite map" `Quick test_composite_map;
          Alcotest.test_case "composite one_of" `Quick test_composite_one_of;
          Alcotest.test_case "composite optional" `Quick test_composite_optional;
          Alcotest.test_case "flat_map" `Quick test_flat_map;
          Alcotest.test_case "note" `Quick test_note;
          Alcotest.test_case "assume" `Quick test_assume;
          Alcotest.test_case "target" `Quick test_target;
          Alcotest.test_case "unit" `Quick test_unit;
        ] );
      ( "as_basic parse callback coverage",
        [
          Alcotest.test_case "list(int32)" `Quick test_list_int32;
          Alcotest.test_case "list(int64)" `Quick test_list_int64;
          Alcotest.test_case "list(float)" `Quick test_list_float;
          Alcotest.test_case "list(from_regex)" `Quick test_list_from_regex;
          Alcotest.test_case "list(binary)" `Quick test_list_binary;
          Alcotest.test_case "list(bool)" `Quick test_list_bool;
          Alcotest.test_case "list(unit)" `Quick test_list_unit;
          Alcotest.test_case "list(just)" `Quick test_list_just;
          Alcotest.test_case "list(string)" `Quick test_list_string;
          Alcotest.test_case "list no max_size" `Quick test_list_no_max_size;
          Alcotest.test_case "array(int)" `Quick test_array_int;
          Alcotest.test_case "pair basic" `Quick test_pair_basic;
          Alcotest.test_case "triple basic" `Quick test_triple_basic;
          Alcotest.test_case "one_of basic" `Quick test_one_of_basic;
          Alcotest.test_case "list(one_of)" `Quick test_list_one_of;
          Alcotest.test_case "list(optional)" `Quick test_list_optional;
          Alcotest.test_case "list(sampled_from)" `Quick test_list_sampled_from;
          Alcotest.test_case "list(email)" `Quick test_list_email;
          Alcotest.test_case "list(domain)" `Quick test_list_domain;
          Alcotest.test_case "list(ip v4)" `Quick test_list_ip_v4;
          Alcotest.test_case "ip no version" `Quick test_ip_no_version;
          Alcotest.test_case "nested list" `Quick test_nested_list;
          Alcotest.test_case "nested pair" `Quick test_nested_pair;
          Alcotest.test_case "map basic" `Quick test_map_basic;
          Alcotest.test_case "flat_map basic" `Quick test_flat_map_basic;
        ] );
      ( "Negative max values",
        [
          Alcotest.test_case "int negative range" `Quick test_int_negative_range;
          Alcotest.test_case "int32 negative max" `Quick test_int32_negative_max;
          Alcotest.test_case "int64 negative max" `Quick test_int64_negative_max;
        ] );
      ( "as_basic parse for pair/triple/array",
        [
          Alcotest.test_case "list(pair) parse" `Quick test_list_pair_parse;
          Alcotest.test_case "list(triple) parse" `Quick test_list_triple_parse;
          Alcotest.test_case "list(array) parse" `Quick test_list_array_parse;
          Alcotest.test_case "list(map) parse" `Quick test_list_map_parse;
        ] );
      ( "Additional edge cases",
        [
          Alcotest.test_case "float default bounds" `Quick
            test_float_default_bounds;
          Alcotest.test_case "binary no max" `Quick test_binary_no_max;
          Alcotest.test_case "list no max basic" `Quick test_list_no_max_basic;
        ] );
      ( "as_basic None branches",
        [
          Alcotest.test_case "pair(list(filter))" `Quick test_pair_list_filter;
          Alcotest.test_case "pair(array(filter))" `Quick test_pair_array_filter;
          Alcotest.test_case "list(pair(filter))" `Quick test_list_pair_filter;
          Alcotest.test_case "list(triple(filter))" `Quick
            test_list_triple_filter;
        ] );
      ( "Default parameter coverage",
        [
          Alcotest.test_case "int32 defaults" `Quick test_int32_defaults;
          Alcotest.test_case "int64 defaults" `Quick test_int64_defaults;
          Alcotest.test_case "from_regex default" `Quick test_from_regex_default;
          Alcotest.test_case "binary defaults" `Quick test_binary_defaults;
          Alcotest.test_case "list no max in pair" `Quick
            test_list_no_max_in_pair;
          Alcotest.test_case "list(one_of nonbasic)" `Quick
            test_list_one_of_nonbasic;
          Alcotest.test_case "list(optional nonbasic)" `Quick
            test_list_optional_nonbasic;
          Alcotest.test_case "list(map nonbasic)" `Quick test_list_map_nonbasic;
        ] );
      ( "Protocol debug",
        [ Alcotest.test_case "HEGEL_PROTOCOL_DEBUG" `Quick test_protocol_debug ]
      );
    ]
