(* Integration tests exercising composite (non-basic) code paths.
   All generators that return as_basic = None take the composite branch
   inside list/array/pair/triple/map/one_of/optional. *)

let () =
  (* Composite list: filter returns as_basic () = None, so list takes
     the new_collection / collection_more path *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:5
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x > 0)) xs);

  (* Composite array: delegates to composite list internally *)
  Hegel.run (fun () ->
      let arr =
        (Hegel.Gen.array ~min_size:1 ~max_size:5
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      Array.iter (fun x -> assert (x > 0)) arr);

  (* Composite pair: one non-basic element forces the composite tuple path *)
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
      ignore b);

  (* Composite triple: one non-basic element forces the composite tuple path *)
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
      ignore (b, c));

  (* Composite map: filter makes underlying generator non-basic *)
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
      assert (n > 0));

  (* Composite one_of: mix of basic and non-basic generators *)
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
      assert (n > 0));

  (* Composite optional: filter makes it non-basic *)
  Hegel.run (fun () ->
      let v =
        (Hegel.Gen.optional
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      match v with Some x -> assert (x > 0) | None -> ());

  (* flat_map: always takes composite path (as_basic = None) *)
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.flat_map
           (fun x -> Hegel.Gen.int ~min:x ~max:(x + 10) ())
           (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate
          ()
      in
      assert (n >= 0 && n <= 110));

  (* note: exercises note output on last run (during shrink replay) *)
  (try
     Hegel.run (fun () ->
         let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
         Hegel.note (Printf.sprintf "Value: %d" n);
         assert (n < 50));
     assert false
   with Failure msg -> assert (msg = "Property test failed"));

  (* assume false: exercises Assume_rejected handling *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.assume (n > 50);
      assert (n > 50));

  (* target with label: exercises target function *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:100 ()).generate () in
      Hegel.target ~label:"maximize" (Float.of_int n);
      assert (n >= 0));

  (* unit generator *)
  Hegel.run (fun () ->
      let () = (Hegel.Gen.unit ()).generate () in
      ());

  (* ================================================================ *)
  (* as_basic parse callback coverage: wrap generators in containers  *)
  (* These exercise the parse functions returned by as_basic()        *)
  (* ================================================================ *)

  (* list(int()) already covered, but ensure list(int32()) exercises int32 parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int32 ~min:0l ~max:100l ()))
          .generate ()
      in
      List.iter (fun x -> assert (Int32.to_int x >= 0)) xs);

  (* list(int64()) exercises int64 as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int64 ~min:0L ~max:100L ()))
          .generate ()
      in
      List.iter (fun x -> assert (Int64.to_int x >= 0)) xs);

  (* list(float()) exercises float as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.float ~min:0.0 ~max:1.0 ~allow_nan:false
              ~allow_infinity:false ()))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0.0)) xs);

  (* list(from_regex) exercises from_regex as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.from_regex ~fullmatch:true "[a-z]+"))
          .generate ()
      in
      List.iter (fun s -> assert (String.length s > 0)) xs);

  (* list(binary()) exercises binary as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.binary ~min_size:1 ~max_size:10 ()))
          .generate ()
      in
      List.iter (fun s -> assert (String.length s >= 1)) xs);

  (* list(bool()) exercises bool as_basic parse *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:5 (Hegel.Gen.bool ())).generate () in
      List.iter (fun b -> ignore b) xs);

  (* list(unit()) exercises unit as_basic parse *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:3 (Hegel.Gen.unit ())).generate () in
      List.iter (fun () -> ()) xs);

  (* list(just 42) exercises just as_basic parse *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:3 (Hegel.Gen.just 42)).generate () in
      List.iter (fun x -> assert (x = 42)) xs);

  (* list(string()) exercises string as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3 (Hegel.Gen.string ~max_size:10 ())).generate
          ()
      in
      ignore xs);

  (* list without max_size: exercises ensure_initialized with None max_size *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:0
           (Hegel.Gen.filter
              (fun x -> x > 0)
              (Hegel.Gen.int ~min:(-10) ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x > 0)) xs);

  (* array(int()) exercises array as_basic parse *)
  Hegel.run (fun () ->
      let arr =
        (Hegel.Gen.array ~max_size:5 (Hegel.Gen.int ~min:0 ~max:100 ()))
          .generate ()
      in
      Array.iter (fun x -> assert (x >= 0)) arr);

  (* pair of basics exercises pair as_basic parse *)
  Hegel.run (fun () ->
      let a, b =
        (Hegel.Gen.pair (Hegel.Gen.int ~min:0 ~max:10 ()) (Hegel.Gen.bool ()))
          .generate ()
      in
      assert (a >= 0);
      ignore b);

  (* triple of basics exercises triple as_basic parse *)
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
      ignore (b, c));

  (* one_of with basics exercises one_of as_basic parse *)
  Hegel.run (fun () ->
      let n =
        (Hegel.Gen.one_of
           [
             Hegel.Gen.int ~min:0 ~max:10 (); Hegel.Gen.int ~min:100 ~max:110 ();
           ])
          .generate
          ()
      in
      assert (n >= 0));

  (* list(one_of) exercises one_of as_basic parse callback *)
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
      List.iter (fun n -> assert (n >= 0)) xs);

  (* list(optional) exercises optional/one_of as_basic parse callback *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:5
           (Hegel.Gen.optional (Hegel.Gen.int ~min:0 ~max:100 ())))
          .generate
          ()
      in
      List.iter
        (fun v -> match v with Some x -> assert (x >= 0) | None -> ())
        xs);

  (* list(sampled_from) exercises sampled_from as_basic parse callback *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:5 (Hegel.Gen.sampled_from [ "a"; "b"; "c" ]))
          .generate ()
      in
      List.iter (fun s -> assert (s = "a" || s = "b" || s = "c")) xs);

  (* list(email()) exercises simple_format as_basic parse callback *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:2 (Hegel.Gen.email ())).generate () in
      List.iter (fun e -> assert (String.contains e '@')) xs);

  (* list(domain()) exercises domain as_basic parse callback *)
  Hegel.run (fun () ->
      let xs = (Hegel.Gen.list ~max_size:2 (Hegel.Gen.domain ())).generate () in
      List.iter (fun d -> assert (String.length d > 0)) xs);

  (* list(ip_address ~version:`V4) exercises ip_address as_basic parse callback *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:2 (Hegel.Gen.ip_address ~version:`V4 ()))
          .generate ()
      in
      List.iter (fun ip -> assert (String.contains ip '.')) xs);

  (* ip_address without version: tests the None branch *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:2 (Hegel.Gen.ip_address ())).generate ()
      in
      List.iter (fun ip -> assert (String.length ip > 0)) xs);

  (* list(list(int())) exercises nested list as_basic parse callback *)
  Hegel.run (fun () ->
      let xss =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.list ~max_size:3 (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter
        (fun xs -> List.iter (fun x -> assert (x >= 0 && x <= 10)) xs)
        xss);

  (* pair(pair(...)) exercises nested pair as_basic parse callback *)
  Hegel.run (fun () ->
      let (a, b), c =
        (Hegel.Gen.pair
           (Hegel.Gen.pair (Hegel.Gen.int ()) (Hegel.Gen.bool ()))
           (Hegel.Gen.string ~max_size:5 ()))
          .generate
          ()
      in
      ignore (a, b, c));

  (* map with basic: exercises map as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.map string_of_int (Hegel.Gen.int ~min:0 ~max:100 ())))
          .generate
          ()
      in
      List.iter (fun s -> ignore (int_of_string s)) xs);

  (* flat_map as_basic (always None, but exercises the path) *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 ~max_size:3
           (Hegel.Gen.flat_map
              (fun x -> Hegel.Gen.int ~min:x ~max:(x + 10) ())
              (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0)) xs);

  (* ================================================================ *)
  (* Negative max values in numeric schemas                          *)
  (* ================================================================ *)

  (* int with both min and max negative: exercises Cbor.Negative max *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int ~min:(-10) ~max:(-1) ()).generate () in
      assert (n >= -10 && n <= -1));

  (* int32 with negative max *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int32 ~min:(-100l) ~max:(-1l) ()).generate () in
      assert (Int32.to_int n >= -100 && Int32.to_int n <= -1));

  (* int64 with negative max *)
  Hegel.run (fun () ->
      let n = (Hegel.Gen.int64 ~min:(-100L) ~max:(-1L) ()).generate () in
      assert (Int64.to_int n >= -100 && Int64.to_int n <= -1));

  (* ================================================================ *)
  (* list() wrapping to trigger as_basic parse for pair/triple/array *)
  (* ================================================================ *)

  (* list(pair(...)) triggers pair.as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.pair
              (Hegel.Gen.int ~min:0 ~max:10 ())
              (Hegel.Gen.bool ())))
          .generate
          ()
      in
      List.iter (fun (a, _) -> assert (a >= 0 && a <= 10)) xs);

  (* list(triple(...)) triggers triple.as_basic parse *)
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
      List.iter (fun (a, _, _) -> assert (a >= 0 && a <= 10)) xs);

  (* list(array(...)) triggers array.as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.array ~max_size:3 (Hegel.Gen.int ~min:0 ~max:10 ())))
          .generate
          ()
      in
      List.iter
        (fun arr -> Array.iter (fun x -> assert (x >= 0 && x <= 10)) arr)
        xs);

  (* list(map(...)) triggers map.as_basic parse *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~max_size:3
           (Hegel.Gen.map (fun x -> x * 2) (Hegel.Gen.int ~min:0 ~max:50 ())))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0 && x <= 100)) xs);

  (* ================================================================ *)
  (* Additional edge case coverage                                    *)
  (* ================================================================ *)

  (* float with default (infinite) bounds - exercises the else branches *)
  Hegel.run (fun () ->
      let _f = (Hegel.Gen.float ()).generate () in
      ());

  (* binary without max_size *)
  Hegel.run (fun () ->
      let data = (Hegel.Gen.binary ~min_size:1 ()).generate () in
      assert (String.length data >= 1));

  (* list without max_size in basic path *)
  Hegel.run (fun () ->
      let xs =
        (Hegel.Gen.list ~min_size:1 (Hegel.Gen.int ~min:0 ~max:10 ())).generate
          ()
      in
      assert (List.length xs >= 1));

  (* ================================================================ *)
  (* as_basic None branches: non-basic generators inside containers  *)
  (* ================================================================ *)

  (* pair(list(filter...), int): list.as_basic returns None *)
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
      assert (n >= 0));

  (* pair(array(filter...), int): array.as_basic returns None *)
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
      assert (n >= 0));

  (* list(pair(filter..., int)): pair.as_basic returns None *)
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
      List.iter (fun (a, _) -> assert (a > 0)) xs);

  (* list(triple(filter..., int, bool)): triple.as_basic returns None *)
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
      List.iter (fun (a, _, _) -> assert (a > 0)) xs);

  (* ================================================================ *)
  (* Default parameter coverage                                       *)
  (* ================================================================ *)

  (* int32 with defaults *)
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.int32 ()).generate () in
      ());

  (* int64 with defaults — now uses native int range *)
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.int64 ()).generate () in
      ());

  (* from_regex without fullmatch (default false) *)
  Hegel.run (fun () ->
      let s = (Hegel.Gen.from_regex "[a-z]+").generate () in
      assert (String.length s > 0));

  (* binary with defaults *)
  Hegel.run (fun () ->
      let _ = (Hegel.Gen.binary ()).generate () in
      ());

  (* list(int) in as_basic path without max_size - wrap in pair *)
  Hegel.run (fun () ->
      let xs, b =
        (Hegel.Gen.pair
           (Hegel.Gen.list ~min_size:0 (Hegel.Gen.int ~min:0 ~max:10 ()))
           (Hegel.Gen.bool ()))
          .generate
          ()
      in
      List.iter (fun x -> assert (x >= 0 && x <= 10)) xs;
      ignore b);

  (* list(one_of with non-basic): one_of.as_basic returns None, list uses composite *)
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
      List.iter (fun n -> assert (n > 0)) xs);

  (* list(optional with non-basic): optional.as_basic returns None *)
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
        xs);

  (* list(map with non-basic): map.as_basic returns None *)
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
      List.iter (fun s -> ignore (int_of_string s)) xs);

  (* HEGEL_PROTOCOL_DEBUG: exercises debug output in send_request.
     This must be the LAST test since we can't unsetenv in OCaml. *)
  Unix.putenv "HEGEL_PROTOCOL_DEBUG" "1";
  Hegel.run ~test_cases:1 (fun () ->
      let n = (Hegel.Gen.int ~min:0 ~max:10 ()).generate () in
      assert (n >= 0));

