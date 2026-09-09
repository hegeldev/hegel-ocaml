RELEASE_TYPE: minor

This release adds test statistics and functions for recording observations.

`event : test_case -> label:string -> unit` records `label` as observed within a test case.
`event_value : test_case -> label:string -> value:float -> unit` records numeric observations.
`with_show_statistics : bool -> settings -> settings` sets whether or not to print statistics.
When `true`, statistics are printed after every run.

```ocaml
let%hegel_test list_statistics tc =
  let xs = draw_silent tc (lists (integers ()) ()) in
  (match xs with
   | [] -> event tc ~label:"empty input"
   | _ -> ());
  event_value tc ~label:"length" ~value:(float_of_int (List.length xs))
```

```
Statistics (over 100 test cases):
  * empty input: 5.0% of test cases
  * length: count 100, min 0, median 4, mean 5.12, p90 9, max 15
```

This release also changes `target` to take its arguments as labels.
