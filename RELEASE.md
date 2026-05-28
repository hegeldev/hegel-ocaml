RELEASE_TYPE: patch

Implements reproducing a test case with failure blobs. 
When a test fails, the following is outputted:

```
<some exception here>
...
[hegel] Failure blob(s) recorded:
[hegel] to replay, add to your test: [@@failure_blobs [ "<blob string>"; ... ]]
```

To replay:

```ocaml
let%hegel_test my_test tc = body
[@@failure_blobs [ "<blob string>"; ... ]]
```