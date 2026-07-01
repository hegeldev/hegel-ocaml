RELEASE_TYPE: patch

Adds database key as an optional argument to `Hegel.run_hegel_test`. If `ppx_hegel_test` 
is used, the key "file_name:test_name" is automatically passed in by let%hegel_test.
