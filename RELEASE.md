RELEASE_TYPE: patch

Handle `StopTest` in `collection_reject` so that unique non-basic lists terminate cleanly when the server's rejection limit is reached, instead of raising an unhandled protocol error.
