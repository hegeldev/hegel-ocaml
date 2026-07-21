RELEASE_TYPE: patch

This patch fixes redundant output from generated functions on a failing replay.
When a drawn function was applied more than once to the same argument, every
application printed its own `name arg = result` line, so a loop calling `f 10`
three times produced three identical lines. Now, under the default verbosity 
only the first application with a given argument prints. Applications with that
same argument after do not print. Under `Verbose` or `Debug` verbosity every
application prints.
