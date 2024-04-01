This file tests the accessor functions exposed by [Ppx_inline_test_lib]. We
check that the functions can't be called if the test settings have not yet been
initialized, either because the program is running from an
[inline_tests_runner] executable, or because [init] has been called.

  $ cd $TEST_DIR

We'd rather not have the exact contents of the help text in the test output below, so we
filter out lines that start with two spaces.

  $ sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib -show-using-color before
  Checking whether the inline tests should [use_color]
  <Error>
  (Failure
    "ppx_inline_test error: attempted to access the [use_color] config before [init] was called")

  $ sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib -show-using-color after
  (About to call [init] the first time)
  <OK>
  Checking whether the inline tests should [use_color]
  <OK>
  [use_color] is true
