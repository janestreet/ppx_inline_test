This file tests the [init] function exposed by [Ppx_inline_test_lib]. We check
that the function can't be called if the test settings were already
initialized, either because the program is running from an
[inline_tests_runner] executable, or because [init] has already been called. We
also check that help, success, and error cases are reported correctly.

  $ cd $TEST_DIR

  $ ./inline_tests_runner
  The inline test runner can only be initialized once, and has already been initialized.

We'd rather not have the exact contents of the help text in the test output below, so we
filter out lines that start with two spaces.

  $ sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib -help | grep -vE '^  '
  (About to call [init] the first time)
  <Help>
  sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib [args]
  
  (About to call [init] the second time)
  <Error>
  The inline test runner can only be initialized once, and has already been initialized.
  (Was able to call [init] twice)

  $ sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib -list-partitions
  (About to call [init] the first time)
  <OK>
  (About to call [init] the second time)
  <Error>
  The inline test runner can only be initialized once, and has already been initialized.
  (Was able to call [init] twice)

  $ sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib -lst-prt | grep -vE '^  '
  (About to call [init] the first time)
  <Error>
  sample-client/inline_tests_sample_client.exe: unknown option '-lst-prt'.
  sample-client/inline_tests_sample_client.exe inline-test-runner dummy-lib [args]
  
  (About to call [init] the second time)
  <Error>
  The inline test runner can only be initialized once, and has already been initialized.
  (Was able to call [init] twice)

