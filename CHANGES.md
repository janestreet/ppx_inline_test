## Release v0.16.0

- Renamed `Ppx_inline_test_runner.Runtime` to `Ppx_inline_test_runner`
- Renamed `Ppx_inline_test_runner.Runtime.am_running_inline_test{,_env_var}` to `Ppx_inline_test_runner.am_running{,_env_var}`
- New tag `let%test _ [@tags "disabled"]` for tests that shouldn't run by default
- Make the README state how to pass flags to the inline tests runner in jbuild/dune files
- A bit of progress towards supporting running tests in parallel with dune (from @hhugo)

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## v0.13.1

- Honor the `inline_tests` Dune variable so that inline tests are
  dropped in release builds

## v0.11

- Depend on ppxlib instead of (now deprecated) ppx\_core, ppx\_driver and
  ppx\_metaquot.

## 113.33.03

- Changed the runtime API to make it easier to build test runners:
  replace the `Runtime.Test_result.record` system by
  `Runtime.add_evaluator`

- Tell the build system via output metadata whether a file contains
  tests or not

## 113.33.00

- Allow to configure hooks for inline tests by redefining a module
  Inline\_test\_config.

## 113.24.00

- Support literate-style .ml files that allow ocaml code interleaved with expected output
  annotations. Compiling with the `ppx_expect_test` generates a program that outputs the
  original source file, but with the actual output substituted for the expected-output
  annotations. Then we can pat-diff the original file against the output file.

  Testing
  -------
  Examples in the test/ and example/ folders.

- Expect-tests can now be written inline in libraries by using `let%expect_test`.

  The runtime library has been split into two components: the test runner, which
  collects the output of the test body, and registers enough information to
  construct the `*.ml.corrected` file from the input; and the test evaluator,
  which compares the test output against the expected output and generates the
  output files.

- Update to follow `Ppx_core` evolution.

- When an exception is raised inside a `let%test_module`, display the position
  and name of the TEST\_MODULE, same as for the `let%test`.

- Mark attributes as handled inside explicitly dropped pieces of code.

  So that a `@@deriving` inside a let%test dropped by
  `ppx_inline_test_drop` doesn't cause a failure.
