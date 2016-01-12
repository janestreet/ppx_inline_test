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
