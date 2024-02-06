When running inline tests, jenga first gets the output of
[inline_tests_runner -list-partitions], then loops through those lines, testing
the partitions one-by-one. If code linked into [inline_tests_runner] produces
other toplevel output, it can mangle the partition list and cause jenga to try
running nonexistent tests instead (while the real ones are silently ignored).

  $ cd $TEST_DIR

  $ ./inline_tests_runner -list-partitions
  PRINTED FROM TEST FILEtest

  $ ./inline_tests_runner -list-partitions | while read partition; do ./inline_tests_runner -partition "$partition"; done
  PRINTED FROM TEST FILE (no-eol)

Since the test succeeded, it means no tests were run.

Instead, we use the [-list-partitions-into-file] flag, which will separate the
partition list from stray STDIO:

  $ PARTITION_FILE=tmp.partitions
  $ touch "${PARTITION_FILE}"

  $ ./inline_tests_runner -list-partitions-into-file $PARTITION_FILE
  PRINTED FROM TEST FILE (no-eol)

  $ cat $PARTITION_FILE
  test

  $ while read partition; do ./inline_tests_runner -partition "$partition"; done < $PARTITION_FILE
  File "test.ml", line 2, characters 0-18: <<false>> is false.
  
  FAILED 1 / 1 tests
  PRINTED FROM TEST FILE (no-eol)
  [2]

This time, the test failed (as it should).

To help catch situations where jenga and the inline_tests_runner can't
communicate over the filesystem (because, for example, the inline_tests_runner
has been wrapped in bwrap), the inline_tests_runner complains if asked to list
partitions into a file that does not exist.

  $ rm $PARTITION_FILE
  $ OCAMLRUNPARAM=b=0 ./inline_tests_runner -list-partitions-into-file $PARTITION_FILE
  PRINTED FROM TEST FILEFatal error: exception Sys_error("tmp.partitions: No such file or directory")
  [2]


In reality, jenga does not interact with the inline test runner using real
files. Instead, it runs a script that manipulates some file descriptors to
collect the partition list like so:

  $ exec {stdout}>&1; ./inline_tests_runner -list-partitions-into-file /dev/fd/${stdout} > /dev/null
  test

  $ ( exec {stdout}>&1; ./inline_tests_runner -list-partitions-into-file /dev/fd/${stdout} > /dev/null ) | while read partition; do ./inline_tests_runner -partition "$partition"; done
  File "test.ml", line 2, characters 0-18: <<false>> is false.
  
  FAILED 1 / 1 tests
  PRINTED FROM TEST FILE (no-eol)
  [2]
