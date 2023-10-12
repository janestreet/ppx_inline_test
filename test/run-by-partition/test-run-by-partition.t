When running inline tests, jenga first gets the output of
[inline_tests_runner -list-partitions], then loops through those lines, testing
the partitions one-by-one. If code linked into [inline_tests_runner] produce
other toplevel output, it can mangle the partition list and cause jenga to try
running nonexistent tests instead (while the real ones are silently ignored).

  $ cd $TEST_DIR

  $ ./inline_tests_runner -list-partitions
  PRINTED FROM TEST FILEtest

  $ ./inline_tests_runner -list-partitions | while read partition; do ./inline_tests_runner -partition "$partition"; done
  PRINTED FROM TEST FILE (no-eol)

  $ PARTITION_FILE=tmp.partitions

  $ ./inline_tests_runner -list-partitions-into-file $PARTITION_FILE
  PRINTED FROM TEST FILE (no-eol)

  $ cat $PARTITION_FILE
  test

  $ while read partition; do ./inline_tests_runner -partition "$partition"; done < $PARTITION_FILE
  File "test.ml", line 2, characters 0-18: <<false>> is false.
  
  FAILED 1 / 1 tests
  PRINTED FROM TEST FILE (no-eol)
  [2]

  $ rm $PARTITION_FILE
