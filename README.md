ppx_inline_test
===============

Syntax extension for writing in-line tests in ocaml code.

New syntactic constructs
------------------------

The following constructs are now valid structure items:

```ocaml
let%test "name" = <boolean expr> (* true means ok, false or exn means broken *)
let%test_unit "name" = <unit expr> (* () means ok, exn means broken *)
let%test_module "name" = (module <module-expr>)  (* to group tests (to share
                                                    some setup for instance) *)
```

We may write `_` instead of `"name"` for anonymous tests. It is also possible to use
`[%name <string expr>]` for a dynamically computed name.


When running tests, they will be executed when the control flow reaches the structure item
(i.e. at toplevel for a toplevel test; when the functor is applied for a test defined in
the body of a functor, etc.).

Tags
----
One can tag tests with the following construct:

```ocaml
let%test "name" [@tags "no-js"] = <expr>
let%test "name" [@tags "no-js", "other-tag"] = <expr>
let%test _ [@tags "no-js"] = <expr>
let%test _ [@tags "js-only"] = <expr>
```

Available tags are:

*   `no-js` for tests that should not run when compiling Ocaml to Javascript

*   `js-only` for tests that should only run in Javascript

*   `32-bits-only` for tests that should only run in 32 bits architectures

*   `64-bits-only` for tests that should only run in 64 bits architectures

*   `fast-flambda` for tests that might only pass when compiling with flambda or
    flambda2, -O3, and cross library inlining

*   `fast-flambda2` for tests that might only pass when compiling with flambda2, -O3,
    and cross library inlining

*   `x-library-inlining-sensitive` for tests that might only pass when compiling
    with cross library inlining switched on

*   `disabled` for tests that should not run (unless requested with -require-tag)

One can also tag entire test modules similarly:

```ocaml
let%test_module "name" [@tags "no-js"] = (module struct end)
```

The flags `-drop-tag` and `-require-tag` can be passed to the test runner to restrict
which tests are run. We say the tags of a test are the union of the tags applied directly
to that test using `[@tags ...]` and the tags of all enclosing modules. It is to this
union that the predicates `-drop-tag` and `-require-tag` are applied.

If it is clear, from a test-module's tags, that none of the tests within will possibly
match the tag predicates imposed by the command line flags, then additionally the
top-level of that module will not be run.

Examples
--------

### prime.ml

```ocaml
let is_prime = <magic>

let%test _ = is_prime 5
let%test _ = is_prime 7
let%test _ = not (is_prime 1)
let%test _ = not (is_prime 8)
```

### Tests in a functor.

```ocaml
module Make(C : S) = struct
     <magic>
     let%test _ = <some expression>
end

module M = Make(Int)
```

### Grouping test and side-effecting initialisation.

Since the module passed as an argument to `let%test_module` is only initialised when we
run the tests, it is ok to perform side-effects in the module-expression argument.

```ocaml
let%test_module _ = (module struct
    module UID = Unique_id.Int(struct end)

    let%test _ = UID.create() <> UID.create()
end)
```

Building and running the tests at jane street
--------------------------------

Inline tests can only be used in libraries, not executables.

The standard build rules create an executable script `inline_tests_runner` which runs all
tests in the directory. This script takes optional arguments (see below) to restrict which
tests are run.

The full set of tests are run when building the jenga `runtest` alias.

    jenga .runtest

Building and running the tests outside of jane street with dune
----------------------------------------

Inline tests can only be used in libraries, not executables.

To use this with dune, see [dune's documentation](https://dune.readthedocs.io/en/latest/tests.html).
At the time of writing of the current document, the short version is:
* define a library this way:
```lisp
(library
  (name foo)
  (inline_tests)
  (preprocess (pps ppx_inline_test)))
```
* add tests to it
* call `dune runtest`

Building and running the tests outside of jane street without dune
----------------------------------------

Code using this extension must be compiled and linked using the
`ppx_inline_test.runtime-lib` library. The `ppx_inline_test` syntax extension will reject
any test if it wasn't passed a `-inline-test-lib libname` flag.

#### Execution

Tests are only executed when both these conditions are met:

- the executable containing the tests is linked with `ppx_inline_test.runner.lib`
- the executable containing the tests is called with command line arguments:

    your.exe inline-test-runner libname [options]

This `libname` is a way of restricting the tests run by the executable. The dependencies
of your library (or executable) could also use `ppx_inline_test`, but you don't
necessarily want to run their tests too. For instance, `core` is built by giving
`-inline-test-lib core` and `core_extended` is built by giving `-inline-test-lib
core_extended`. And now when an executable linked with both `core` and `core_extended` is
run with a `libname` of `core_extended`, only the tests of `core_extended` are run.

Finally, after running tests, `Ppx_inline_test_lib.exit ()` should be called (to
exit with an error and a summary of the number of failed tests if there were errors or
exit normally otherwise).

One can construct a dual-use binary that only runs the tests when prompted to (through the
command line), by sticking the following piece of code in it, after the tests have run but
before the binary starts doing non-test side effects. However be aware that
`Base.am_testing` will be `true` even when not running tests, which may be undesirable.

```ocaml
match Ppx_inline_test_lib.testing with
| `Testing `Am_test_runner ->
  print_endline "Exiting test suite";
  Ppx_inline_test_lib.exit ()
| `Testing _ -> exit 0
| `Not_testing -> ()
```

Command line arguments
----------------------

The executable that runs tests can take additional command line arguments. The most useful
of these are:

* `-stop-on-error`

    Stop running tests after the first error.

*   `-verbose`

    to see the tests as they run

*   `-only-test location`

    where location is either a filename `-only-test main.ml`, a filename
    with a line number `-only-test main.ml:32`, or with the syntax that the
    compiler uses: `File "main.ml"`, or `File "main.ml", line 32` or `File "main.ml",
    line 32, characters 2-6` (characters are ignored).
    The position that matters is the position of the `let%test` or `let%test_unit`.

    The positions shown by `-verbose` are valid inputs for `-only-test`.

    If no `-only-test` flag is given, all the tests are
    run. Otherwise all the tests matching any of the locations are run.

*   `-drop-tag tag`
 
    drop all the tests tagged with `tag`.

These can be specified to jenga like this:

```
(library
  (...
   (inline_tests ((flags (-stop-on-error))))
   ...
  ))
```

and to dune like this:

```
(library
  ...
  (inline_tests (flags (-stop-on-error)))
  ...)
```

Parallelizing tests
-------------------

If you pass arguments of the form `-inline-test-lib lib:partition` to `ppx_inline_test`,
then you will be able to run tests from a given source file in parallel with tests from
other source files. All the tests inside the same source file are still run sequentially.

You should pick different `partition` names for the different files in your library (the
name of the .ml files for instance).

`ppx_inline_test_lib` currently requires some external system like a build system to run
it multiple times in parallel, although we may make it possible to run the inline tests in
parallel directly in the future.

If you do that, you can now use two new flags of the executable containing the tests:

*   `-list-partitions`

    lists all the partitions that contain at least one test, one per line.

*   `-partition P`

    only run the tests of the library that are encountered at toplevel of the source file
    that was preprocessed with the given partition `P` (the tests need not be
    syntactically in the file, they could be the result of applying a functor)

A build system can combine these two commands by first listing partitions, and then
running one command for each partition.
