
Generate a mock opam repository
  $ mkdir -p mock-opam-repository
  $ cat >mock-opam-repository/repo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg foo <<EOF
  > install: [
  >   ["echo" "package: %{name}%.%{version}%"]
  >   ["echo" "enable: %{enable}%"]
  >   ["echo" "installed: %{installed}%"]
  >   ["echo" "string selection: %{installed?foo:bar}%"]
  >   ["echo" "package conjunction: %{foo+bar+_:installed}%"]
  >   ["echo" "package conjunction string selection: %{foo+bar+_:installed?foo:bar}%"]
  > ]
  > EOF

  $ mkpkg bar <<EOF
  > install: [
  >   ["echo" "installed"] { installed }
  >   ["echo" "pinned"] { pinned }
  >   ["echo" "installed or pinned"] { installed | pinned }
  >   ["echo" "installed and pinned"] { installed & pinned }
  >   ["echo" "version greater than 0.1 (version is %{version}%)"] { version > "0.1" }
  >   ["echo" "version greater than 0 (version is %{version}%)"] { version > "0" }
  >   ["echo" "disjunction with some undefined vars"] { madeup | false | installed | madeup2 }
  >   ["echo" "conjunction with some undefined vars"] { ! (madeup & false & installed & madeup2) }
  >   ["echo" "check if variable 'madeup' is defined"] { ? madeup }
  >   ["echo" "check if variable 'installed' is defined"] { ? installed }
  > ]
  > EOF

  $ mkpkg baz <<EOF
  > install: [
  >   ["echo" "installed" { installed } "not installed" { ! installed }]
  >   ["echo" "madeup:installed" { madeup:installed } "not madeup:installed" { ! madeup:installed }]
  >   ["not-a-program" { ! (2 < 3) } "echo" "hello" ]
  >   ["echo" "madeup-defined" { ? madeup } "installed-defined" { ? installed } ]
  > ]
  > EOF

  $ mkpkg error1 <<EOF
  > install: [
  >   ["echo" "disjunction with all undefined or false vars"] { a | b | false | c | madeup:installed }
  > ]
  > EOF

  $ mkpkg error2 <<EOF
  > install: [
  >   ["echo" "conjunction with all undefined or true vars"] { a & b & true & c & installed }
  > ]
  > EOF

  $ mkpkg error3 <<EOF
  > install: [
  >   ["not-a-program" { ! (2 < 1) } "echo" "hello" ]
  > ]
  > EOF

  $ mkpkg error4 <<EOF
  > install: [
  >   ["not-a-program-%{name}%" { ! (2 < 1) } "echo" "hello" ]
  > ]
  > EOF

  $ build_single_package() {
  > solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends
  >   $1))
  > EOF
  > build_pkg $1
  > }

  $ build_single_package foo
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package bar
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package baz
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package error1
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package error2
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package error3
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
  $ build_single_package error4
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/00afbfdb14533f72c5c633bb3eb1b1db/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  Error: Lock directory is not active for context "default".
  [1]
