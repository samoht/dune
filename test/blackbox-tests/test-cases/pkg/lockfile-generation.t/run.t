Simple example of generating a lock file with Dune

Helper shell function that generates an opam file for a package:

  $ emptypkg() {
  >   mkpkg $1 <<EOF
  > EOF
  > }
  $ emptyverpkg() {
  >   mkpkg $1 $2 <<EOF
  > EOF
  > }

Generate a `dune-project` file.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.3"))))
  > EOF
  > mkpkg foo <<EOF
  > depends: [
  >     "baz" {>= "0.1"}
  >     "bar" {>= "0.2"}
  > ]
  > EOF
  $ add_mock_repo_if_needed

Run the solver and generate a lock directory.

  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]

Helper to the name and contents of each file in the lock directory separated by
"---", sorting by filename for consistency.
  $ print_all() { find ${default_lock_dir} -type f | sort | xargs -I{} sh -c "printf '{}:\n\n'; cat {}; printf '\n\n---\n\n'"; }

Print the contents of each file in the lockdir:
  $ print_all
  find: dune.lock: No such file or directory

Run the solver again preferring oldest versions of dependencies:
  $ dune_pkg_lock_normalized --version-preference=oldest
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]

  $ print_all
  find: dune.lock: No such file or directory

Regenerate the `dune-project` file introducing an unsatisfiable constraint.
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends
  >    foo
  >    (bar (>= "0.6"))))
  > EOF

Run the solver again. This time it will fail.
  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]

We'll also test how the lockfile generation works with alternate solutions.
`bar-or-baz` is a package that depends on either `bar` or `baz` and the solver
should pick one of them.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends bar-or-baz))
  > EOF
  $ mkpkg bar-or-baz <<EOF
  > depends: [ "bar" | "baz" ]
  > EOF

After running this we expact a solution that has either `bar` or `baz` but not
both.

  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]
Top level or is simple, but does nested or work? nested-r defines nested or
patterns that can't be simplified

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends nested-or))
  > EOF
  $ emptypkg qux
  > emptypkg quz
  $ emptypkg quux
  $ emptypkg corge
  $ mkpkg nested-or <<EOF
  > depends: [ "quux" (("baz" | "quz") & ("bar" | "qux")) ]
  > EOF

After runninng we expect the solution to have quux and either baz or quz as
well as bar or qux.

  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]
In the dependency formulas, & should bind stronger than | so if we depend on
bar and quux or baz, it should pick the first two or the last one, but nothing
in between.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends priorities))
  > EOF
  $ mkpkg priorities <<EOF
  > depends: [ ("bar" & "quux") | "baz" ]
  > EOF

  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]
 
We also want to make sure nested negation in versions work fine. For this we
have the same package with version 1-4 and we want to negate the choice of
versions 1 or 3, as well as making sure it doesn't pick the newest version.
 
  $ emptyverpkg pkg 1
  $ emptyverpkg pkg 2
  $ emptyverpkg pkg 3
  $ emptyverpkg pkg 4
  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name lockfile_generation_test)
  >  (depends negation))
  > EOF
  $ mkpkg negation <<EOF
  > depends: [ "pkg" {!((= "1") | (= "3")) & (< "4")} ]
  > EOF

With versions 1 and 3 negated and version 4 removed via version constraint,
we'd expect version 2 to be chosen:

  $ dune_pkg_lock_normalized
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-176:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/da1268f68d17ef40b6da706f210178e4/default/test/blackbox-tests/test-cases/pkg/lockfile-generation.t/mock-opam-repository"
  [1]
