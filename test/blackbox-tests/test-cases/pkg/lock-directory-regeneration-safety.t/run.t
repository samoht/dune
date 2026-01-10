Create a lock directory that didn't originally exist

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories mock))
  > (lock_dir
  >  (path "dev/dune.lock")
  >  (repositories mock))
  > EOF
  $ add_mock_repo_if_needed

  $ dune_pkg_lock_normalized "dev/dune.lock" --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]
  $ dune_pkg_lock_normalized --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]
  $ cat ${default_lock_dir}/lock.dune
  cat: dune.lock/lock.dune: No such file or directory
  [1]



Re-create a lock directory in the newly created lock dir
  $ dune_pkg_lock_normalized --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]
  $ cat ${default_lock_dir}/lock.dune
  cat: dune.lock/lock.dune: No such file or directory
  [1]



Attempt to create a lock directory inside an existing directory without a lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-without-metadata ${default_lock_dir}
  $ dune_pkg_lock_normalized --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]


Attempt to create a lock directory inside an existing directory with an invalid lock.dune file

  $ rm -rf ${default_lock_dir}
  $ cp -r dir-with-invalid-metadata ${default_lock_dir}
  $ dune_pkg_lock_normalized --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]



Attempt to create a lock directory with the same name as an existing regular file

  $ rm -rf ${default_lock_dir}
  $ touch ${default_lock_dir}
  $ dune_pkg_lock_normalized --format=directory
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 10, characters 6-191:
  10 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/d039220a9eb466222e271cb1bca8385d/default/test/blackbox-tests/test-cases/pkg/lock-directory-regeneration-safety.t/mock-opam-repository"
  [1]


