Note that test output here is heavily sanitized due to tools using different
exit codes and error messages, see #11560 for example output

Create a mock package whose url is a corrupted/invalid tar file attempt to
build this package and check for sufficient error handling

  $ echo "corrupted tar" > corrupted.tar

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.tar"
  > }
  > EOF

  $ add_mock_repo_if_needed
  $ solve foo
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  [1]
  $ build_pkg foo 2>&1 | dune_cmd print-from 'Error:' | dune_cmd print-until '^Reason' | dune_cmd subst "'[0-9]*'" X
  Error: Lock directory is not active for context "default".

Repeat the same test as above but ensure that error output from gzip is
captured

  $ echo "corrupted tar.gz" > corrupted.tar.gz

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.tar.gz"
  > }
  > EOF

  $ solve foo
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  [1]

  $ build_pkg foo 2>&1 | dune_cmd print-from 'Error:' | dune_cmd print-until '^Reason' | dune_cmd subst "'[0-9]*'" X
  Error: Lock directory is not active for context "default".

Now try another local package but this time of zip format to test if stderr is
captured from the unzip tool. Note that preprocessing here makes the unzip
error message a bit less clear

  $ echo "corrupted zip" > corrupted.zip

  $ mkpkg foo <<EOF
  > url {
  >  src: "corrupted.zip"
  > }
  > EOF

  $ add_mock_repo_if_needed
  $ solve foo
  fatal: '$TESTCASE_ROOT/mock-opam-repository' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune-workspace", line 6, characters 6-154:
  6 |  (url "git+file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file:///Users/samoht/git/dune/_build/.sandbox/dd3a5444de8c08f01ff37af872c9d22b/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository"
  [1]

  $ build_pkg foo 2>&1 | dune_cmd print-from 'Error:' | dune_cmd print-until '^Reason' | dune_cmd subst "'[0-9]*'" X
  Error: Lock directory is not active for context "default".
