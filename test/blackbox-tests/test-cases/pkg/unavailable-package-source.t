Demonstrate what happens when we try to fetch from a source that doesn't exist:

  $ make_lockdir

  $ runtest() {
  > make_lockpkg foo <<EOF
  > (build (run echo building))
  > (source $1)
  > (version dev)
  > EOF
  > build_pkg foo 2>&1
  > }

Local file system
  $ runtest "(copy \"$PWD/dummy\")" 2>&1 \
  >  | dune_cmd subst "$PWD" 'PWD' \
  >  | dune_cmd delete ' *\^\^*$' \
  >  | dune_cmd delete '^File ".*dune.lock/foo.pkg", line 2, characters'
  Error:
  stat(PWD/dummy): No such file or directory

Git
  $ runtest "(fetch (url \"git+file://$PWD/dummy\"))" 2>&1 \
  > | dune_cmd subst "$PWD" 'PWD' \
  > | sanitize_pkg_digest foo.dev \
  > | dune_cmd subst '/url/[a-f0-9]+' '/url/DIGEST'
  fatal: 'PWD/dummy' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  File "dune.lock/foo.pkg", line 2, characters 20-153:
  2 | (source (fetch (url "git+file://PWD/dummy")))
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Failed to run external command:
  'git ls-remote "file://PWD/dummy"'
  Hint: Check that this Git URL in the project configuration is correct:
  "file://PWD/dummy"

HTTP

  $ runtest "(fetch (url \"https://0.0.0.0:35000\"))" 2>&1 \
  >  | dune_cmd print-from 'Error:' \
  >  | dune_cmd print-until '^Reason' \
  >  | dune_cmd subst "'[0-9]*'" 'X'
  Error: failed to extract 'download'
  Reason: 'tar' failed with non-zero exit code X and output:
