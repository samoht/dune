Test that installed binaries are visible in dependent packages

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| echo "#!/bin/sh\necho from test package" > foo;
  >          "\| chmod +x foo;
  >          "\| touch libxxx lib_rootxxx;
  >          "\| cat >test.install <<EOF
  >          "\| bin: [ "foo" ]
  >          "\| lib: [ "libxxx" ]
  >          "\| lib_root: [ "lib_rootxxx" ]
  >          "\| share_root: [ "lib_rootxxx" ]
  >          "\| EOF
  >  ))
  > EOF

  $ make_lockpkg usetest <<EOF
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (progn
  >   (run foo)
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  Error: Don't know how to build _build/.pkgs/default/usetest/installed
  [1]

  $ show_pkg_targets test
  [1]
  $ show_pkg_cookie test
  Error:
  $TESTCASE_ROOT/_build/.pkgs/default/test/target/cookie:
  No such file or directory
  [1]

It should also be visible in the workspace:

  $ cat >dune-project <<EOF
  > (lang dune 3.9)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout (run %{bin:foo})))
  > EOF

  $ dune build ./testout && cat _build/default/testout
  File "dune", lines 1-2, characters 0-49:
  1 | (rule
  2 |  (with-stdout-to testout (run %{bin:foo})))
  Error: No rule found for .pkgs/test/target/cookie
  File "dune", lines 1-2, characters 0-49:
  1 | (rule
  2 |  (with-stdout-to testout (run %{bin:foo})))
  Error: No rule found for .pkgs/usetest/target/cookie
  [1]
