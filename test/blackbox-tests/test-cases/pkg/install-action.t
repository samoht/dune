Testing install actions

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Check that the file was installed to the shared install directory:
  $ find _build/install/default -name "xxx" 2>/dev/null
  [1]

  $ show_pkg_targets test
  [1]

  $ show_pkg_cookie test
  Error:
  $TESTCASE_ROOT/_build/.pkgs/default/test/target/cookie:
  No such file or directory
  [1]
