Testing install actions

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ build_pkg test
  foobar

Check that the file was installed to the shared install directory:
  $ find _build/install/default -name "xxx" 2>/dev/null
  _build/install/default/lib/xxx

  $ show_pkg_targets test
  /lib
  /lib/xxx

  $ show_pkg_cookie test
  { files =
      [ (LIB_ROOT, [ In_build_dir ".pkgs/default/test/target/lib/xxx" ]) ]
  ; variables = []
  }
