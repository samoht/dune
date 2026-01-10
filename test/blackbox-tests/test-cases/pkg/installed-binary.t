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
  Error: execve(../../test.0.0.1/target/bin/foo): No such file or directory
  -> required by _build/.pkgs/default/usetest.0.0.1/target/cookie
  -> required by _build/.pkgs/default/usetest.0.0.1/installed
  [1]

  $ show_pkg_targets test
  /bin
  /bin/foo
  /lib
  /lib/lib_rootxxx
  /lib/test
  /lib/test/libxxx
  /share
  /share/lib_rootxxx
  $ show_pkg_cookie test
  { files =
      [ (LIB,
         [ In_build_dir ".pkgs/default/test.0.0.1/target/lib/test/libxxx" ])
      ; (LIB_ROOT,
         [ In_build_dir ".pkgs/default/test.0.0.1/target/lib/lib_rootxxx" ])
      ; (BIN, [ In_build_dir ".pkgs/default/test.0.0.1/target/bin/foo" ])
      ; (SHARE_ROOT,
         [ In_build_dir ".pkgs/default/test.0.0.1/target/share/lib_rootxxx" ])
      ]
  ; variables = []
  }

It should also be visible in the workspace:

  $ cat >dune-project <<EOF
  > (lang dune 3.9)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout (run %{bin:foo})))
  > EOF

  $ dune build ./testout && cat _build/default/testout
  Error: execve(../../test.0.0.1/target/bin/foo): No such file or directory
  -> required by _build/.pkgs/default/usetest.0.0.1/target/cookie
  -> required by Loading all binaries in the lock directory for "default"
  -> required by looking up binary "foo" in context "default"
  -> required by %{bin:foo} at dune:2
  -> required by _build/default/testout
  [1]
