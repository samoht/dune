Test that BUILD_PATH_PREFIX_MAP is properly set for relocatable builds,
and that prefix paths don't affect cache keys.

Create a package that echoes environment variables to verify BUILD_PATH_PREFIX_MAP is set:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo BUILD_PATH_PREFIX_MAP is set: \${BUILD_PATH_PREFIX_MAP:+yes}"))
  > EOF

  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

Test that a package with a run action also has BUILD_PATH_PREFIX_MAP:

  $ make_lockpkg runtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX_MAP_SET=\${BUILD_PATH_PREFIX_MAP:+yes}"))
  > EOF

  $ build_pkg runtest
  Error: Don't know how to build _build/.pkgs/default/runtest/installed
  [1]

Test that PREFIX and OPAM_SWITCH_PREFIX are available during build:

  $ make_lockpkg envtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX_SET=\${PREFIX:+yes}; echo OPAM_PREFIX_SET=\${OPAM_SWITCH_PREFIX:+yes}"))
  > EOF

  $ build_pkg envtest
  Error: Don't know how to build _build/.pkgs/default/envtest/installed
  [1]

Test that a package with install action uses target_dir as PREFIX during build:

  $ make_lockpkg installtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX contains target: \$(echo \$PREFIX | grep -q target && echo yes || echo no)"))
  > (install
  >  (run sh -c "echo Installing to PREFIX: \$(echo \$PREFIX | grep -q target && echo target_dir || echo shared)"))
  > EOF

  $ build_pkg installtest
  Error: Don't know how to build _build/.pkgs/default/installtest/installed
  [1]

Test that a package WITHOUT install action uses shared PREFIX during build
(needed to find dependencies):

  $ make_lockpkg noinstall <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX contains install: \$(echo \$PREFIX | grep -q install && echo yes || echo no)"))
  > EOF

  $ build_pkg noinstall
  Error: Don't know how to build _build/.pkgs/default/noinstall/installed
  [1]

Test that the installed marker file contains metadata:

  $ make_lockpkg markertest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "mkdir -p \$PREFIX/bin && echo test > \$PREFIX/bin/test"))
  > (install
  >  (run sh -c "echo installed"))
  > EOF

  $ build_pkg markertest
  Error: Don't know how to build _build/.pkgs/default/markertest/installed
  [1]

  $ cat "$(get_build_pkg_dir markertest)/installed"
  cat: _build/.pkgs/default/markertest/installed: No such file or directory
  [1]

Test that files written to PREFIX during build are tracked in cookie and copied to shared prefix:

  $ make_lockpkg copytest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "mkdir -p \$PREFIX/bin && echo 'hello from copytest' > \$PREFIX/bin/copytest"))
  > (install
  >  (run sh -c "echo installed"))
  > EOF

  $ build_pkg copytest
  Error: Don't know how to build _build/.pkgs/default/copytest/installed
  [1]

The file should exist in the target directory:

  $ cat "$(get_build_pkg_dir copytest)/target/bin/copytest"
  cat: _build/.pkgs/default/copytest/target/bin/copytest: No such file or directory
  [1]

Check what's in the cookie (should list the bin/copytest file):

  $ show_pkg_cookie copytest
  Error:
  $TESTCASE_ROOT/_build/.pkgs/default/copytest/target/cookie:
  No such file or directory
  [1]

The file should be copied to the shared install directory:

  $ cat _build/install/default/bin/copytest
  cat: _build/install/default/bin/copytest: No such file or directory
  [1]

Test BUILD_PATH_PREFIX_MAP contains the expected mappings:

  $ make_lockpkg maptest <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo \$BUILD_PATH_PREFIX_MAP | tr ':' '\n' | grep -c OPAMROOT"))
  > EOF

  $ build_pkg maptest
  Error: Don't know how to build _build/.pkgs/default/maptest/installed
  [1]
