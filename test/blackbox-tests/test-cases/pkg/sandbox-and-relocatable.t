Test sandbox isolation and relocatability checks for package builds

Setup a simple project and lock directory:

  $ cat > dune-project << EOF
  > (lang dune 3.12)
  > EOF

  $ make_lockdir

Test 1: BUILD_PATH_PREFIX_MAP is set during package builds
=========================================================

Create a package that prints its build environment to verify BUILD_PATH_PREFIX_MAP:

  $ make_lockpkg check-env <<EOF
  > (version 0.0.1)
  > (build (system "echo BUILD_PATH_PREFIX_MAP=\$BUILD_PATH_PREFIX_MAP | head -c 200"))
  > EOF

  $ build_pkg check-env 2>&1 | grep -o 'BUILD_PATH_PREFIX_MAP=.*' | head -1

Test 2: Package with absolute paths (relocatability check TODO)
===============================================================

Create a package that embeds absolute paths in its output:

  $ make_lockpkg bad-pkg <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; echo 'prefix=%{prefix}' > %{lib}/%{pkg-self:name}/bad.pc"))
  > EOF

Building this package currently succeeds (relocatability checks not yet implemented):

  $ build_pkg bad-pkg
  Error: Don't know how to build _build/.pkgs/default/bad-pkg/installed
  [1]
  $ show_pkg_targets bad-pkg | grep '\.pc'
  [1]

Test 3: Package using relative paths should succeed
==================================================

Create a package that uses relative paths:

  $ make_lockpkg good-pkg <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; echo 'prefix=\${pcfiledir}/../..' > %{lib}/%{pkg-self:name}/good.pc"))
  > EOF

  $ build_pkg good-pkg
  Error: Don't know how to build _build/.pkgs/default/good-pkg/installed
  [1]
  $ show_pkg_targets good-pkg | grep '\.pc'
  [1]

Test 4: Sandbox isolation (TODO - currently not isolating /tmp)
==============================================================

Create a package that tries to access files outside its source directory.
In a sandbox, the build should not be able to see files outside the sandbox:

First create a file outside the package source:

  $ echo "secret-data" > /tmp/outside-file-$$

Create a package that tries to read the file:

  $ make_lockpkg sandbox-test <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; if [ -f /tmp/outside-file-$$ ]; then echo 'LEAK: can see outside file'; else echo 'ISOLATED: cannot see outside file'; fi"))
  > EOF

Currently sandboxing does not isolate /tmp (this may be improved in the future):

  $ build_pkg sandbox-test 2>&1 | grep -E '(LEAK|ISOLATED)'
  [1]

Cleanup:

  $ rm -f /tmp/outside-file-$$

Test 5: Install commands can write to PREFIX
===========================================

Create a package with an install command that writes to the shared prefix:

  $ make_lockpkg install-to-prefix <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; touch %{lib}/%{pkg-self:name}/installed-file"))
  > EOF

  $ build_pkg install-to-prefix
  Error: Don't know how to build
  _build/.pkgs/default/install-to-prefix/installed
  [1]
  $ show_pkg_targets install-to-prefix | grep installed
  [1]

Test 6: Package with META file containing absolute paths (TODO)
===============================================================

META files are commonly checked for relocatability:

  $ make_lockpkg meta-with-abs-path <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; echo 'directory=\"%{lib}/%{pkg-self:name}\"' > %{lib}/%{pkg-self:name}/META"))
  > EOF

Currently builds succeed (relocatability checks not yet implemented):

  $ build_pkg meta-with-abs-path
  Error: Don't know how to build
  _build/.pkgs/default/meta-with-abs-path/installed
  [1]
  $ show_pkg_targets meta-with-abs-path | grep META
  [1]

Test 7: dune-package file with absolute paths (TODO)
====================================================

  $ make_lockpkg dune-pkg-with-abs-path <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{pkg-self:name}; echo '(name mylib) (dir %{lib}/%{pkg-self:name})' > %{lib}/%{pkg-self:name}/dune-package"))
  > EOF

Currently builds succeed (relocatability checks not yet implemented):

  $ build_pkg dune-pkg-with-abs-path
  Error: Don't know how to build
  _build/.pkgs/default/dune-pkg-with-abs-path/installed
  [1]
  $ show_pkg_targets dune-pkg-with-abs-path | grep dune-package
  [1]
