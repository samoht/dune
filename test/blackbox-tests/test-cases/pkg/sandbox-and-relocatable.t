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
  BUILD_PATH_PREFIX_MAP=/OPAMROOT/lib=*:*/OPAMROOT=* (glob)

Test 2: Relocatability check - package with absolute paths should fail
=====================================================================

Create a package that embeds absolute paths in its output:

  $ make_lockpkg bad-pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "mkdir -p %{lib}/%{name}; echo 'prefix=%{prefix}' > %{lib}/%{name}/bad.pc"))
  > EOF

Building this package should fail because it embeds absolute paths:

  $ build_pkg bad-pkg 2>&1 | dune_cmd sanitize | head -20
  Error: Package bad-pkg is not relocatable. The following files contain
  absolute paths that would prevent sharing via the build cache:
  - */.pkg/*/target/lib/bad-pkg/bad.pc (glob)

  Packages should use relative paths or respect BUILD_PATH_PREFIX_MAP for
  reproducible builds.
  -> required by */.pkg/*/target (glob)

Test 3: Package using relative paths should succeed
==================================================

Create a package that uses relative paths:

  $ make_lockpkg good-pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "mkdir -p %{lib}/%{name}; echo 'prefix=\${pcfiledir}/../..' > %{lib}/%{name}/good.pc"))
  > EOF

  $ build_pkg good-pkg
  $ show_pkg_targets good-pkg | grep '\.pc'
  /lib/good-pkg/good.pc

Test 4: Sandbox isolation - build command runs in sandbox
========================================================

Create a package that tries to access files outside its source directory.
In a sandbox, the build should not be able to see files outside the sandbox:

First create a file outside the package source:

  $ echo "secret-data" > /tmp/outside-file-$$

Create a package that tries to read the file:

  $ make_lockpkg sandbox-test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "mkdir -p %{lib}/%{name}; if [ -f /tmp/outside-file-$$ ]; then echo 'LEAK: can see outside file'; else echo 'ISOLATED: cannot see outside file'; fi"))
  > EOF

When built with sandboxing, the package should not be able to access /tmp files
(Note: This depends on sandbox mode - symlink/copy/hardlink all provide isolation)

  $ build_pkg sandbox-test 2>&1 | grep -E '(LEAK|ISOLATED)'
  ISOLATED: cannot see outside file

Cleanup:

  $ rm -f /tmp/outside-file-$$

Test 5: Install commands can write to PREFIX
===========================================

Create a package with an install command that writes to the shared prefix:

  $ make_lockpkg install-to-prefix <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{lib}/%{name}; touch %{lib}/%{name}/installed-file"))
  > EOF

  $ build_pkg install-to-prefix
  $ show_pkg_targets install-to-prefix | grep installed
  /lib/install-to-prefix/installed-file

Test 6: Package with META file containing absolute paths should fail
===================================================================

META files are commonly checked for relocatability:

  $ make_lockpkg meta-with-abs-path <<EOF
  > (version 0.0.1)
  > (build
  >  (system "mkdir -p %{lib}/%{name}; echo 'directory=\"%{lib}/%{name}\"' > %{lib}/%{name}/META"))
  > EOF

  $ build_pkg meta-with-abs-path 2>&1 | dune_cmd sanitize | head -10
  Error: Package meta-with-abs-path is not relocatable. The following files
  contain absolute paths that would prevent sharing via the build cache:
  - */.pkg/*/target/lib/meta-with-abs-path/META (glob)

  Packages should use relative paths or respect BUILD_PATH_PREFIX_MAP for
  reproducible builds.
  -> required by */.pkg/*/target (glob)

Test 7: dune-package file with absolute paths should fail
========================================================

  $ make_lockpkg dune-pkg-with-abs-path <<EOF
  > (version 0.0.1)
  > (build
  >  (system "mkdir -p %{lib}/%{name}; echo '(name mylib) (dir %{lib}/%{name})' > %{lib}/%{name}/dune-package"))
  > EOF

  $ build_pkg dune-pkg-with-abs-path 2>&1 | dune_cmd sanitize | head -10
  Error: Package dune-pkg-with-abs-path is not relocatable. The following files
  contain absolute paths that would prevent sharing via the build cache:
  - */.pkg/*/target/lib/dune-pkg-with-abs-path/dune-package (glob)

  Packages should use relative paths or respect BUILD_PATH_PREFIX_MAP for
  reproducible builds.
  -> required by */.pkg/*/target (glob)
