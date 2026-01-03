Test that duniverse packages work with various dune-workspace features.

This test verifies that dune pkg integrates well with:
- Multiple build contexts
- Vendored directory handling (automatic for duniverse)
- Warning suppression in duniverse packages

Set up a project with multiple contexts:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (context (default))
  > (context (default (name release)))
  > EOF

  $ cat >dune <<EOF
  > (library (name myproj) (public_name myproj) (libraries test-lib))
  > EOF

  $ cat >myproj.ml <<EOF
  > let x = Test_lib.value
  > EOF

Create duniverse with a package source:

  $ mkdir -p duniverse
  $ touch duniverse/.dune-duniverse

  $ mkdir -p duniverse/test-lib.1.0.0
  $ cat >duniverse/test-lib.1.0.0/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test-lib))
  > EOF

  $ cat >duniverse/test-lib.1.0.0/dune <<EOF
  > (library (name test_lib) (public_name test-lib))
  > EOF

  $ cat >duniverse/test-lib.1.0.0/test_lib.ml <<EOF
  > let value = "test-lib"
  > EOF

Build in all contexts:

  $ dune build @install 2>&1 | head -20

The library should be built in both contexts:

  $ ls _build/default/.myproj.objs/byte/*.cmo
  _build/default/.myproj.objs/byte/myproj.cmo

  $ ls _build/release/.myproj.objs/byte/*.cmo
  _build/release/.myproj.objs/byte/myproj.cmo

The duniverse library should also be built in both contexts:

  $ ls _build/default/duniverse/test-lib.1.0.0/.test_lib.objs/byte/*.cmo
  _build/default/duniverse/test-lib.1.0.0/.test_lib.objs/byte/test_lib.cmo

  $ ls _build/release/duniverse/test-lib.1.0.0/.test_lib.objs/byte/*.cmo
  _build/release/duniverse/test-lib.1.0.0/.test_lib.objs/byte/test_lib.cmo

Verify that warnings are suppressed in duniverse (vendored behavior):

  $ cat >duniverse/test-lib.1.0.0/test_lib.ml <<EOF
  > (* This unused binding would normally cause a warning *)
  > let unused_value = "unused"
  > let value = "test-lib"
  > EOF

  $ dune build @install 2>&1 | grep -i warning || echo "No warnings (expected)"
  No warnings (expected)
