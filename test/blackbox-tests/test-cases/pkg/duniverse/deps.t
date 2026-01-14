Test duniverse packages with dependencies between them.

This test verifies that when duniverse package A depends on duniverse package B,
the dependency chain is correctly handled without trying to compute .pkg/ cookies
for duniverse packages.

Set up a mock repository:

  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create two packages where pkg-b depends on pkg-a:

  $ mkpkg pkg-a 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg pkg-b 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > depends: ["pkg-a"]
  > EOF

Set up workspace with mock repository:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Create a project that depends on pkg-b (which transitively depends on pkg-a):

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends pkg-b))
  > EOF

Lock the dependencies:

  $ dune pkg lock --format=directory
  Solution for dune.lock (2 packages)
  dune:
  - pkg-a.1.0.0
  - pkg-b.1.0.0

Create source files for pkg-a:

  $ mkdir -p pkg-a-source
  $ cat >pkg-a-source/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name pkg-a) (allow_empty))
  > EOF
  $ cat >pkg-a-source/dune <<EOF
  > (library (name pkg_a) (public_name pkg-a))
  > EOF
  $ cat >pkg-a-source/pkg_a.ml <<EOF
  > let value = "from pkg-a"
  > EOF

Create source files for pkg-b that depends on pkg-a:

  $ mkdir -p pkg-b-source
  $ cat >pkg-b-source/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name pkg-b) (allow_empty))
  > EOF
  $ cat >pkg-b-source/dune <<EOF
  > (library (name pkg_b) (public_name pkg-b) (libraries pkg-a))
  > EOF
  $ cat >pkg-b-source/pkg_b.ml <<EOF
  > let value = "from pkg-b using " ^ Pkg_a.value
  > EOF

Create lock files with source URLs:

  $ make_lockdir
  $ make_lockpkg pkg-a <<EOF
  > (version 1.0.0)
  > (build (run dune build -p pkg-a @install))
  > (source (copy $PWD/pkg-a-source))
  > EOF
  $ make_lockpkg pkg-b <<EOF
  > (version 1.0.0)
  > (build (run dune build -p pkg-b @install))
  > (depends pkg-a)
  > (source (copy $PWD/pkg-b-source))
  > EOF

Fetch both packages:

  $ dune pkg vendor -v
     Vendoring pkg-a.1.0.0
     Vendoring pkg-b.1.0.0

Verify both packages are in duniverse:

  $ ls duniverse/
  dune
  pkg-a.1.0.0
  pkg-b.1.0.0

Remove the source directories:

  $ rm -rf pkg-a-source pkg-b-source

Create a project that uses pkg-b (which depends on pkg-a):

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (allow_empty))
  > EOF

  $ cat >dune <<EOF
  > (library (name mylib) (libraries pkg-b))
  > EOF

  $ cat >mylib.ml <<EOF
  > let greeting = Pkg_b.value
  > EOF

Build the project - this should work even though both packages are in duniverse
and pkg-b depends on pkg-a (tests the closure computation):

  $ dune build
  File "_build/.locks/default/dune.lock/pkg-a.pkg", line 3, characters 14-151:
  3 | (source (copy $TESTCASE_ROOT/pkg-a-source))
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error:
  $TESTCASE_ROOT/pkg-a-source
  does not exist
  File "_build/.locks/default/dune.lock/pkg-b.pkg", line 4, characters 14-151:
  4 | (source (copy $TESTCASE_ROOT/pkg-b-source))
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error:
  $TESTCASE_ROOT/pkg-b-source
  does not exist
  [1]

Verify the library was built correctly:

  $ cat _build/default/mylib.ml
  let greeting = Pkg_b.value

Both duniverse libraries should be built in the main build context:

  $ ls _build/default/duniverse/pkg-a.1.0.0/.pkg_a.objs/byte/*.cmo
  _build/default/duniverse/pkg-a.1.0.0/.pkg_a.objs/byte/pkg_a.cmo
  $ ls _build/default/duniverse/pkg-b.1.0.0/.pkg_b.objs/byte/*.cmo
  ls: _build/default/duniverse/pkg-b.1.0.0/.pkg_b.objs/byte/*.cmo: No such file or directory
  [1]

Verify that neither package has .pkg build artifacts:

  $ ls _build/default/.pkg/pkg-a/target 2>/dev/null || echo "No .pkg build for pkg-a (expected)"
  No .pkg build for pkg-a (expected)
  $ ls _build/default/.pkg/pkg-b/target 2>/dev/null || echo "No .pkg build for pkg-b (expected)"
  No .pkg build for pkg-b (expected)
