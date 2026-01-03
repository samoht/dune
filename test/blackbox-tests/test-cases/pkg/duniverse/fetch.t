Test the dune pkg fetch command for duniverse packages.

Set up a mock repository:

  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create a package that uses dune as its build system:

  $ mkpkg dune-pkg 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
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

Create a project that depends on the dune package:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends dune-pkg))
  > EOF

Lock the dependencies:

  $ dune pkg lock
  Solution for dune.lock (1 package)
  dune:
  - dune-pkg.1.0.0

Check that fetch command shows message when duniverse packages have no sources:

  $ dune pkg fetch
  1 dune package(s) have no source URL (likely local packages).

Now let's test with a package that has an actual source URL.

Create source files for a dune package:

  $ mkdir -p pkg-source
  $ cat >pkg-source/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name mypkg) (allow_empty))
  > EOF
  $ cat >pkg-source/dune <<EOF
  > (library (name mypkg) (public_name mypkg))
  > EOF
  $ cat >pkg-source/mypkg.ml <<EOF
  > let hello = "hello from mypkg"
  > EOF

Create a lock file with a source URL pointing to our local directory:

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 1.0.0)
  > (build (run dune build -p mypkg @install))
  > (source (copy $PWD/pkg-source))
  > EOF

Fetch the package:

  $ dune pkg fetch
  Fetching mypkg.1.0.0 to duniverse/mypkg.1.0.0
  Fetched 1 duniverse package(s) to duniverse/

Verify the package was placed in duniverse/:

  $ ls duniverse/
  mypkg.1.0.0
  $ ls duniverse/mypkg.1.0.0/
  dune
  dune-project
  mypkg.ml

Fetch again - should skip already-fetched package:

  $ dune pkg fetch
  Package mypkg.1.0.0 already fetched
  Fetched 1 duniverse package(s) to duniverse/

The duniverse directory should have the marker file:

  $ cat duniverse/.dune-duniverse
  # This directory is managed by dune pkg
