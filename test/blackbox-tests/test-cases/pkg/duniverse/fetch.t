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
  Solution for dune.lock:
  - dune-pkg.1.0.0

Check that fetch command shows message when duniverse packages have no sources:

  $ dune pkg fetch
  1 duniverse package(s) have no source URL (likely local packages).

The package has no URL in this test (mock packages don't have sources by
default), so it's treated as a local package. To test actual fetching,
we'd need to set up a server with archives.
