Test that non-dune packages are also fetched to duniverse and can be edited.

Set up a mock repository:

  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create a package that uses a non-dune build system (e.g., make):

  $ mkpkg make-pkg 1.0.0 <<EOF
  > build: [make]
  > install: [make "install"]
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

Create a project that depends on the non-dune package:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends make-pkg))
  > EOF

Lock the dependencies:

  $ dune pkg lock
  Solution for dune.lock (1 package)
  opam:
  - make-pkg.1.0.0

Note: make-pkg is classified as "opam:" (non-dune package).

Now let's test with a package that has an actual source URL.
Create source files for a non-dune package:

  $ mkdir -p pkg-source
  $ cat >pkg-source/Makefile <<EOF
  > all:
  > 	@echo "Building make-pkg"
  > install:
  > 	@echo "Installing make-pkg"
  > 	mkdir -p \$(PREFIX)/lib/make-pkg
  > 	echo "make-pkg data" > \$(PREFIX)/lib/make-pkg/data.txt
  > EOF

Create a lock file with a source URL pointing to our local directory:

  $ make_lockdir
  $ make_lockpkg make-pkg <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (source (copy $PWD/pkg-source))
  > EOF

Fetch the package - non-dune packages should also be fetched to duniverse:

  $ dune pkg fetch
  Fetching make-pkg.1.0.0 to duniverse/make-pkg.1.0.0
  Fetched 1 package(s) to duniverse/

Verify the package was placed in duniverse/:

  $ ls duniverse/
  make-pkg.1.0.0
  $ ls duniverse/make-pkg.1.0.0/
  Makefile

The duniverse directory should have the marker file:

  $ cat duniverse/.dune-duniverse
  # This directory is managed by dune pkg

Now test that the non-dune package in duniverse can be built:

First, remove the original source directory to prove we're using duniverse:

  $ rm -rf pkg-source

Build the project - should use the duniverse source for the non-dune package:

  $ dune build @pkg-install 2>&1 | grep -E "^(Building|Installing)"
  Building make-pkg
  Installing make-pkg

Now test that edits to non-dune packages in duniverse are picked up:

  $ cat >duniverse/make-pkg.1.0.0/Makefile <<EOF
  > all:
  > 	@echo "Building EDITED make-pkg v2"
  > install:
  > 	@echo "Installing EDITED make-pkg v2"
  > 	mkdir -p \$(PREFIX)/lib/make-pkg
  > 	echo "EDITED make-pkg data" > \$(PREFIX)/lib/make-pkg/data.txt
  > EOF

Force rebuild by cleaning:

  $ dune clean
  $ dune build @pkg-install 2>&1 | grep -E "^(Building|Installing)"
  Building EDITED make-pkg v2
  Installing EDITED make-pkg v2

This confirms that non-dune packages fetched to duniverse:
1. Are built using the .pkg sandbox (existing behavior)
2. Use sources from duniverse/ instead of re-fetching
3. Edits to duniverse sources are picked up on rebuild
