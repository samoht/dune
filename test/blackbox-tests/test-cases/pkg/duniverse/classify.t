Test that packages are correctly classified as duniverse (dune-built) or opam sandbox
in the lock command output.

Set up a mock repository:

  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create a package that uses dune as its build system:

  $ mkpkg dune-pkg 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

Create a package that uses plain opam build commands:

  $ mkpkg opam-pkg 1.0.0 <<EOF
  > build: ["make" "all"]
  > install: ["make" "install"]
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

Create a project that depends on both packages:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends dune-pkg opam-pkg))
  > EOF

Lock the dependencies and verify classification in output:

  $ dune pkg lock --format=directory
  Solution for dune.lock (2 packages)
  dune:
  - dune-pkg.1.0.0
  
  opam:
  - opam-pkg.1.0.0


Verify the lockdir contents show dune packages use dune build command:

  $ cat dune.lock/dune-pkg.pkg
  (version 1.0.0)
  
  (build
   (run dune build -p %{pkg-self:name} -j %{jobs}))


  $ cat dune.lock/opam-pkg.pkg
  (version 1.0.0)
  
  (install
   (run make install))
  
  (build
   (run make all))


