Test duniverse classification with mixed dune/non-dune packages at various
depths in the dependency chain.

This test verifies that the lock command correctly classifies packages:
- Dune packages are listed under "duniverse (dune-built)"
- Non-dune packages are listed under "opam sandbox"

Set up a mock repository:

  $ mkrepo

Create a chain of packages with mixed build systems:
- dune-leaf: uses dune, no dependencies
- make-middle: uses make, depends on dune-leaf
- dune-top: uses dune, depends on make-middle

This tests that dune packages are still classified correctly even when
they depend on non-dune packages.

  $ mkpkg dune-leaf 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg make-middle 1.0.0 <<EOF
  > depends: ["dune-leaf"]
  > build: [make]
  > EOF

  $ mkpkg dune-top 1.0.0 <<EOF
  > depends: ["make-middle"]
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

Create a project that depends on the top-level dune package:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends dune-top))
  > EOF

Lock the dependencies - packages should be classified correctly:

  $ dune pkg lock 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (3 packages)
  
  dune:
  - dune-leaf.1.0.0
  - dune-top.1.0.0
  
  opam:
  - make-middle.1.0.0




Now test a diamond dependency pattern where both paths have mixed packages:
- app depends on dune-a and make-b
- dune-a depends on shared-dune
- make-b depends on shared-dune

  $ mkpkg shared-dune 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg dune-a 1.0.0 <<EOF
  > depends: ["shared-dune"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg make-b 1.0.0 <<EOF
  > depends: ["shared-dune"]
  > build: [make]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name app) (depends dune-a make-b))
  > EOF

  $ rm -rf dune.lock
  $ dune pkg lock 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (3 packages)
  
  dune:
  - dune-a.1.0.0
  - shared-dune.1.0.0
  
  opam:
  - make-b.1.0.0


