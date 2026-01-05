Test that post dependencies on virtual packages (like base-bigarray) are
properly included in the solution and can be resolved at build time.

This mimics the relocatable-compiler package which has post dependencies on
base-* packages that may not be regular dependencies of any other package.

  $ mkrepo

  $ cat >dune-workspace << EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os macos)))
  > EOF

Create virtual packages (no source, just opam metadata):
  $ mkpkg base-bigarray << EOF
  > flags: [compiler]
  > EOF

  $ mkpkg base-unix << EOF
  > flags: [compiler]
  > EOF

  $ mkpkg base-threads << EOF
  > flags: [compiler]
  > EOF

Create a package that depends on base-* packages as post dependencies.
This mimics relocatable-compiler which has these as post deps because
they're provided by the compiler but need to be "installed" together.
  $ mkpkg relocatable-compiler << EOF
  > depends: [
  >   "base-unix" {post}
  >   "base-bigarray" {post}
  >   "base-threads" {post}
  > ]
  > EOF

Create a package that depends on relocatable-compiler:
  $ mkpkg myapp << EOF
  > depends: [
  >   "relocatable-compiler"
  > ]
  > EOF

The solution should include all base-* packages even though they're only
post dependencies:
  $ solve myapp
  Solution for dune.lock (5 packages):
  opam:
  - base-bigarray.0.0.1
  - base-threads.0.0.1
  - base-unix.0.0.1
  - myapp.0.0.1
  - relocatable-compiler.0.0.1

Ensure that packages can be resolved at build time:
  $ dune build
