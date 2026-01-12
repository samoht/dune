Test the duniverse feature - classifying and fetching packages based on build system

  $ mkrepo
  $ add_mock_repo_if_needed

Create a package that uses dune as build system:
  $ mkpkg dune-pkg << 'EOF'
  > build: ["dune" "build" "-p" name "-j" jobs]
  > install: ["dune" "install" "-p" name]
  > EOF

Create a package that uses make (non-dune):
  $ mkpkg make-pkg << 'EOF'
  > build: ["make"]
  > install: ["make" "install"]
  > EOF

Create a project that depends on both:
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name myproject)
  >  (depends dune-pkg make-pkg))
  > EOF

Lock the project (using directory format for mock repo compatibility):
  $ dune pkg lock --format=directory 2>&1 | head -6
  Solution for dune.lock (2 packages)
  
  Dependencies common to all supported platforms:
  dune:
  - dune-pkg.0.0.1
  



Note: The output shows "dune:" prefix for dune packages and "opam:" for non-dune packages.

Test fetch command - reports no packages to fetch since mock packages have no source URLs:
  $ dune pkg fetch 2>&1

This is expected since our mock packages don't have URLs defined.

Now test with packages that have actual source URLs:

First create a simple dune library tarball:
  $ mkdir mylib
  $ cat > mylib/dune-project << EOF
  > (lang dune 3.13)
  > (name mylib)
  > EOF
  $ cat > mylib/dune << EOF
  > (library (public_name mylib) (name mylib))
  > EOF
  $ cat > mylib/mylib.ml << EOF
  > let greeting = "Hello from mylib"
  > EOF
  $ tar cf mylib.tar mylib
  $ MYLIB_MD5=$(md5sum mylib.tar | cut -f1 -d' ')
  $ rm -rf mylib

Create a make-based package tarball:
  $ mkdir makelib
  $ cat > makelib/Makefile << EOF
  > all:
  > 	@echo "building makelib"
  > install:
  > 	@echo "installing makelib"
  > EOF
  $ tar cf makelib.tar makelib
  $ MAKELIB_MD5=$(md5sum makelib.tar | cut -f1 -d' ')
  $ rm -rf makelib

Set up mock HTTP server to serve the tarballs:
  $ echo mylib.tar > fake-curls
  $ MYLIB_PORT=1
  $ echo makelib.tar >> fake-curls
  $ MAKELIB_PORT=2

Remove old lock and packages:
  $ rm -rf dune.lock

Create new packages with URLs:
  $ mkpkg mylib 1.0.0 << EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > install: ["dune" "install" "-p" name]
  > url {
  >   src: "http://localhost:$MYLIB_PORT"
  >   checksum: "md5=$MYLIB_MD5"
  > }
  > EOF

  $ mkpkg makelib 1.0.0 << EOF
  > build: ["make"]
  > install: ["make" "install"]
  > url {
  >   src: "http://localhost:$MAKELIB_PORT"
  >   checksum: "md5=$MAKELIB_MD5"
  > }
  > EOF

Create a project that depends on both:
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name myproject)
  >  (depends mylib makelib))
  > EOF

Lock the project - should show mylib as "dune:" and makelib as "opam:":
  $ dune_pkg_lock_normalized
  Solution for dune.lock (2 packages):
  dune:
  - mylib.1.0.0
  
  opam:
  - makelib.1.0.0

Fetch duniverse packages - should fetch only mylib (the dune package):
  $ dune pkg fetch 2>&1

Verify the duniverse directory structure:
  $ find duniverse -type f | sort
  duniverse/.gitignore
  duniverse/dune
  duniverse/makelib.1.0.0/.dune-source-url
  duniverse/makelib.1.0.0/Makefile
  duniverse/mylib.1.0.0/.dune-source-url
  duniverse/mylib.1.0.0/dune
  duniverse/mylib.1.0.0/dune-project
  duniverse/mylib.1.0.0/mylib.ml

Verify the dune file marks packages as vendored:
  $ cat duniverse/dune
  ; This directory is managed by dune pkg
  (vendored_dirs *)
  (vendor mylib.1.0.0 (libraries mylib))
  (vendor makelib.1.0.0 (mode opam))

Verify the library code was fetched:
  $ cat duniverse/mylib.1.0.0/mylib.ml
  let greeting = "Hello from mylib"

Running fetch again should skip already-fetched packages:
  $ dune pkg fetch 2>&1
