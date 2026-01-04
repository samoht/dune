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

Lock the project:
  $ dune pkg lock 2>&1 | head -6
  Solution for dune.lock (2 packages)
  
  Dependencies common to all supported platforms:
  dune:
  - dune-pkg.0.0.1
  


Note: The output shows "dune:" prefix for dune packages and "opam:" for non-dune packages.

Test fetch command - reports no packages to fetch since mock packages have no source URLs:
  $ dune pkg fetch 2>&1
  1 dune package(s) have no source URL (likely local packages).

This is expected since our mock packages don't have URLs defined.
