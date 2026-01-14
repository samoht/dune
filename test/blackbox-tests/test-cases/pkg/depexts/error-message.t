When a package fails to build, dune will print opam depexts warning.

Disable auto_fetch to test sandboxed build error messages with depexts:
  $ export DUNE_CONFIG__AUTO_FETCH=disabled
  $ export DUNE_PKG_PLATFORM=brew
  $ mkrepo
  $ add_mock_repo_if_needed

Make a library that would fail when building it (empty dune-project):
  $ mkdir foo-src
  $ cat > foo-src/dune-project <<EOF
  > EOF
  $ tar cf foo.tar foo-src
  $ rm -rf foo-src

Create an opam package with depexts and a build command that will fail.
The source URL points to our tarball. The build runs "dune build" which will
fail because the source has an invalid dune-project.

  $ mkpkg foo <<EOF
  > build: ["dune" "build"]
  > depexts: ["unzip" "gnupg"]
  > url {
  >   src: "file://$PWD/foo.tar"
  >   checksum: "md5=$(md5sum foo.tar | cut -f1 -d' ')"
  > }
  > EOF

Make a project that depends on the foo package:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

Solve to create a proper lock directory with dependency hash:
  $ solve --format=directory foo 2>&1 | head -3
  Solution for dune.lock (1 package):
  dune:
  - foo.0.0.1

Build the packages, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build @pkg-install
  File "_build/.locks/default/dune.lock/foo.0.0.1.pkg", line 12, characters 3-140:
  12 |    file:///Users/samoht/git/dune/_build/.sandbox/e7317dfd5522e11628e69ccdbb2a872b/default/test/blackbox-tests/test-cases/pkg/depexts/foo.tar)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package source not cached and network access is disabled
  (--fetch=disabled).
  Run 'dune pkg fetch' first to download package sources, or use
  --fetch=enabled.
  [1]

