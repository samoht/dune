Demonstrate that we should support tarballs with and without a root directory

  $ mkdir _source/
  $ touch _source/foo

  $ tar -czf tarball1.tar.gz -C _source foo
  $ tar -czf tarball2.tar.gz _source/foo

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ make_lockdir

  $ echo tarball1.tar.gz > fake-curls
  $ echo tarball2.tar.gz >> fake-curls

  $ runtest() {
  > make_lockpkg foo <<EOF
  > (version 0.1.0)
  > (source (fetch (url http://0.0.0.0:$1)))
  > (build (run sh -c "find . | sort"))
  > EOF
  > build_pkg foo
  > rm -rf _build
  > }
  $ runtest 1
  Error: Don't know how to build _build/.pkgs/default/foo/installed
  $ runtest 2
  Error: No opam file found for vendored package foo in duniverse/foo.0.1.0
  -> required by - package foo
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  -> required by _build/default/.dune/configurator
  Error: Vendor directory duniverse/foo.0.1.0 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
