Test that packages are correctly classified as duniverse (dune-built) or opam sandbox.

  $ make_lockdir

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > EOF

Create a package that uses dune (has "dune" build command):

  $ make_lockpkg dune-pkg <<EOF
  > (version 1.0.0)
  > (dune)
  > EOF

Create a package that uses opam build commands (action):

  $ make_lockpkg opam-pkg <<EOF
  > (version 1.0.0)
  > (build (run echo "building with opam"))
  > EOF

Verify the lockdir contents:

  $ cat dune.lock/dune-pkg.pkg
  (version 1.0.0)
  (dune)

  $ cat dune.lock/opam-pkg.pkg
  (version 1.0.0)
  (build (run echo "building with opam"))
