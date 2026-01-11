Test that non-local (opam) packages can depend on local (workspace) packages.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg remote <<EOF
  > depends: [
  >  "local_b"
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name local_a)
  >  (depends remote))
  > (package
  >  (name local_b))
  > EOF

The lock should succeed - remote can depend on the workspace package local_b.
The dependency on local_b will be satisfied by the workspace, not by opam.

  $ dune pkg lock
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - remote.0.0.1

