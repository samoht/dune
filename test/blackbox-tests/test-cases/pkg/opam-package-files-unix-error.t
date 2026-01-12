This test demonstrates the behaviour when a Unix error is encountered when copying the
files/ directory from a package directory inside an opam repository.

  $ mkrepo

Make a package with a patch
  $ mkpkg with-patch <<EOF
  > EOF

  $ fname1="foo.patch"
  $ fname2="dir/bar.patch"
  $ opam_repo="$mock_packages/with-patch/with-patch.0.0.1"
  $ mkdir -p $opam_repo/files/dir
  $ cat >$opam_repo/files/$fname1 <<EOF
  > foo
  > EOF
  $ cat >$opam_repo/files/$fname2 <<EOF
  > bar
  > EOF
We remove the read permissions for dir/ making sure to add them back if we exit
the test.

  $ trap "chmod +r $opam_repo/files/dir" EXIT
  $ chmod -r $opam_repo/files/dir

The error message should have a location for the opam repository.

This does not currently seem to be the case.

  $ cat >dune<<EOF
  > (dirs * \ mock-opam-repository)
  > EOF

  $ solve with-patch
  Solution for dune.lock (1 package):
  opam:
  - with-patch.0.0.1

