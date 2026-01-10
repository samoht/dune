Test that upgrading a package cleans up old files from the shared prefix.

  $ . ./helpers.sh
  /var/folders/7g/4yr0hhnx5ml4kvvxfjszdqf00000gn/T/dune_cram_978e3b_.cram.sh/1.sh: line 1: ./helpers.sh: No such file or directory
  ***** UNREACHABLE *****
  $ mkrepo
  ***** UNREACHABLE *****

Create a package with version 0.0.1 that installs a file:

  $ mkdir -p foo-src
  ***** UNREACHABLE *****
  $ cat > foo-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo) (allow_empty))
  > EOF
  ***** UNREACHABLE *****
  $ cat > foo-src/dune <<EOF
  > (install
  >  (section lib)
  >  (files (old_file.txt as old_file.txt)))
  > EOF
  ***** UNREACHABLE *****
  $ echo "old content" > foo-src/old_file.txt
  ***** UNREACHABLE *****
  $ tar cf foo-0.0.1.tar foo-src
  ***** UNREACHABLE *****
  $ rm -rf foo-src
  ***** UNREACHABLE *****

  $ mkpkg foo 0.0.1 <<EOF
  > build: ["dune" "build" "-p" "foo"]
  > url {
  >   src: "file://$PWD/foo-0.0.1.tar"
  >   checksum: "md5=$(md5sum foo-0.0.1.tar | cut -f1 -d' ')"
  > }
  > EOF
  ***** UNREACHABLE *****

Create a project that depends on foo:

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF
  ***** UNREACHABLE *****

  $ cat > dune <<EOF
  > EOF
  ***** UNREACHABLE *****

Solve and build version 0.0.1:

  $ add_mock_repo_if_needed
  ***** UNREACHABLE *****
  $ dune pkg lock 2>&1 | head -5
  ***** UNREACHABLE *****

  $ dune build @pkg-install 2>&1 | grep -v "^File"
  ***** UNREACHABLE *****

Check the old file is installed:

  $ cat _build/install/default/lib/foo/old_file.txt
  ***** UNREACHABLE *****

Now create version 0.0.2 with a different file (old_file.txt is removed):

  $ mkdir -p foo-src
  ***** UNREACHABLE *****
  $ cat > foo-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo) (allow_empty))
  > EOF
  ***** UNREACHABLE *****
  $ cat > foo-src/dune <<EOF
  > (install
  >  (section lib)
  >  (files (new_file.txt as new_file.txt)))
  > EOF
  ***** UNREACHABLE *****
  $ echo "new content" > foo-src/new_file.txt
  ***** UNREACHABLE *****
  $ tar cf foo-0.0.2.tar foo-src
  ***** UNREACHABLE *****
  $ rm -rf foo-src
  ***** UNREACHABLE *****

  $ mkpkg foo 0.0.2 <<EOF
  > build: ["dune" "build" "-p" "foo"]
  > url {
  >   src: "file://$PWD/foo-0.0.2.tar"
  >   checksum: "md5=$(md5sum foo-0.0.2.tar | cut -f1 -d' ')"
  > }
  > EOF
  ***** UNREACHABLE *****

Update and rebuild:

  $ dune pkg lock 2>&1 | head -5
  ***** UNREACHABLE *****

  $ dune build @pkg-install 2>&1 | grep -v "^File"
  ***** UNREACHABLE *****

The new file should exist:

  $ cat _build/install/default/lib/foo/new_file.txt
  ***** UNREACHABLE *****

The old file should be cleaned up:

  $ test -f _build/install/default/lib/foo/old_file.txt && echo "old file still exists" || echo "old file cleaned up"
  ***** UNREACHABLE *****
