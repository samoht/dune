Test that upgrading a package cleans up old files from the shared prefix.

  $ mkrepo

Create a package with version 0.0.1 that installs a file:

  $ mkdir -p foo-src
  $ cat > foo-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo) (allow_empty))
  > EOF
  $ cat > foo-src/dune <<EOF
  > (install
  >  (section lib)
  >  (files (old_file.txt as old_file.txt)))
  > EOF
  $ echo "old content" > foo-src/old_file.txt
  $ tar cf foo-0.0.1.tar foo-src
  $ rm -rf foo-src

  $ mkpkg foo 0.0.1 <<EOF
  > build: ["dune" "build" "-p" "foo"]
  > url {
  >   src: "file://$PWD/foo-0.0.1.tar"
  >   checksum: "md5=$(md5sum foo-0.0.1.tar | cut -f1 -d' ')"
  > }
  > EOF

Create a project that depends on foo:

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package (name test) (allow_empty) (depends foo))
  > EOF

  $ cat > dune <<EOF
  > EOF

Solve and build version 0.0.1:

  $ add_mock_repo_if_needed
  $ dune pkg lock 2>&1 | grep -v 'Solution\|Selected\|repositories'
  
  Dependencies common to all supported platforms:
  dune:
  - foo.0.0.1
  $ dune build @pkg-install 2>&1 | grep -v "^File"
  [1]

Check the old file is installed:

  $ cat _build/install/default/lib/foo/old_file.txt
  old content

Show the installed manifest file:

  $ cat _build/.pkgs/default/*/installed | sort
  lib/foo/META
  lib/foo/dune-package
  lib/foo/old_file.txt

Now create version 0.0.2 with a different file (old_file.txt is removed):

  $ mkdir -p foo-src
  $ cat > foo-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo) (allow_empty))
  > EOF
  $ cat > foo-src/dune <<EOF
  > (install
  >  (section lib)
  >  (files (new_file.txt as new_file.txt)))
  > EOF
  $ echo "new content" > foo-src/new_file.txt
  $ tar cf foo-0.0.2.tar foo-src
  $ rm -rf foo-src

  $ mkpkg foo 0.0.2 <<EOF
  > build: ["dune" "build" "-p" "foo"]
  > url {
  >   src: "file://$PWD/foo-0.0.2.tar"
  >   checksum: "md5=$(md5sum foo-0.0.2.tar | cut -f1 -d' ')"
  > }
  > EOF

Update and rebuild:

  $ dune pkg lock 2>&1 | grep -v 'Solution\|Selected\|repositories'
  
  Dependencies common to all supported platforms:
  dune:
  - foo.0.0.2
  $ dune build @pkg-install 2>&1 | grep -v "^File"
  [1]

The new file should exist:

  $ cat _build/install/default/lib/foo/new_file.txt
  new content

The old file should be cleaned up:

  $ test -f _build/install/default/lib/foo/old_file.txt && echo "old file still exists" || echo "old file cleaned up"
  old file still exists

The installed manifest should show the new file:

  $ cat _build/.pkgs/default/*/installed | sort
  lib/foo/META
  lib/foo/dune-package
  lib/foo/new_file.txt
