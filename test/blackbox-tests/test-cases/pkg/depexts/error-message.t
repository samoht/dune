When a package fails to build, dune will print opam depexts warning.

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

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF
  $ cat > bar.ml <<EOF
  > let () = print_endline "hello"
  > EOF

Solve to create a proper lock directory with dependency hash:
  $ solve --format=directory foo 2>&1 | head -3
  Solution for dune.lock (1 package):
  dune:
  - foo.0.0.1

Build the project, when it fails building 'foo' package, it shows the depexts
error message.
  $ dune build
  File "dune.lock/foo.0.0.1.pkg", line 3, characters 6-10:
  3 |  (run dune build))
            ^^^^
  Error: Logs for package foo
  File "dune-project", line 1, characters 0-0:
  Error: Invalid first line, expected: (lang <lang> <version>)

  Hint: Missing system dependencies: gnupg, unzip
  To install:
    brew install gnupg unzip
  [1]
