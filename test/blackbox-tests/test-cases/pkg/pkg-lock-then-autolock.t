Test that explicit locking followed by auto-locking produces equivalent results
and does not trigger unnecessary rebuilds.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a library package:

  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let message = "Hello from foo 0.0.1!"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo.tar foo
  $ rm -rf foo

Make a package for the library using a local file source:

  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "build" "-p" name "@install"]
  > ]
  > url {
  >  src: "$PWD/foo.tar"
  > }
  > EOF

Create a project that depends on foo:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ cat > bar.ml <<EOF
  > let () = print_endline Foo.message
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name bar)
  >  (libraries foo))
  > EOF

Lock and build with explicit dune pkg lock:
  $ dune pkg lock --format=directory
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  dune:
  - foo.0.0.1

  $ dune exec --display short bar 2>&1 | grep "Building"
  [1]

  $ dune exec bar
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.bar.eobjs/native/dune__exe__Bar.cmx
  -> required by _build/default/bar.exe
  -> required by _build/install/default/bin/bar
  [1]

Remove the lock directory:
  $ rm -rf dune.lock

Enable auto-locking:
  $ enable_pkg

Build again - should auto-lock internally and NOT rebuild foo:
  $ dune exec --display short bar 2>&1 | grep "Building" || echo "no rebuilds"
  no rebuilds

  $ dune exec bar
  Error: dune.lock/lock.dune: Not a directory
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by creating installed environment for "default"
  [1]
