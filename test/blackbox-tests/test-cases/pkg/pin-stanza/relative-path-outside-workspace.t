Demonstrate that you can't use a relative path referring outside the workspace
in the pin stanza:

Make a package containing a library:
  $ mkdir foo
  $ cat > foo/dune-workspace <<EOF
  > (lang dune 3.14)
  > EOF
  $ cat > foo/dune-project <<EOF
  > (lang dune 3.14)
  > (package
  >  (name foo))
  > EOF
  $ cat > foo/dune <<EOF
  > (library
  >  (public_name foo))
  > EOF
  $ cat > foo/foo.ml <<EOF
  > let foo = "foo"
  > EOF

Make a second package depending on the first via a pin:
  $ mkdir bar
  $ cd bar
  $ mkrepo
  $ add_mock_repo_if_needed
  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (pin
  >  (url file://$PWD/../foo)
  >  (package (name foo)))
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
  > let () = print_endline Foo.foo
  > EOF

Lock and build the second package to demonstrate that everything works so far:
  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  dune:
  - foo.dev
  $ dune exec ./bar.exe
     Vendoring foo.dev
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.bar.eobjs/native/dune__exe__Bar.cmx
  -> required by _build/default/bar.exe
  [1]

Now change the pin to use a relative path:
  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (pin
  >  (url file://../foo)
  >  (package (name foo)))
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Solving the project now results in an error, though it's still possible to build the project:
  $ dune clean
  $ dune_pkg_lock_normalized
  Error: path outside the workspace: ../foo from .
  [1]
  $ dune exec ./bar.exe
  foo
