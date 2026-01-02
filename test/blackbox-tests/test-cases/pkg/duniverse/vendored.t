Test that the duniverse directory is automatically treated as vendored when it
contains the marker file.

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends some-pkg))
  > EOF

  $ cat >dune <<EOF
  > (library (name myproj) (public_name myproj) (libraries some-pkg))
  > EOF

  $ cat >myproj.ml <<EOF
  > let x = Some_pkg.y
  > EOF

Create a duniverse directory with the marker file:

  $ mkdir -p duniverse
  $ touch duniverse/.dune-duniverse

Create a fake package in duniverse with a library:

  $ mkdir -p duniverse/some-pkg.1.0.0
  $ cat >duniverse/some-pkg.1.0.0/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name some-pkg))
  > EOF

  $ cat >duniverse/some-pkg.1.0.0/dune <<EOF
  > (library (name some_pkg) (public_name some-pkg))
  > EOF

  $ cat >duniverse/some-pkg.1.0.0/some_pkg.ml <<EOF
  > let y = 2
  > EOF

Build the project - duniverse should be auto-detected as vendored, and our
library should be able to use the duniverse library:

  $ dune build @install 2>&1 | head -10

Verify the library from duniverse was built:

  $ ls _build/default/duniverse/some-pkg.1.0.0/.some_pkg.objs/byte/*.cmo
  _build/default/duniverse/some-pkg.1.0.0/.some_pkg.objs/byte/some_pkg.cmo

And our main library uses it:

  $ ls _build/default/.myproj.objs/byte/*.cmo
  _build/default/.myproj.objs/byte/myproj.cmo
