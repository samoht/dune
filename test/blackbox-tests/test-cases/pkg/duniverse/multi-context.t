Test duniverse packages with multiple build contexts in dune-workspace.

This test verifies that duniverse packages work correctly when building
with multiple OCaml contexts (simulating multiple OCaml versions).

Set up a project with multiple contexts:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj))
  > EOF

Create a dune-workspace with multiple contexts:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (context (default))
  > (context (default (name alt)))
  > EOF

  $ cat >dune <<EOF
  > (library (name myproj) (public_name myproj) (libraries dune-lib))
  > EOF

  $ cat >myproj.ml <<EOF
  > let x = Dune_lib.version
  > EOF

Create duniverse with a package source:

  $ mkdir -p duniverse
  $ touch duniverse/.dune-duniverse

  $ mkdir -p duniverse/dune-lib.1.0.0
  $ cat >duniverse/dune-lib.1.0.0/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-lib))
  > EOF

  $ cat >duniverse/dune-lib.1.0.0/dune <<EOF
  > (library (name dune_lib) (public_name dune-lib))
  > EOF

  $ cat >duniverse/dune-lib.1.0.0/dune_lib.ml <<EOF
  > let version = "1.0.0"
  > EOF

Build in both contexts - duniverse should be built in both:

  $ dune build @install 2>&1 | head -20

The library should be built in both contexts:

  $ ls _build/default/.myproj.objs/byte/*.cmo
  _build/default/.myproj.objs/byte/myproj.cmo
  $ ls _build/alt/.myproj.objs/byte/*.cmo
  _build/alt/.myproj.objs/byte/myproj.cmo

The duniverse library should also be built in both contexts:

  $ ls _build/default/duniverse/dune-lib.1.0.0/.dune_lib.objs/byte/*.cmo
  _build/default/duniverse/dune-lib.1.0.0/.dune_lib.objs/byte/dune_lib.cmo
  $ ls _build/alt/duniverse/dune-lib.1.0.0/.dune_lib.objs/byte/*.cmo
  _build/alt/duniverse/dune-lib.1.0.0/.dune_lib.objs/byte/dune_lib.cmo
