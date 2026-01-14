Demonstrate the handling of findlib directories that don't exist

Reproduces #11405

  $ mkdir external_sources

  $ cat >external_sources/META <<EOF
  > package "yes" (
  >   directory = "yes"
  >   version = "0.0.1"
  >   exists_if = "yes.cma"
  > )
  > package "no" (
  >   directory = "no"
  >   version = "0.0.1"
  >   exists_if = "no.cma"
  > )
  > EOF

  $ cat >external_sources/mypkg.install <<EOF
  > lib: [
  >  "META"
  >  "yes/yes.cma" {"yes/yes.cma"}
  > ]
  > EOF

  $ mkdir external_sources/yes
  $ touch external_sources/yes/yes.cma

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ make_lockdir

  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > EOF

  $ touch foo.ml

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.yes)
  >  (name foo))
  > EOF

No errors here as 'yes' actually exists
  $ dune build foo.exe
     Vendoring mypkg.0.0.1
  File "dune", line 2, characters 12-21:
  2 |  (libraries mypkg.yes)
                  ^^^^^^^^^
  Error: Library "mypkg.yes" not found.
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe
  [1]

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.no)
  >  (name foo))
  > EOF

Clearer error here as we really depend on non-existing 'no'
  $ dune build foo.exe 2>&1 | sanitize_pkg_digest mypkg.0.0.1
  Error: No opam file found for vendored package mypkg in duniverse/mypkg.0.0.1
  -> required by - package mypkg
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  -> required by _build/default/.dune/configurator
  Error: Vendor directory duniverse/mypkg.0.0.1 has (mode opam) but no opam
  file found. Try running 'dune pkg fetch' to generate opam files.
