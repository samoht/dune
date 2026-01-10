Things should be the same whether dependencies are specified or not.

  $ make_lockdir

If we have a package we depend on

  $ mkdir dependency-source
  $ make_lockpkg dependency <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/dependency-source))
  > EOF

And we have a package we want to build

  $ mkdir test-source
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

It should continue to work even if `dependency` modifies `PATH`:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > ; adding deps breaks cat
  > (depends dependency)
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  Error: No opam file found for vendored package dependency in
  duniverse/dependency.0.0.1
  -> required by - package dependency
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  -> required by _build/default/.dune/configurator
  Error: No opam file found for vendored package test in duniverse/test.0.0.1
  -> required by - package test
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  -> required by _build/default/.dune/configurator
  Error: Vendor directory duniverse/dependency.0.0.1 has (mode opam) but no
  opam file found. Try running 'dune pkg fetch' to generate opam files.
  Error: Vendor directory duniverse/test.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  [1]
