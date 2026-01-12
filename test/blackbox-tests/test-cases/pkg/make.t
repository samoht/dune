Build a package with make

  $ mkdir foo
  $ cat >foo/Makefile <<EOF
  > .DEFAULT: foo
  > .PHONY: foo
  > foo: ; @echo running makefile
  > EOF
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (run %{make}))
  > (source (copy $PWD/foo))
  > EOF
  $ build_pkg foo
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  running makefile
