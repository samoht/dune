Demonstrate that we copy empty directories

  $ make_lockdir

  $ src=_foo
  $ mkdir $src
  $ cd $src
  $ mkdir empty
  $ cd ..

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (system "find . | sort"))
  > (source (copy $PWD/$src))
  > EOF

  $ build_pkg foo
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  .
