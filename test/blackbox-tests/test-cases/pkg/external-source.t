Test that can fetch the sources from an external dir

  $ mkdir foo
  $ echo "y" > foo/x

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/foo))
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp x %{prefix}/bin/x)))
  > EOF

  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]

  $ show_pkg test
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  find: _build/.pkgs/default/test: No such file or directory
  
