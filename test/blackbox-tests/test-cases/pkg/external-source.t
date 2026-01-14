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
     Vendoring test.0.0.1

  $ show_pkg test
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  
  /installed
  /source
  /source/x
  /target
  /target/bin
  /target/cookie
  /target/doc
  /target/doc/test
  /target/etc
  /target/etc/test
  /target/lib
  /target/lib/stublibs
  /target/lib/test
  /target/lib/toplevel
  /target/man
  /target/sbin
  /target/share
  /target/share/test
