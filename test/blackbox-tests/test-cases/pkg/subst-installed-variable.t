Test the %{pkg:installed}% form inside file substitution:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/source))
  > (build
  >  (progn
  >   (system "echo somepkg installation %{pkg:somepkg:installed}")
  >   (substitute foo.in foo)
  >   (system "cat foo")))
  > EOF
  $ mkdir source
  $ cat >source/foo.in <<EOF
  > foo: %{somepkg:installed}%
  > EOF

  $ build_pkg test
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  somepkg installation false
  foo: false
