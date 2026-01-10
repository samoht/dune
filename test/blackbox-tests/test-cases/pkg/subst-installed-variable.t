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
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
