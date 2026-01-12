Test fetching from git

  $ mkdir somerepo
  $ cd somerepo
  $ git init --quiet
  $ echo "hello world" > foo
  $ git add foo
  $ git commit -am _ --quiet
  $ cd ..

  $ MYGITREPO=$PWD/somerepo

  $ mkdir foo && cd foo
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$MYGITREPO")))
  > (build (run cat foo))
  > EOF

  $ build_pkg test
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  hello world
