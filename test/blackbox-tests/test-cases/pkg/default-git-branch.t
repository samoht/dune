Modify the default branch of a source repo.

Once the branch is modified, dune should rebuild the package.

This bug is reported in #10063

  $ src="_git_source"

  $ mkdir $src && cd $src
  $ git init --quiet
  $ git checkout -b "branch1"
  Switched to a new branch 'branch1'
  $ echo "branch 1" > file
  $ git add -A
  $ git commit --quiet -m "branch 1"
  $ git checkout -b branch2
  Switched to a new branch 'branch2'
  $ echo "branch 2" > file
  $ git add -A
  $ git commit --quiet -m "branch 2"
  $ cd ..

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (source (fetch (url "git+file://$PWD/$src")))
  > (version 0.0.1)
  > (build (run cat file))
  > EOF

Build the package

  $ build_pkg foo
     Vendoring foo.0.0.1
  branch 2

Change the default branch

  $ git -C $src checkout branch1
  Switched to branch 'branch1'

And now rebuild

  $ build_pkg foo
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
  Error: No opam file found for vendored package foo in duniverse/foo.0.0.1
  -> required by - package foo
  [1]
