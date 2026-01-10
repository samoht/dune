
When we fetch a package source we should also fetch any submodules. Since we
will use the file protocol for git submodules we will need to explicitly enable
it as it is disabled for security purposes.

  $ export GIT_ALLOW_PROTOCOL=file

This repository will be a submodule in our main repository.
  $ mkdir someotherrepo
  $ cd someotherrepo
  $ git init --quiet
  $ echo "world" > bar
  $ git add bar
  $ git commit -am _ --quiet
  $ cd ..
  $ SOMEOTHERREPO=$PWD/someotherrepo

We create a repository for the package that we wish to build that has
someotherrepo as a submodule.
  $ mkdir somerepo
  $ cd somerepo
  $ git init --quiet
  $ echo "hello" > foo
  $ git submodule add --quiet $SOMEOTHERREPO mysubmodule
  $ git add foo mysubmodule .gitmodules
  $ git commit -am _ --quiet
  $ cd ..
  $ SOMEREPO=$PWD/somerepo

  $ mkdir foo && cd foo
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$SOMEREPO")))
  > (build (progn (run cat foo) (run cat mysubmodule/bar)))
  > EOF

Building this package should pull in both repositories:

  $ build_pkg test
  hello
  world

It should act exactly like:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$SOMEREPO")))
  > (build 
  >  (progn
  >   (run cp -r $SOMEOTHERREPO/bar mysubmodule)
  >   (run cat foo)
  >   (run cat mysubmodule/bar)))
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
  Error: No opam file found for vendored package test in duniverse/test.0.0.1
  -> required by - package test
  [1]

