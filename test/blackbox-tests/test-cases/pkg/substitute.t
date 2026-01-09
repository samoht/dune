The test-source folder has a file to use substitution on.

  $ mkdir test-source
  $ cat >test-source/foo.ml.in <<EOF
  > This file will be fed to the substitution mechanism
  > EOF
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute foo.ml.in foo.ml)
  >   (system "cat foo.ml")))
  > EOF

This should take the `foo.ml.in`, do the substitutions and create `foo.ml`:

  $ build_pkg test
  This file will be fed to the substitution mechanism

Demonstrate that the original sources aren't modified:

  $ src=_build/_private/default/.pkg/test/source/foo.ml; [ -e $src ] && cat $src
  [1]

This should also work with any other filename combination:

  $ cat >test-source/foo.ml.template <<EOF
  > This is using a different file suffix
  > EOF
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute foo.ml.template not-a-prefix)
  >   (system "cat not-a-prefix")))
  > EOF

This should take the `foo.ml.template`, do the substitution and create
`foo.ml`, thus be more flexible that the OPAM `substs` field:

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

Undefined variables, how do they substitute?

  $ cat >test-source/variables.ml.in <<EOF
  > We substitute this '%%{var}%%' into '%{var}%'
  > EOF
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute variables.ml.in variables.ml)
  >   (system "cat variables.ml")))
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

Now with variables set

  $ cat >test-source/defined.ml.in <<EOF
  > We substitute '%%{name}%%' into '%{name}%' and '%%{_:name}%%' into '%{_:name}%'
  > And '%%{version}%%' is set to '%{version}%'
  > There is also some paths set:
  > %%{lib}%% is '%{lib}%'
  > %%{libexec}%% is '%{libexec}%'
  > %%{bin}%% is '%{bin}%'
  > %%{sbin}%% is '%{sbin}%'
  > %%{toplevel}%% is '%{toplevel}%'
  > %%{share}%% is '%{share}%'
  > %%{etc}%% is '%{etc}%'
  > %%{doc}%% is '%{doc}%'
  > %%{stublibs}%% is '%{stublibs}%'
  > %%{man}%% is '%{man}%'
  > %%{with-test}%% is '%{with-test}%'
  > %%{os}%% is '%{os}%'
  > EOF
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (substitute defined.ml.in defined.ml)
  >   (system "cat defined.ml")))
  > EOF
  $ build_pkg test 2>&1 | dune_cmd subst '%\{os\}% is.*' '%{os}% is $REDACTED'
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

It is also possible to use variables of your dependencies:

  $ mkdir dependency-source
  $ make_lockpkg dependency <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/dependency-source))
  > EOF
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (depends dependency)
  > (build
  >  (progn
  >   (substitute dependencies.ml.in dependencies.ml)
  >   (system "cat dependencies.ml")))
  > EOF
  $ cat >test-source/dependencies.ml.in <<EOF
  > There is also some paths set:
  > '%%{dependency:lib}%%' is '%{dependency:lib}%'
  > EOF
  $ build_pkg test 2>&1 | sanitize_pkg_digest dependency.0.0.1
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

The substitute action should not observe the environment:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (depends dependency)
  > (build
  >  (progn
  >   (system "echo running")
  >   (substitute foo.in foo)))
  > EOF
  $ touch test-source/foo.in
  $ build_pkg test 2>&1
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
  $ build_pkg test 2>&1
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

Modifying this variable should not trigger a rebuild:

  $ export FOOBAR=1
  $ build_pkg test 2>&1
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
