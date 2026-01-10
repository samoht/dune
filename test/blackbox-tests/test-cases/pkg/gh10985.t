This test attempts to build the ocaml index while depending on a library
installed through a lock file.

We set up a library that will be installed as part of the package:

  $ mkdir external_sources
  $ cat >external_sources/dune-project <<EOF
  > (lang dune 3.11)
  > (package (name mypkg))
  > EOF
  $ cat >external_sources/dune <<EOF
  > (library
  >  (public_name mypkg.lib)
  >  (name test_lib))
  > EOF
  $ cat >external_sources/test_lib.ml <<EOF
  > let x = ()
  > EOF

We put the actual build in a separate directory, so we don't have to ignore
the package directory in the dune file:
  $ mkdir actual
  $ cd actual

Now we set up a lock file with this package and then attempt to use it:

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/../external_sources))
  > (build (run dune build --release --promote-install-file=true . @install))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (libraries mypkg.lib))
  > EOF

  $ cat >foo.ml <<EOF
  > let () = Test_lib.x
  > EOF

  $ mkdir .bin
  $ cat > .bin/ocaml-index <<EOF
  > #!/usr/bin/env sh
  > exit 1
  > EOF

  $ chmod +x .bin/ocaml-index
  $ export PATH="$PWD/.bin:$PATH"

  $ dune build --fetch=disabled @ocaml-index
  File "dune", line 3, characters 12-21:
  3 |  (libraries mypkg.lib))
                  ^^^^^^^^^
  Error: Library "mypkg.lib" not found.
  -> required by library "foo" in _build/default
  -> required by _build/default/.foo.objs/byte/foo.cmt
  -> required by _build/default/.foo.objs/cctx.ocaml-index
  -> required by alias ocaml-index
  [1]
