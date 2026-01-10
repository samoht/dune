Test that the "dune tools exec ocamllsp" command causes ocamllsp to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >   (ocaml (= 5.2.0))))
  > EOF

  $ dune build
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]

  $ dune tools exec ocamllsp
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  [1]

Make sure that after evaling the output of 'dune tools env', the first ocamllsp
executable in PATH is the one installed by dune as a dev tool.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled eval $(dune tools env)
  $ which ocamllsp
  /Users/samoht/.local/bin/ocamllsp
