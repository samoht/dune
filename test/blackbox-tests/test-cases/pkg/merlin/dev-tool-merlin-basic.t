Test that the "dune tools exec ocamlmerlin" command causes merlin to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0

  $ setup_merlin_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ dune build
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]

  $ dune tools exec ocamlmerlin
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]
