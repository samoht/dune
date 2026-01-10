Test that the "dune ocaml doc" command causes odoc to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ mkrepo
  $ make_mock_odoc_package
  $ mk_ocaml 5.2.0
  $ setup_odoc_workspace

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
  Error: No rule found for .pkgs/ocaml-base-compiler/target/cookie
  -> required by loading the OCaml compiler for context "default"
  [1]

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: dune.lock/lock.dune: Not a directory
  [1]
