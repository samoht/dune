Test the special compiler version is picked up by ocamllsp.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ mkpkg ocaml-variants 5.2.0+ox << EOF
  > flags: compiler
  > conflict-class: "ocaml-core-compiler"
  > EOF

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))
  >    (ocaml-variants (= 5.2.0+ox))))
  > EOF

  $ dune build
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-variants.5.2.0+ox not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]

Here `ocamllsp` will pickup the compiler dependency on 5.2.0+ox
  $ dune tools exec ocamllsp
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml-variants.5.2.0+ox not found in any repository
  [1]

