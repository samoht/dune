Test that if the version of the "ocaml" package in the project's
lockdir changes then the odoc dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because odoc must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ mkrepo
  $ make_mock_odoc_package
  $ mk_ocaml 5.2.0
  $ mk_ocaml 5.1.0

  $ setup_odoc_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >  (ocaml (= 5.2.0))))
  > EOF

  $ dune build

Initially odoc will depend on ocaml-base-compiler.5.2.0 to match the project.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: dune.lock/lock.dune: Not a directory
  [1]
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  grep: _build/.locks/tools-odoc/ocaml-base-compiler.pkg: No such file or directory
  [2]

We can re-run "dune ocaml doc" without relocking or rebuilding.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: dune.lock/lock.dune: Not a directory
  [1]

Change the version of ocaml that the project depends on.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >  (ocaml (= 5.1.0))))
  > EOF

  $ dune build
  Error: dune.lock/lock.dune: Not a directory
  [1]

Running "dune ocaml doc" causes odoc to be relocked and rebuilt
before running. Odoc now depends on ocaml.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Error: dune.lock/lock.dune: Not a directory
  [1]
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  grep: _build/.locks/tools-odoc/ocaml-base-compiler.pkg: No such file or directory
  [2]
