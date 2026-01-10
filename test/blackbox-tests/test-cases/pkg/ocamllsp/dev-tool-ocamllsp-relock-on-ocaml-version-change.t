Test that if the version of the "ocaml" package in the project's
lockdir changes then the ocamllsp dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because ocamllsp must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0
  $ mk_ocaml 5.1.0

  $ setup_ocamllsp_workspace

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

Initially ocamllsp will depend on ocaml-base-compiler.5.2.0 to match the project.
  $ dune tools exec ocamllsp
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  grep: _build/.locks/tools-ocaml-lsp-server/ocaml-base-compiler.pkg: No such file or directory
  [2]


We can re-run "dune tools exec ocamllsp" without relocking or rebuilding.
  $ dune tools exec ocamllsp
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  [1]

Change the version of ocaml that the project depends on.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.1.0))))
  > EOF

  $ dune build
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.1.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.1.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.1.0 not found in any repository
  [1]


Running "dune tools exec ocamllsp" causes ocamllsp to be relocked and rebuilt
before running. Ocamllsp now depends on ocaml.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec ocamllsp
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.1.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.1.0 not found in any repository
  File "_build/.locks/default/dune.lock/lock", line 1, characters 0-0:
  Error: Package ocaml.5.1.0 not found in any repository
  [1]
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  grep: _build/.locks/tools-ocaml-lsp-server/ocaml-base-compiler.pkg: No such file or directory
  [2]
