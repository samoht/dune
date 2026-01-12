Test that the ocamllsp dev tool executes in an environment where other dev
tools are in PATH.

  $ mkrepo
  $ mk_ocaml 5.2.0
  $ setup_ocamllsp_workspace

Make a fake ocamllsp package that prints out the PATH variable:
  $ mkpkg ocaml-lsp-server <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "echo 'echo \$PATH' >> %{bin}%/ocamllsp" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamllsp" ]
  > ]
  > EOF

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

Confirm that each dev tool's bin directory is now in PATH:
  $ dune tools exec ocamllsp | tr : '\n' | grep '_build/_private/default/.dev-tool'
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml.5.2.0 not found in any repository
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml-base-compiler.5.2.0 not found in any repository
  File "dune.lock", line 1, characters 0-0:
  Error: Package ocaml-compiler.5.2.0 not found in any repository
  [1]
