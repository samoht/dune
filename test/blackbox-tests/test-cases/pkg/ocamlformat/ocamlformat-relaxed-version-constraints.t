Check that dune can choose a version of ocamlformat with a suffix (e.g.
0.24+foo) to satisfy a .ocamlformat config that specifies a matching version
without the suffix.

  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Fake ocamlformat package that appends a comment with the ocamlformat version to the end of the file:
  $ ocamlformat_package() {
  >   cat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat %{version}% *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF
  > }

Make some fake ocamlformat packages:
  $ ocamlformat_package | mkpkg ocamlformat 0.24+foo
  $ ocamlformat_package | mkpkg ocamlformat 0.25+bar

Initial file:
  $ cat foo.ml
  let () = print_endline "Hello, world"

This should choose the 0.24+foo version:
  $ echo "version=0.24" > .ocamlformat
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.locks/tools-ocamlformat (1 package)
  opam:
  - ocamlformat.0.24+foo
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"

This should choose the 0.24+bar version:
  $ echo "version=0.25" > .ocamlformat
  $ rm -r "${dev_tool_lock_dir}"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.locks/tools-ocamlformat (1 package)
  opam:
  - ocamlformat.0.25+bar
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"

This should fail as there is no version matching 0.24.1:
  $ echo "version=0.24.1" > .ocamlformat
  $ rm -r "${dev_tool_lock_dir}"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory _build/.locks/tools-ocamlformat:
  Couldn't solve the package dependency formula.
  Selected candidates: ocamlformat_dev_tool_wrapper.dev
  - ocamlformat -> (no usable version)
      Rejected by depends of local package ocamlformat_dev_tool_wrapper
      (constraint: >= 0.24.1 & <= 0.24.1___MAX_VERSION) [0.25+bar, 0.24+foo]
  [1]
