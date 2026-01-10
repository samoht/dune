Test that ocamlformat is re-run when a source file changes.

  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Make a fake ocamlformat package that appends a comment to the end of its input.
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

Initial file:
  $ cat foo.ml
  let () = print_endline "Hello, world"

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.locks/tools-ocamlformat (1 package)
  opam:
  - ocamlformat.0.0.1
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]

After formatting the fake ocamlformat has added a suffix:
  $ cat foo.ml
  let () = print_endline "Hello, world"

Update the file:
  $ cat > foo.ml <<EOF
  > let () = print_endline "Hello, ocaml!"
  > EOF

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]

The update to the file persists after formatting it a second time:
  $ cat foo.ml
  let () = print_endline "Hello, ocaml!"
