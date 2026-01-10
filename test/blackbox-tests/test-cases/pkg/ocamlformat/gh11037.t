Exercise differences between the behavior of `dune fmt` when a lockdir is
present and a lockdir is absent.

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

The foo package depends on the bar package.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF

The foo executable depends on the bar library.
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo)
  >  (libraries bar))
  > EOF

Without a .ocamlformat `dune fmt` does nothing.
  $ touch .ocamlformat

Run `dune fmt` before creating a lockdir, and print the file foo.ml before and
after to demonstrate that it was formatted. Note that the package "bar" hasn't
yet been defined, so the fact that `dune fmt` works indicates that dune did not
attempt to build the package "foo".
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
  $ cat foo.ml
  let () = print_endline "Hello, world"

Create a lockdir and define the package "bar". Note its install command is
`false` so it will fail to install.
  $ make_lockdir
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (install (run false))
  > EOF

Now run `dune fmt` again. It attempts to build the project and its
dependencies, and fails to install the dependency "bar".
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
