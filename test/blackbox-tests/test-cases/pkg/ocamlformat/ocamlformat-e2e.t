Exercises end to end, locking and building ocamlformat dev tool.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_fake_ocamlformat "0.26.3"

Add the tar file for the fake curl to copy it:
  $ echo ocamlformat-0.26.2.tar > fake-curls
  $ PORT=1

  $ make_ocamlformat_opam_pkg "0.26.2" $PORT

Add the tar file for the fake curl to copy it:
  $ echo ocamlformat-0.26.3.tar >> fake-curls
  $ PORT=2

We consider this version of OCamlFormat as the latest version:
  $ make_ocamlformat_opam_pkg "0.26.3" $PORT

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Without a ".ocamlformat" file, "dune fmt" takes the latest version of
OCamlFormat.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.3
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat _build/default/.formatted/foo.ml
  cat: _build/default/.formatted/foo.ml: No such file or directory
  [1]

Create .ocamlformat file
  $ cat > .ocamlformat <<EOF
  > version = 0.26.2
  > EOF

An important cleaning here, "dune fmt" will relock and build the new version(0.26.2) of OCamlFormat.
  $ rm -r "${dev_tool_lock_dir}"
  $ dune clean

With a ".ocamlformat" file, "dune fmt" takes the version mentioned inside ".ocamlformat"
file.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat _build/default/.formatted/foo.ml
  cat: _build/default/.formatted/foo.ml: No such file or directory
  [1]

Formating a second time would not trigger the lock/solve.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
  $ cat _build/default/.formatted/foo.ml
  cat: _build/default/.formatted/foo.ml: No such file or directory
  [1]

When the lock dir is removed, the solving/lock is renewed:

  $ rm -r "${dev_tool_lock_dir}"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
