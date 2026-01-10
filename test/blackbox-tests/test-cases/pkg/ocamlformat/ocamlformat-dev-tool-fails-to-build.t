With a faulty version of OCamlFormat, "dune fmt" is supposed to stop with the
build error of "ocamlformat".

  $ mkrepo

Make a fake ocamlformat with a missing ocamlformat.ml file:
  $ make_fake_ocamlformat "0.26.4" "no-ml-file"
  $ make_ocamlformat_opam_pkg "0.26.4"

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

It fails during the build because of missing OCamlFormat module.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.4
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  -> required by _build/default/.formatted/foo.ml
  -> required by alias .formatted/fmt
  -> required by alias fmt
  [1]
