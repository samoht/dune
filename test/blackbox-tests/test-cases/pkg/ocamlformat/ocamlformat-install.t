Test `dune tools which ocamlformat`:

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir

Install ocamlformat as a dev tool:
  $ dune tools install ocamlformat
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  [1]

Verify that ocamlformat is installed:
  $ dune tools which ocamlformat
  Error: ocamlformat is not installed as a dev tool
  [1]
