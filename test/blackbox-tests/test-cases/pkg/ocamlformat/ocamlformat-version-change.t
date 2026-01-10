When the version in .ocamlformat changes, automatically relock ocamlformat with
the new version.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.0"
  $ echo ocamlformat-0.26.0.tar > fake-curls
  $ make_ocamlformat_opam_pkg "0.26.0" 1

  $ make_fake_ocamlformat "0.27.0"
  $ echo ocamlformat-0.27.0.tar >> fake-curls
  $ make_ocamlformat_opam_pkg "0.27.0" 2

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Create .ocamlformat file
  $ cat > .ocamlformat <<EOF
  > version = 0.26.0
  > EOF

Install ocamlformat. 0.26.0 should be installed because that's the version in .ocamlformat.
  $ dune tools install ocamlformat
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.27.0
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  [1]

Change the version in .ocamlformat.
  $ cat > .ocamlformat <<EOF
  > version = 0.27.0
  > EOF

Install ocamlformat again. Dune should detect that the version has changed and relock:
  $ dune tools install ocamlformat
  Error: No rule found for .pkgs/ocamlformat/installed (context
  tools-ocamlformat)
  -> required by _build/install/default/bin/ocamlformat
  [1]
