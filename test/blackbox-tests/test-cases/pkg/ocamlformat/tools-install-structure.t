Test that verifies the directory structure for `dune tools install`:
- Lock directory is created at _build/.locks/tools-<name>/
- Promotion copies tool's files to _build/install/default/

Set up a mock ocamlformat package:

  $ mkrepo
  $ OCAMLFORMAT_VERSION="0.26.2"
  $ make_fake_ocamlformat "${OCAMLFORMAT_VERSION}"
  $ make_ocamlformat_opam_pkg "${OCAMLFORMAT_VERSION}"

Set up project with .ocamlformat config:

  $ make_project_with_dev_tool_lockdir
  $ cat > .ocamlformat <<EOF
  > version = ${OCAMLFORMAT_VERSION}
  > EOF

Install ocamlformat using `dune tools install`:

  $ dune tools install ocamlformat
  Solution for _build/.locks/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2

Verify the directory structure:

The lock directory should be created:

  $ ls _build/.locks/tools-ocamlformat
  lock.dune
  ocamlformat.pkg

The binary should be promoted to the default context:

  $ ls _build/install/default/bin/ocamlformat
  _build/install/default/bin/ocamlformat

The promoted binary should work:

  $ _build/install/default/bin/ocamlformat --help 2>&1 | head -1
  formatted with version 0.26.2
