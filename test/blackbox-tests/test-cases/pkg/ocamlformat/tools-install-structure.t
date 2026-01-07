Test that verifies the directory structure for `dune tools install`:
- Packages build in _build/pkg/tools-<name>/
- Packages install to _build/install/tools-<name>/
- .install file is copied to target/
- Promotion copies tool's files to _build/install/default/

  $ . ../helpers.sh

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
  Solution for _build/lock/tools-ocamlformat (1 package)
  dune:
  - ocamlformat.0.26.2

Verify the directory structure:

The lock directory should be created:

  $ ls _build/lock/tools-ocamlformat
  lock.dune
  ocamlformat.pkg

The package should be built in pkg/tools-ocamlformat:

  $ ls _build/pkg/tools-ocamlformat | grep "ocamlformat.0.26.2"
  ocamlformat.0.26.2-*

The package directory should have source and target:

  $ pkg_dir=$(ls -d _build/pkg/tools-ocamlformat/ocamlformat.0.26.2-* | head -1)
  $ ls "$pkg_dir"
  source
  target

The target directory should have cookie and .install file:

  $ ls "$pkg_dir/target" | sort
  cookie
  ocamlformat.install

The .install file should list the bin section with ocamlformat:

  $ grep -A1 "^bin:" "$pkg_dir/target/ocamlformat.install"
  bin: [
    *ocamlformat* (glob)

The shared install directory should have the binary:

  $ ls _build/install/tools-ocamlformat/bin
  ocamlformat

The binary should be promoted to the default context:

  $ ls _build/install/default/bin/ocamlformat
  _build/install/default/bin/ocamlformat

The promoted binary should work:

  $ _build/install/default/bin/ocamlformat --help 2>&1 | head -1
  formatted with version 0.26.2
