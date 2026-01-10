Test that verifies the directory structure for `dune tools install`:
- Packages build in _build/pkg/tools-<name>/
- Packages install to _build/install/tools-<name>/
- .install file is copied to target/
- Promotion copies tool's files to _build/install/default/

  $ . ../helpers.sh
  /var/folders/7g/4yr0hhnx5ml4kvvxfjszdqf00000gn/T/dune_cram_c5f048_.cram.sh/1.sh: line 1: ../helpers.sh: No such file or directory
  ***** UNREACHABLE *****

Set up a mock ocamlformat package:

  $ mkrepo
  ***** UNREACHABLE *****
  $ OCAMLFORMAT_VERSION="0.26.2"
  ***** UNREACHABLE *****
  $ make_fake_ocamlformat "${OCAMLFORMAT_VERSION}"
  ***** UNREACHABLE *****
  $ make_ocamlformat_opam_pkg "${OCAMLFORMAT_VERSION}"
  ***** UNREACHABLE *****

Set up project with .ocamlformat config:

  $ make_project_with_dev_tool_lockdir
  ***** UNREACHABLE *****
  $ cat > .ocamlformat <<EOF
  > version = ${OCAMLFORMAT_VERSION}
  > EOF
  ***** UNREACHABLE *****

Install ocamlformat using `dune tools install`:

  $ dune tools install ocamlformat
  ***** UNREACHABLE *****

Verify the directory structure:

The lock directory should be created:

  $ ls _build/.locks/tools-ocamlformat
  ***** UNREACHABLE *****

The package should be built in pkg/tools-ocamlformat:

  $ ls _build/pkg/tools-ocamlformat | grep "ocamlformat.0.26.2"
  ***** UNREACHABLE *****

The package directory should have source and target:

  $ pkg_dir=$(ls -d _build/pkg/tools-ocamlformat/ocamlformat.0.26.2-* | head -1)
  ***** UNREACHABLE *****
  $ ls "$pkg_dir"
  ***** UNREACHABLE *****

The target directory should have cookie and .install file:

  $ ls "$pkg_dir/target" | sort
  ***** UNREACHABLE *****

The .install file should list the bin section with ocamlformat:

  $ grep -A1 "^bin:" "$pkg_dir/target/ocamlformat.install"
  ***** UNREACHABLE *****

The shared install directory should have the binary:

  $ ls _build/install/tools-ocamlformat/bin
  ***** UNREACHABLE *****

The binary should be promoted to the default context:

  $ ls _build/install/default/bin/ocamlformat
  ***** UNREACHABLE *****

The promoted binary should work:

  $ _build/install/default/bin/ocamlformat --help 2>&1 | head -1
  ***** UNREACHABLE *****
