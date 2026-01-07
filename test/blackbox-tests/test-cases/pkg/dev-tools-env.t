Test `dune tools env` command for both POSIX and fish shells.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

The POSIX shell output should export PATH with dev tool paths prepended.
Check that it starts with 'export PATH=' and contains dev tool paths.

  $ dune tools env | head -1 | cut -d: -f1-10
  export PATH=$TESTCASE_ROOT/_build/install/dev-tools-merlin/bin:$TESTCASE_ROOT/_build/install/dev-tools-ocaml-index/bin:$TESTCASE_ROOT/_build/install/dev-tools-dune-release/bin:$TESTCASE_ROOT/_build/install/dev-tools-opam-publish/bin:$TESTCASE_ROOT/_build/install/dev-tools-odig/bin:$TESTCASE_ROOT/_build/install/dev-tools-earlybird/bin:$TESTCASE_ROOT/_build/install/dev-tools-utop/bin:$TESTCASE_ROOT/_build/install/dev-tools-ocaml-lsp-server/bin:$TESTCASE_ROOT/_build/install/dev-tools-odoc/bin:$TESTCASE_ROOT/_build/install/dev-tools-ocamlformat/bin

The fish shell output should use fish_add_path with dev tool paths.

  $ dune tools env --fish
  fish_add_path --prepend _build/install/dev-tools-ocamlformat/bin _build/install/dev-tools-odoc/bin _build/install/dev-tools-ocaml-lsp-server/bin _build/install/dev-tools-utop/bin _build/install/dev-tools-earlybird/bin _build/install/dev-tools-odig/bin _build/install/dev-tools-opam-publish/bin _build/install/dev-tools-dune-release/bin _build/install/dev-tools-ocaml-index/bin _build/install/dev-tools-merlin/bin
