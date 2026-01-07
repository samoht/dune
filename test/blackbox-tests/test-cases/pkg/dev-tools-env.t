Test `dune tools env` command for both POSIX and fish shells.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

The POSIX shell output should export PATH with the default install bin path.
All dev tools are promoted to _build/install/default/bin/.

  $ dune tools env
  export PATH=$TESTCASE_ROOT/_build/install/default/bin:$PATH

The fish shell output should use fish_add_path.

  $ dune tools env --fish
  fish_add_path --prepend _build/install/default/bin
