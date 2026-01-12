Some environment variables are automatically exported by packages:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > EOF
  $ make_lockpkg usetest <<'EOF'
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (system "\| echo MANPATH=$MANPATH
  >          "\| echo OCAMLPATH=$OCAMLPATH
  >          "\| echo CAML_LD_LIBRARY_PATH=$CAML_LD_LIBRARY_PATH
  >          "\| echo OCAMLTOP_INCLUDE_PATH=$OCAMLTOP_INCLUDE_PATH
  >          "\| echo PATH=$PATH
  >  ))
  > EOF

  $ mkdir .bin
  $ ln -s $(which ocamlc) .bin/ocamlc
  $ ln -s $(which sh) .bin/sh
  $ dune=$(which dune)
  $ MANPATH="" OCAMLPATH="" CAML_LD_LIBRARY_PATH="" OCAMLTOP_INCLUDE_PATH="" PATH="$PWD/.bin" build_pkg usetest
  Error: Program git not found in the tree or in PATH
  Hint: Git is required for version information in 'dune subst', build info,
  and package management. Install git or add it to your PATH.
  [1]
