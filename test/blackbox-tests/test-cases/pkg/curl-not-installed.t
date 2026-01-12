Test the error message when curl is needed but not installed.

  $ make_lockdir

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "http://0.0.0.0:8000")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package in an environment without curl.
  $ PATH=$(dirname $(which dune)) build_pkg foo
  Error: Program git not found in the tree or in PATH
  Hint: Git is required for version information in 'dune subst', build info,
  and package management. Install git or add it to your PATH.
  [1]
