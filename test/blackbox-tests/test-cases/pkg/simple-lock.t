Test that we run the build and install commands

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build (run true))
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)))
  > EOF

  $ build_pkg test

The pkg directory contains build marker and cookie for dependency tracking:

  $ show_pkg test
  
  /.dune-pkg-build-done
  /cookie
  /source



The build output goes to the shared install directory:

  $ ls _build/install/default/bin
  foo
