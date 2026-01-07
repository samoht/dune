Test that we run the build command

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run touch %{prefix}/bin/foo)))
  > EOF

  $ build_pkg test

The pkg directory contains source and target dirs with a cookie for dependency tracking:

  $ show_pkg test
  
  /source
  /target
  /target/cookie


The build output goes to the shared install directory:

  $ ls _build/install/default/bin
  foo
