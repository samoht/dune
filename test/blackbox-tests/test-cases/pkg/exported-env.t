Packages can export environment variables

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (exported_env
  >  (= FOO bar)
  >  (= BAR xxx)
  >  (+= BAR yyy)
  >  (:= BAR zzz))
  > EOF

  $ make_lockpkg usetest <<'EOF'
  > (depends test)
  > (version 1.2.3)
  > (build
  >  (progn
  >   (system "\| echo FOO=$FOO
  >           "\| echo BAR=$BAR
  >           "\| echo OPAM_PACKAGE_NAME=$OPAM_PACKAGE_NAME
  >           "\| echo OPAM_PACKAGE_VERSION=$OPAM_PACKAGE_VERSION
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  Error: Don't know how to build _build/.pkgs/default/usetest/installed
  [1]
