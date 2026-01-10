Test that the OCAMLFIND_DESTDIR environment variable is set when running
install and build commands.

  $ make_lockdir
  $ make_lockpkg test <<'EOF'
  > (version 0.0.1)
  > (build (run sh -c "echo [build] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > (install (run sh -c "echo [install] OCAMLFIND_DESTDIR=$OCAMLFIND_DESTDIR"))
  > EOF

  $ build_pkg test 2>&1 \
  > | dune_cmd subst "$PWD" PWD \
  > | dune_cmd subst '\.sandbox/.*/_private' '.sandbox/SANDBOX/_private'
  Error: Don't know how to build _build/.pkgs/default/test/installed
