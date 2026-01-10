Install actions should have the switch directory prepared:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "find %{prefix} | sort"))
  > EOF

  $ build_pkg test
  Error: Don't know how to build _build/.pkgs/default/test/installed
  [1]
