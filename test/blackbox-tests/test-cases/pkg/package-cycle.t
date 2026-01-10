Package resolution creating a cycle

  $ make_lockdir
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (depends a)
  > EOF

  $ build_pkg a
  Error: Don't know how to build _build/.pkgs/default/a/installed
  [1]
