Demonstrate that we copy empty directories

  $ make_lockdir

  $ src=_foo
  $ mkdir $src
  $ cd $src
  $ mkdir empty
  $ cd ..

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (system "find . | sort"))
  > (source (copy $PWD/$src))
  > EOF

  $ build_pkg foo
  Error: Don't know how to build _build/.pkgs/default/foo/installed
  [1]
