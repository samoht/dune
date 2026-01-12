Test missing entries in the .install file

  $ make_lockdir
  $ lockfile() {
  > make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo 'lib: [ \"$1\" ]' > test.install"))
  > EOF
  > }

This should give us a proper error that myfile wasn't generated

  $ lockfile "myfile"
  $ build_pkg test 2>&1 | dune_cmd subst '\.sandbox/[^/]+' '.sandbox/SANDBOX'
  Error: entry
  _build/.sandbox/SANDBOX/.pkgs/default/test/source/myfile
  in
  _build/.sandbox/SANDBOX/.pkgs/default/test/source/test.install
  does not exist
  -> required by _build/.pkgs/default/test/target/cookie
  -> required by _build/.pkgs/default/test/installed

This on the other hand shouldn't error because myfile is optional

  $ lockfile "?myfile"
  $ build_pkg test
