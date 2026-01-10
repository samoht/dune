This test demonstrates a package where the .install file being created by the
file copying step rather than the build step.

  $ make_lockdir
  $ mkdir -p ${default_lock_dir}/foo.files

  $ touch ${default_lock_dir}/foo.files/foo.install
  $ echo "(version 0.0.1)" > ${default_lock_dir}/foo.pkg

The foo.install file in files/ should have been copied over.
  $ build_pkg foo
  Error:
  symlink(_build/.sandbox/ddf19bf2399f20d1194608d91a92c4b3/.locks/default/dune.lock/foo.files): File exists
  -> required by _build/.pkgs/default/foo/target/cookie
  -> required by _build/.pkgs/default/foo/installed
  [1]
