Make sure that we verify archives of local archives

  $ make_lockdir

  $ touch foo.tar.gz

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (checksum md5=069aa55d40e548280f92af693f6c625a)
  >   (url $PWD/foo.tar.gz)))
  > EOF

  $ build_pkg foo
     Vendoring foo.0.0.1
  Error: Checksum mismatch for foo.0.0.1 (got
  md5=d41d8cd98f00b204e9800998ecf8427e)
  [1]
