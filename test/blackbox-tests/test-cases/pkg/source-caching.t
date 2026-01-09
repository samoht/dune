This test demonstrates that fetching package sources should be cached

  $ make_lockdir

  $ tarball=source.tar
  $ sources="sources/"
  $ mkdir $sources; touch $sources/dummy
  $ tar cf $tarball $sources
  $ checksum=$(md5sum $tarball | awk '{ print $1 }')
  $ echo $tarball > fake-curls
  $ port=1

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (build (run echo building $1))
  > (source
  >  (fetch
  >   (url "http://0.0.0.0:$port")
  >   (checksum md5=$checksum)))
  > (version dev)
  > EOF
  > }
  $ makepkg foo

This command is expected to download the source:
  $ build_pkg foo
  File "dune.lock/foo.pkg", line 4, characters 7-25:
  4 |   (url "http://0.0.0.0:1")
             ^^^^^^^^^^^^^^^^^^
  Error: Download failed with code 404
         
  [1]

  $ wait

  $ makepkg bar

This command isn't expected to download the source. It will not be available as
the server will disappear after serving the first command.
  $ build_pkg bar 2>&1
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  Error: Failed to fetch bar.dev: Download failed with code 404
  
  [1]
