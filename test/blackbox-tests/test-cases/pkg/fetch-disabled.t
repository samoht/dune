Test that --fetch=disabled blocks network access and shows a helpful error message.

Create a lock directory with a package that has an HTTP source:

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url https://example.com/mypkg.tar.gz)
  >   (checksum md5=00000000000000000000000000000000)))
  > (build (run dune build -p mypkg @install))
  > EOF

Create a project:

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

Now try to build the package with --fetch=disabled. Since the package source
hasn't been fetched yet, this should fail with a clear error message:

  $ dune build @pkg-install --fetch=disabled
  File "dune.lock/mypkg.pkg", line 4, characters 7-39:
  4 |   (url https://example.com/mypkg.tar.gz)
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package source not cached and network access is disabled
  (--fetch=disabled).
  Run 'dune pkg fetch' first to download package sources, or use
  --fetch=enabled.
  [1]
