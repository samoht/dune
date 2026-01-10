Test that dune can handle the case where a dependency's source contains a
symlink with a missing destination.

Define a package foo containing a broken symlink.
  $ mkdir foo
  $ touch foo/a.txt
  $ ln -s non_existent foo/b.txt

Define a package bar containing a broken symlink.
  $ mkdir bar
  $ touch bar/a.txt
  $ ln -s non_existent bar/b.txt
  $ tar czf bar.tar.gz bar

Make a directory to contain a test project and change to it.
  $ mkdir project
  $ cd project

Create a lockdir for the project.
  $ make_lockdir

The package "foo" exercises copying package sources from a local directory.
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url
  >    file:///$PWD/../foo)))
  > EOF

The package "bar" exercises extracting a source archive from a local file.
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url
  >    file:///$PWD/../bar.tar.gz)))
  > EOF

The package "bar" exercises extracting a source archive from a downloaded file.
  $ make_lockpkg baz <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url http://0.0.0.0:1)
  >   (checksum md5=$(md5sum $PWD/../bar.tar.gz | cut -f1 -d' '))))
  > EOF

Set up a fake web server to serve the source archive for the package "bar".
  $ echo $PWD/../bar.tar.gz >> fake-curls

Make a project file that depends on all the packages.
  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo bar baz))
  > EOF

Build the packages.
  $ build_pkg foo
  Error: Don't know how to build _build/.pkgs/default/foo/installed
  [1]
  $ build_pkg bar
  Error: No opam file found for vendored package bar in duniverse/bar.0.0.1
  -> required by - package bar
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: No opam file found for vendored package baz in duniverse/baz.0.0.1
  -> required by - package baz
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: No opam file found for vendored package foo in duniverse/foo.0.0.1
  -> required by - package foo
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: Vendor directory duniverse/bar.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  Error: Vendor directory duniverse/baz.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  Error: Vendor directory duniverse/foo.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  [1]
  $ build_pkg baz
  Error: No opam file found for vendored package bar in duniverse/bar.0.0.1
  -> required by - package bar
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: No opam file found for vendored package baz in duniverse/baz.0.0.1
  -> required by - package baz
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: No opam file found for vendored package foo in duniverse/foo.0.0.1
  -> required by - package foo
  -> required by lock directory environment for context "default"
  -> required by base environment for context "default"
  -> required by loading findlib for context "default"
  -> required by loading the OCaml compiler for context "default"
  Error: Vendor directory duniverse/bar.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  Error: Vendor directory duniverse/baz.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  Error: Vendor directory duniverse/foo.0.0.1 has (mode opam) but no opam file
  found. Try running 'dune pkg fetch' to generate opam files.
  [1]

All files were copied except for the broken symlinks:
  $ ls _build/_private/default/.pkg/foo.*/source
  ls: _build/_private/default/.pkg/foo.*/source: No such file or directory
  [1]
  $ ls _build/_private/default/.pkg/bar.*/source
  ls: _build/_private/default/.pkg/bar.*/source: No such file or directory
  [1]
  $ ls _build/_private/default/.pkg/baz.*/source
  ls: _build/_private/default/.pkg/baz.*/source: No such file or directory
  [1]
