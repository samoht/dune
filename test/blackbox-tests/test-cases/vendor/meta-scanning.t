Test that vendor packages with META files have their libraries properly discovered.

Setup a vendored package that uses META instead of dune files:

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a vendored package with a META file at the root:

  $ mkdir -p duniverse/mypkg.1.0.0
  $ cat >duniverse/mypkg.1.0.0/META <<EOF
  > description = "My package"
  > version = "1.0.0"
  > archive(byte) = "mypkg.cma"
  > archive(native) = "mypkg.cmxa"
  > package "sub" (
  >   description = "Subpackage"
  >   version = "1.0.0"
  >   archive(byte) = "mypkg_sub.cma"
  >   archive(native) = "mypkg_sub.cmxa"
  > )
  > EOF

  $ cat >duniverse/mypkg.1.0.0/mypkg.ml <<EOF
  > let greeting = "Hello from mypkg"
  > EOF

  $ cat >duniverse/mypkg.1.0.0/mypkg_sub.ml <<EOF
  > let sub_greeting = "Hello from mypkg.sub"
  > EOF

Use vendored_dirs with :standard to scan for libraries:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0)
  > EOF

The vendor stanza with implicit :standard should scan META and discover libraries.

Create a main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mypkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mypkg.greeting
  > EOF

Build should work - the library should be discovered from META:

  $ dune build main.exe 2>&1 | head -10

Test explicit libraries override scanning:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (libraries mypkg))
  > EOF

  $ dune build main.exe 2>&1 | head -10

Test pkg/META location (common in some packages like bytesrw):

  $ mkdir -p duniverse/thirdpkg.1.0.0/pkg
  $ cat >duniverse/thirdpkg.1.0.0/pkg/META <<EOF
  > description = "Third package"
  > version = "1.0.0"
  > archive(byte) = "thirdpkg.cma"
  > archive(native) = "thirdpkg.cmxa"
  > package "extra" (
  >   description = "Extra subpackage"
  >   version = "1.0.0"
  >   archive(byte) = "thirdpkg_extra.cma"
  > )
  > EOF

  $ cat >duniverse/thirdpkg.1.0.0/thirdpkg.ml <<EOF
  > let msg = "Hello from thirdpkg"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (libraries mypkg))
  > (vendor thirdpkg.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries thirdpkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Thirdpkg.msg
  > EOF

  $ dune build main.exe 2>&1 | head -10

Test :standard \ exclusions:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (libraries :standard \ mypkg.sub))
  > (vendor thirdpkg.1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mypkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mypkg.greeting
  > EOF

  $ dune build main.exe 2>&1 | head -10
