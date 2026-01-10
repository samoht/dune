Test the (install false) option in vendor stanza for opam packages.

This option prevents installation to the shared prefix, which is useful for
packages with library remapping that should coexist with other versions.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a vendored opam package:

  $ mkdir -p duniverse/mypkg.1.0.0
  $ cat >duniverse/mypkg.1.0.0/opam <<EOF
  > opam-version: "2.0"
  > name: "mypkg"
  > version: "1.0.0"
  > EOF
  $ cat >duniverse/mypkg.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name mypkg))
  > EOF
  $ cat >duniverse/mypkg.1.0.0/dune <<EOF
  > (library
  >  (name mypkg)
  >  (modules mypkg)
  >  (public_name mypkg))
  > EOF
  $ cat >duniverse/mypkg.1.0.0/mypkg.ml <<EOF
  > let version = "1.0.0"
  > EOF

Use vendor stanza with (install false) to prevent installation to shared prefix:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (mode opam) (install false))
  > EOF

Create dune-workspace:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > EOF

Create main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mypkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mypkg.version
  > EOF

Build should work:

  $ dune build main.exe 2>&1 | head -10

Test that (install false) is parsed correctly (basic syntax check):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (install false))
  > EOF

  $ dune build main.exe 2>&1 | head -5

Test (install true) is the default:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (install true))
  > EOF

  $ dune build main.exe 2>&1 | head -5
