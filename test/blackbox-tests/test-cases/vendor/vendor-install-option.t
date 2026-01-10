Test the (install false) option in vendor stanza for opam packages.

This option prevents installation to the shared prefix, which is useful for
packages with library remapping that should coexist with other versions.

Reference: rfc/vendor.md

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > EOF

Test basic (install false) syntax with a simple dune package:

  $ mkdir -p duniverse/simple.1.0.0
  $ cat >duniverse/simple.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name simple))
  > EOF
  $ cat >duniverse/simple.1.0.0/dune <<EOF
  > (library
  >  (name simple)
  >  (modules simple)
  >  (public_name simple))
  > EOF
  $ cat >duniverse/simple.1.0.0/simple.ml <<EOF
  > let x = 42
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor simple.1.0.0 (install false))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries simple))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_int Simple.x
  > EOF

  $ dune build main.exe 2>&1 | head -5

Test (install true) is the default (explicit):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor simple.1.0.0 (install true))
  > EOF

  $ dune build main.exe 2>&1 | head -5

Test (install false) with opam mode package:

  $ mkdir -p duniverse/mypkg.1.0.0
  $ cat >duniverse/mypkg.1.0.0/opam <<EOF
  > opam-version: "2.0"
  > name: "mypkg"
  > version: "1.0.0"
  > build: ["true"]
  > install: ["true"]
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

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (mode opam) (install false))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mypkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mypkg.version
  > EOF

  $ dune build main.exe 2>&1 | head -10

Test combined options (mode + libraries + install):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor mypkg.1.0.0 (mode opam) (libraries mypkg) (install false))
  > EOF

  $ dune build main.exe 2>&1 | head -5
