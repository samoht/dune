Test the (toolchain native) field in vendor stanzas for providing OCaml compilers.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a vendored OCaml compiler package (mock):

  $ mkdir -p duniverse/ocaml.5.2.0
  $ cat >duniverse/ocaml.5.2.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name ocaml))
  > EOF

  $ cat >duniverse/ocaml.5.2.0/dune <<EOF
  > (library
  >  (name ocaml_stdlib)
  >  (modules ocaml_stdlib)
  >  (public_name ocaml.stdlib))
  > EOF

  $ cat >duniverse/ocaml.5.2.0/ocaml_stdlib.ml <<EOF
  > let version = "5.2.0"
  > EOF

Set up the vendor stanza with toolchain native:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor ocaml.5.2.0
  >  (libraries ocaml.stdlib)
  >  (toolchain native))
  > EOF

Create main app:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (public_name myapp))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline "Hello"
  > EOF

Build should succeed (uses system compiler since vendor toolchain doesn't
actually provide ocamlc/ocamlopt binaries):

  $ dune build main.exe

Test with packages field as well:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor ocaml.5.2.0
  >  (libraries ocaml.stdlib)
  >  (packages ocaml)
  >  (toolchain native))
  > EOF

  $ dune build main.exe
