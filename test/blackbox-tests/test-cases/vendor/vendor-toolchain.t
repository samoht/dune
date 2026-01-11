Test the (toolchain ...) field in vendor stanzas for cross-compilation.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a vendored cross-compiler toolchain:

  $ mkdir -p duniverse/ocaml-arm.1.0.0
  $ cat >duniverse/ocaml-arm.1.0.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name ocaml-arm))
  > EOF

  $ cat >duniverse/ocaml-arm.1.0.0/dune <<EOF
  > (library
  >  (name arm_support)
  >  (modules arm_support)
  >  (public_name ocaml-arm.support))
  > EOF

  $ cat >duniverse/ocaml-arm.1.0.0/arm_support.ml <<EOF
  > let platform = "arm-linux-gnueabihf"
  > EOF

Set up the vendor stanza with toolchain field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor ocaml-arm.1.0.0
  >  (libraries ocaml-arm.support)
  >  (toolchain arm-linux-gnueabihf))
  > EOF

Create main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (public_name myapp)
  >  (libraries ocaml-arm.support))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Arm_support.platform
  > EOF

Build should succeed with the vendored library:

  $ dune build main.exe

Test that the toolchain field is parsed correctly and findlib paths are set.
We create a workspace with a target toolchain matching the vendor declaration:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > (context
  >  (default
  >   (targets native arm-linux-gnueabihf)))
  > EOF

The build should still work (native target uses vendored library):

  $ dune build main.exe
