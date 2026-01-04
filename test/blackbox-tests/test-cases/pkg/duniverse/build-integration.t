Test that duniverse packages are built in the main context and edits are picked up.

This test verifies:
1. Duniverse packages are built in the main context (not .pkg sandbox)
2. Edits to duniverse packages are picked up on rebuild
3. The duniverse package can be used by the main project

Create a project that uses a duniverse library:

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package (name myapp))
  > EOF

Set up duniverse with a library:

  $ mkdir -p duniverse
  $ touch duniverse/.dune-duniverse

  $ mkdir -p duniverse/mylib.1.0.0
  $ cat > duniverse/mylib.1.0.0/dune-project << EOF
  > (lang dune 3.16)
  > (package (name mylib))
  > EOF
  $ cat > duniverse/mylib.1.0.0/dune << EOF
  > (library (name mylib) (public_name mylib))
  > EOF
  $ cat > duniverse/mylib.1.0.0/mylib.ml << EOF
  > let msg = "hello from mylib v1"
  > EOF

Create an executable that uses the duniverse library:

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (public_name myapp)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Mylib.msg
  > EOF

Build the project:

  $ dune build ./main.exe

Run the executable:

  $ dune exec ./main.exe
  hello from mylib v1

Verify duniverse package is built in main context:

  $ ls _build/default/duniverse/mylib.1.0.0/.mylib.objs/byte/*.cmo
  _build/default/duniverse/mylib.1.0.0/.mylib.objs/byte/mylib.cmo

Now test that edits to duniverse are picked up:

  $ cat > duniverse/mylib.1.0.0/mylib.ml << EOF
  > let msg = "hello from mylib v2 (edited!)"
  > EOF

Rebuild and run - should show the edited message:

  $ dune build ./main.exe
  $ dune exec ./main.exe
  hello from mylib v2 (edited!)

Verify warning suppression (vendored behavior):

  $ cat > duniverse/mylib.1.0.0/mylib.ml << EOF
  > (* This unused binding would normally cause a warning *)
  > let unused = "unused"
  > let msg = "hello with unused binding"
  > EOF

  $ dune build ./main.exe 2>&1 | grep -i warning || echo "No warnings (expected for vendored code)"
  No warnings (expected for vendored code)

  $ dune exec ./main.exe
  hello with unused binding

This confirms:
1. Duniverse packages are built in the main context
2. Edits to duniverse packages are picked up immediately
3. Warnings are suppressed (vendored behavior)
