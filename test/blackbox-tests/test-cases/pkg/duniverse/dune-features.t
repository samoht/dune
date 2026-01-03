Test that duniverse packages work with various dune features.

This test verifies that dune pkg integrates well with common dune features:
- Libraries depending on duniverse packages
- Executables depending on duniverse packages
- Tests using duniverse dependencies
- Subdirectories

Set up a project:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > EOF

Create duniverse with a package source:

  $ mkdir -p duniverse
  $ touch duniverse/.dune-duniverse

  $ mkdir -p duniverse/helper.1.0.0
  $ cat >duniverse/helper.1.0.0/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name helper))
  > EOF

  $ cat >duniverse/helper.1.0.0/dune <<EOF
  > (library (name helper) (public_name helper))
  > EOF

  $ cat >duniverse/helper.1.0.0/helper.ml <<EOF
  > let greet name = "Hello, " ^ name ^ "!"
  > EOF

Test 1: Library depending on duniverse package

  $ mkdir -p test1
  $ cat >test1/dune <<EOF
  > (library (name mylib1) (libraries helper))
  > EOF

  $ cat >test1/mylib1.ml <<EOF
  > let message = Helper.greet "World"
  > EOF

  $ dune build test1/mylib1.cma

Test 2: Executable depending on duniverse package

  $ mkdir -p test2
  $ cat >test2/dune <<EOF
  > (executable (name main2) (libraries helper))
  > EOF

  $ cat >test2/main2.ml <<EOF
  > let () = print_endline (Helper.greet "Exec")
  > EOF

  $ dune exec ./test2/main2.exe
  Hello, Exec!

Test 3: Test using duniverse dependency

  $ mkdir -p test3
  $ cat >test3/dune <<EOF
  > (test (name mytest3) (libraries helper))
  > EOF

  $ cat >test3/mytest3.ml <<EOF
  > let () =
  >   if Helper.greet "Test" = "Hello, Test!" then
  >     print_endline "PASS"
  >   else
  >     failwith "FAIL"
  > EOF

  $ dune runtest test3
  PASS

Test 4: Subdirectory using duniverse package

  $ mkdir -p test4/subdir
  $ cat >test4/dune <<EOF
  > (library (name test4lib) (libraries helper))
  > EOF

  $ cat >test4/test4lib.ml <<EOF
  > let msg = Helper.greet "Test4"
  > EOF

  $ cat >test4/subdir/dune <<EOF
  > (library (name sublib4) (libraries test4lib))
  > EOF

  $ cat >test4/subdir/sublib4.ml <<EOF
  > let sub_message = Test4lib.msg ^ " from subdir"
  > EOF

  $ dune build test4/subdir/sublib4.cma
