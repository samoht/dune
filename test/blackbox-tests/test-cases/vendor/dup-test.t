  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

  $ mkdir -p duniverse/pkg1.1.0
  $ cat >duniverse/pkg1.1.0/pkg1.opam <<EOF
  > opam-version: "2.0"
  > name: "pkg1"
  > version: "1.0"
  > EOF
  $ cat >duniverse/pkg1.1.0/dune <<EOF
  > (library (name mylib))
  > EOF
  $ echo "let v = 1" > duniverse/pkg1.1.0/mylib.ml

  $ mkdir -p duniverse/pkg2.1.0
  $ cat >duniverse/pkg2.1.0/pkg2.opam <<EOF
  > opam-version: "2.0"
  > name: "pkg2"
  > version: "1.0"
  > EOF
  $ cat >duniverse/pkg2.1.0/dune <<EOF
  > (library (name mylib))
  > EOF
  $ echo "let v = 2" > duniverse/pkg2.1.0/mylib.ml

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor pkg1.1.0 (libraries mylib))
  > (vendor pkg2.1.0 (libraries mylib))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ echo "let () = print_int Mylib.v" > main.ml

  $ dune build main.exe 2>&1 | head -10
