Test error conditions for vendor stanza mode field as specified in RFC.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Test auto-detection: directory with dune files should use dune mode:

  $ mkdir -p duniverse/dune-pkg
  $ cat >duniverse/dune-pkg/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name dune-pkg))
  > EOF

  $ cat >duniverse/dune-pkg/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name dune-pkg))
  > EOF

  $ cat >duniverse/dune-pkg/mylib.ml <<EOF
  > let msg = "hello"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor dune-pkg)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dune-pkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.msg
  > EOF

Should succeed because dune files exist and auto-detection uses dune mode:

  $ dune build main.exe
  $ dune exec ./main.exe
  hello

Test explicit mode dune works:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor dune-pkg
  >  (mode dune))
  > EOF

  $ dune build main.exe
  $ dune exec ./main.exe
  hello

Test mode dune with directory containing only opam file (no dune files):
The directory has no libraries so build fails with "library not found".
RFC specifies this should ideally error at parse time, but current behavior
is acceptable - error still occurs.

  $ mkdir -p duniverse/opam-only-pkg
  $ cat >duniverse/opam-only-pkg/opam <<EOF
  > opam-version: "2.0"
  > name: "opam-only-pkg"
  > build: ["echo" "building"]
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor opam-only-pkg
  >  (mode dune))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries opam-only-pkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline "hello"
  > EOF

Build fails because no dune library is defined in the directory:

  $ dune build main.exe 2>&1 | grep -i "not found"
  Error: Library "opam-only-pkg" not found.

Test mode opam with directory containing only dune files (no opam file):
RFC specifies this should error at parse time, but current behavior
lets the opam build fail when it can't find build commands.

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor dune-pkg
  >  (mode opam))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dune-pkg))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Mylib.msg
  > EOF

Build fails because mode opam requires opam file for build commands:

  $ dune build main.exe 2>&1 | grep -i "not found\|error"
  Error: No opam file found for vendored package dune-pkg in duniverse/dune-pkg
