Test the :standard ordered set language syntax in vendor stanzas.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (package (name myapp))
  > EOF

Create a vendored package with multiple libraries in separate directories:

  $ mkdir -p duniverse/multi-lib/lib_a
  $ mkdir -p duniverse/multi-lib/lib_b
  $ mkdir -p duniverse/multi-lib/lib_c
  $ cat >duniverse/multi-lib/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name multi-lib))
  > EOF

  $ cat >duniverse/multi-lib/lib_a/dune <<EOF
  > (library
  >  (name lib_a)
  >  (public_name multi-lib.a))
  > EOF

  $ cat >duniverse/multi-lib/lib_b/dune <<EOF
  > (library
  >  (name lib_b)
  >  (public_name multi-lib.b))
  > EOF

  $ cat >duniverse/multi-lib/lib_c/dune <<EOF
  > (library
  >  (name lib_c)
  >  (public_name multi-lib.c))
  > EOF

  $ cat >duniverse/multi-lib/lib_a/lib_a.ml <<EOF
  > let msg = "from a"
  > EOF

  $ cat >duniverse/multi-lib/lib_b/lib_b.ml <<EOF
  > let msg = "from b"
  > EOF

  $ cat >duniverse/multi-lib/lib_c/lib_c.ml <<EOF
  > let msg = "from c"
  > EOF

Test explicit library list (no :standard):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries multi-lib.a multi-lib.b))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a multi-lib.b))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Lib_a.msg ^ " " ^ Lib_b.msg)
  > EOF

  $ dune build main.exe

Test :standard (all libraries):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a multi-lib.b multi-lib.c))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Lib_a.msg ^ " " ^ Lib_b.msg ^ " " ^ Lib_c.msg)
  > EOF

  $ dune build main.exe

Test :standard with exclusion:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard \ multi-lib.c))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a multi-lib.b))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Lib_a.msg ^ " " ^ Lib_b.msg)
  > EOF

  $ dune build main.exe

Verify excluded library is not available:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a multi-lib.b multi-lib.c))
  > EOF

  $ dune build main.exe 2>&1 | grep -i "library.*not found\|error"
  Error: Library "multi-lib.c" not found.

Test :standard with multiple exclusions:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard \ multi-lib.b multi-lib.c))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Lib_a.msg
  > EOF

  $ dune build main.exe

Test aliasing syntax with simple names (should imply install false):
Note: Aliased libraries wrap their internal modules, so access is through
the wrapper module (e.g., Mylib.Lib_a.msg).

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries (multi-lib.a :as mylib) multi-lib.b))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib multi-lib.b))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Mylib.Lib_a.msg ^ " " ^ Lib_b.msg)
  > EOF

  $ dune build main.exe

Error: Cannot mix :standard with explicit names:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard multi-lib.a))
  > EOF

  $ dune build main.exe 2>&1 | grep -i error
  Error: Cannot mix :standard with explicit library names

Error: \ can only be used after :standard:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries multi-lib.a \ multi-lib.b))
  > EOF

  $ dune build main.exe 2>&1 | grep -i error
  Error: \ can only be used after :standard in vendor stanzas

Error: Cannot use :as aliasing in exclusion list:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard \ (multi-lib.c :as foo)))
  > EOF

  $ dune build main.exe 2>&1 | grep -i error
  Error: Cannot use :as aliasing in exclusion list

Test packages field with :standard:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (packages :standard))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Lib_a.msg
  > EOF

  $ dune build main.exe

Test packages field with exclusion:

  $ mkdir -p duniverse/multi-pkg/pkg_a
  $ mkdir -p duniverse/multi-pkg/pkg_b
  $ cat >duniverse/multi-pkg/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name pkg-a))
  > (package (name pkg-b))
  > EOF

  $ cat >duniverse/multi-pkg/pkg_a/dune <<EOF
  > (library
  >  (name lib_from_a)
  >  (public_name pkg-a))
  > EOF

  $ cat >duniverse/multi-pkg/pkg_a/lib_from_a.ml <<EOF
  > let msg = "from pkg a"
  > EOF

  $ cat >duniverse/multi-pkg/pkg_b/dune <<EOF
  > (library
  >  (name lib_from_b)
  >  (public_name pkg-b))
  > EOF

  $ cat >duniverse/multi-pkg/pkg_b/lib_from_b.ml <<EOF
  > let msg = "from pkg b"
  > EOF

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor multi-lib
  >  (libraries :standard))
  > (vendor multi-pkg
  >  (packages :standard \ pkg-b))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries multi-lib.a pkg-a))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Lib_a.msg ^ " " ^ Lib_from_a.msg)
  > EOF

  $ dune build main.exe
