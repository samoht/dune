Test workspace package dependencies - when opam packages from lock files
depend on packages defined in the workspace.

===========================================
SETUP: Create helper for mock opam repo
===========================================

  $ mkrepo

===========================================
TEST 1: Basic workspace dependency
===========================================

Lock file package depends on a workspace package.

Create a workspace package:

  $ mkdir -p wslib
  $ cat > wslib/dune-project <<EOF
  > (lang dune 3.17)
  > (package (name wslib))
  > EOF

  $ cat > wslib/dune <<EOF
  > (library
  >  (name wslib)
  >  (public_name wslib))
  > EOF

  $ cat > wslib/wslib.ml <<EOF
  > let msg = "from workspace"
  > EOF

Create an opam package that depends on wslib:

  $ mkpkg consumer <<EOF
  > depends: ["wslib"]
  > build: ["echo" "building consumer"]
  > EOF

Set up main project that depends on consumer:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name main)
  >  (depends consumer))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name main)
  >  (libraries wslib))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Wslib.msg
  > EOF

Lock and build:

  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - consumer.0.0.1



  $ dune exec ./main.exe
  from workspace

The workspace package should be built and available.

===========================================
TEST 2: Transitive workspace dependency
===========================================

Lock A depends on Lock B which depends on Workspace C.

Clean up:

  $ rm -rf _build dune.lock

  $ mkpkg pkg-a <<EOF
  > depends: ["pkg-b"]
  > build: ["echo" "building pkg-a"]
  > EOF

  $ mkpkg pkg-b <<EOF
  > depends: ["wslib"]
  > build: ["echo" "building pkg-b"]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name main)
  >  (depends pkg-a))
  > EOF

  $ dune pkg lock
  Solution for dune.lock (2 packages)
  
  Dependencies common to all supported platforms:
  opam:
  - pkg-a.0.0.1
  - pkg-b.0.0.1



  $ dune exec ./main.exe
  from workspace

===========================================
TEST 3: Multiple workspace packages
===========================================

Lock package depends on multiple workspace packages.

  $ rm -rf _build dune.lock

  $ mkdir -p wslib2
  $ cat > wslib2/dune-project <<EOF
  > (lang dune 3.17)
  > (package (name wslib2))
  > EOF

  $ cat > wslib2/dune <<EOF
  > (library
  >  (name wslib2)
  >  (public_name wslib2))
  > EOF

  $ cat > wslib2/wslib2.ml <<EOF
  > let msg2 = "from workspace 2"
  > EOF

  $ mkpkg multi-dep <<EOF
  > depends: ["wslib" "wslib2"]
  > build: ["echo" "building multi-dep"]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name main)
  >  (depends multi-dep))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name main)
  >  (libraries wslib wslib2))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline Wslib.msg;
  >   print_endline Wslib2.msg2
  > EOF

  $ dune pkg lock
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - multi-dep.0.0.1



  $ dune exec ./main.exe
  from workspace
  from workspace 2

===========================================
TEST 4: Workspace package with internal deps
===========================================

Workspace package depends on another workspace package.

  $ rm -rf _build dune.lock

  $ mkdir -p wsbase wstop
  $ cat > wsbase/dune-project <<EOF
  > (lang dune 3.17)
  > (package (name wsbase))
  > EOF

  $ cat > wsbase/dune <<EOF
  > (library
  >  (name wsbase)
  >  (public_name wsbase))
  > EOF

  $ cat > wsbase/wsbase.ml <<EOF
  > let base_msg = "base"
  > EOF

  $ cat > wstop/dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name wstop)
  >  (depends wsbase))
  > EOF

  $ cat > wstop/dune <<EOF
  > (library
  >  (name wstop)
  >  (public_name wstop)
  >  (libraries wsbase))
  > EOF

  $ cat > wstop/wstop.ml <<EOF
  > let top_msg = Wsbase.base_msg ^ "-top"
  > EOF

  $ mkpkg consumer2 <<EOF
  > depends: ["wstop"]
  > build: ["echo" "building consumer2"]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name main)
  >  (depends consumer2))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name main)
  >  (libraries wstop))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Wstop.top_msg
  > EOF

  $ dune pkg lock
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - consumer2.0.0.1



  $ dune exec ./main.exe
  base-top

===========================================
TEST 5: Direct use of workspace package (no lock)
===========================================

Using a workspace package directly without going through pkg lock.

  $ rm -rf _build dune.lock wslib wslib2 wsbase wstop

  $ mkdir -p mylib
  $ cat > mylib/dune-project <<EOF
  > (lang dune 3.17)
  > (package (name mylib))
  > EOF

  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib/mylib.ml <<EOF
  > let value = 42
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package (name main))
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (public_name main)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Printf.printf "value = %d\n" Mylib.value
  > EOF

  $ dune exec ./main.exe
  value = 42
