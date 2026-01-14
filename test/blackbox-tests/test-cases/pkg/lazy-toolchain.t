Test that toolchain packages are built lazily - only when needed.

  $ mkrepo
  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create a C-only package that doesn't need OCaml:

  $ mkdir -p c-pkg-source
  $ cat >c-pkg-source/hello.c <<'EOF'
  > #include <stdio.h>
  > int main() { printf("Hello from C\n"); return 0; }
  > EOF
  $ cat >c-pkg-source/Makefile <<'EOF'
  > all:
  > 	@echo "Building C package (no OCaml needed)"
  > 	@$(CC) -o hello hello.c
  > install:
  > 	@echo "Installing C package"
  > 	@mkdir -p $(PREFIX)/bin
  > 	@cp hello $(PREFIX)/bin/
  > EOF
  $ cat >c-pkg-source/opam <<'EOF'
  > opam-version: "2.0"
  > name: "c-pkg"
  > version: "1.0"
  > build: [make]
  > install: [make "install" "PREFIX=%{prefix}%"]
  > EOF

Create a fake OCaml compiler package (to simulate toolchain):

  $ mkdir -p ocaml-source
  $ cat >ocaml-source/Makefile <<'EOF'
  > all:
  > 	@echo "BUILDING OCAML TOOLCHAIN - this should not happen for c-pkg!"
  > 	exit 1
  > install:
  > 	@echo "Installing fake ocaml"
  > EOF
  $ cat >ocaml-source/opam <<'EOF'
  > opam-version: "2.0"
  > name: "ocaml"
  > version: "5.0.0"
  > build: [make]
  > install: [make "install" "PREFIX=%{prefix}%"]
  > EOF

Set up workspace:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends c-pkg))
  > EOF

Create lock file with both packages (ocaml as the toolchain, c-pkg as dependency):

  $ make_lockdir
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (ocaml ocaml)
  > EOF
  $ make_lockpkg ocaml <<EOF
  > (version 5.0.0)
  > (build (run make))
  > (install (run make install PREFIX=%{prefix}))
  > (source (copy $PWD/ocaml-source))
  > EOF
  $ make_lockpkg c-pkg <<EOF
  > (version 1.0)
  > (build (run make))
  > (install (run make install PREFIX=%{prefix}))
  > (source (copy $PWD/c-pkg-source))
  > EOF

Build c-pkg - this should NOT trigger ocaml toolchain build:

  $ dune build _build/.pkgs/default/c-pkg/target/bin/hello 2>&1
     Vendoring c-pkg.1.0
     Vendoring ocaml.5.0.0
  Building C package (no OCaml needed)
  Installing C package

Verify the C binary was built:

  $ test -f _build/.pkgs/default/c-pkg/target/bin/hello && echo "C package built successfully"
  C package built successfully

The ocaml toolchain should NOT have been built (since c-pkg doesn't depend on it):

  $ test -d _build/.pkgs/default/ocaml/target && echo "OCaml was built (unexpected!)" || echo "OCaml was NOT built (expected - lazy toolchain working)"
  OCaml was NOT built (expected - lazy toolchain working)
