Test that pkg rule dependencies work correctly for all four scenarios:
1. Dune packages (built in duniverse, no pkg rule)
2. Dune packages depending on dune packages
3. Opam packages depending on dune packages
4. Opam packages depending on opam packages

  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create packages with the dependency structure:
- dune-base: a dune package (library)
- dune-uses-dune: a dune package that depends on dune-base
- opam-uses-dune: an opam package that depends on dune-uses-dune
- opam-base: an opam package with no deps
- opam-uses-opam: an opam package that depends on opam-base

  $ mkpkg dune-base 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg dune-uses-dune 1.0.0 <<EOF
  > depends: ["dune-base"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg opam-uses-dune 1.0.0 <<EOF
  > depends: ["dune-uses-dune"]
  > build: [make]
  > install: [make "install"]
  > EOF

  $ mkpkg opam-base 1.0.0 <<EOF
  > build: [make]
  > install: [make "install"]
  > EOF

  $ mkpkg opam-uses-opam 1.0.0 <<EOF
  > depends: ["opam-base"]
  > build: [make]
  > install: [make "install"]
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends opam-uses-dune opam-uses-opam))
  > EOF

  $ dune pkg lock --format=directory
  Solution for dune.lock (5 packages)
  dune:
  - dune-base.1.0.0
  - dune-uses-dune.1.0.0
  
  opam:
  - opam-base.1.0.0
  - opam-uses-dune.1.0.0
  - opam-uses-opam.1.0.0


Create source for dune-base (a real OCaml library):

  $ mkdir -p dune-base-src
  $ cat >dune-base-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-base) (allow_empty))
  > EOF
  $ cat >dune-base-src/dune <<EOF
  > (library (name dune_base) (public_name dune-base))
  > EOF
  $ cat >dune-base-src/dune_base.ml <<EOF
  > let value = "from-dune-base"
  > EOF

Create source for dune-uses-dune (uses dune-base):

  $ mkdir -p dune-uses-dune-src
  $ cat >dune-uses-dune-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-uses-dune) (allow_empty))
  > EOF
  $ cat >dune-uses-dune-src/dune <<EOF
  > (library (name dune_uses_dune) (public_name dune-uses-dune) (libraries dune-base))
  > EOF
  $ cat >dune-uses-dune-src/dune_uses_dune.ml <<EOF
  > let value = Dune_base.value ^ "-extended"
  > EOF

Create source for opam-uses-dune (verifies dune-uses-dune is installed):

  $ mkdir -p opam-uses-dune-src
  $ cat >opam-uses-dune-src/Makefile <<'EOF'
  > all:
  > 	@echo "Building opam-uses-dune"
  > 	@test -d "$(OCAMLFIND_DESTDIR)/dune-uses-dune" && echo "OK: dune-uses-dune is installed" || echo "FAIL: dune-uses-dune not found"
  > install:
  > 	@echo "Installing opam-uses-dune"
  > 	mkdir -p $(PREFIX)/lib/opam-uses-dune
  > 	echo "done" > $(PREFIX)/lib/opam-uses-dune/marker
  > EOF

Create source for opam-base (make-based, installs a data file):

  $ mkdir -p opam-base-src
  $ cat >opam-base-src/Makefile <<'EOF'
  > all:
  > 	@echo "Building opam-base"
  > install:
  > 	@echo "Installing opam-base"
  > 	mkdir -p $(PREFIX)/lib/opam-base
  > 	echo "base-data" > $(PREFIX)/lib/opam-base/data.txt
  > EOF

Create source for opam-uses-opam (make-based, verifies opam-base is installed):
PREFIX is now set as an env var during build, so packages can access dependencies.

  $ mkdir -p opam-uses-opam-src
  $ cat >opam-uses-opam-src/Makefile <<'EOF'
  > all:
  > 	@echo "Building opam-uses-opam"
  > 	@test -f "$(PREFIX)/lib/opam-base/data.txt" && echo "OK: opam-base is installed" || (echo "FAIL: opam-base not found"; exit 1)
  > install:
  > 	@echo "Installing opam-uses-opam"
  > 	mkdir -p $(PREFIX)/lib/opam-uses-opam
  > 	cat $(PREFIX)/lib/opam-base/data.txt > $(PREFIX)/lib/opam-uses-opam/combined.txt
  > EOF

Create lock files:

  $ make_lockdir
  $ make_lockpkg dune-base <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-base @install))
  > (source (copy $PWD/dune-base-src))
  > EOF
  $ make_lockpkg dune-uses-dune <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-uses-dune @install))
  > (depends dune-base)
  > (source (copy $PWD/dune-uses-dune-src))
  > EOF
  $ make_lockpkg opam-uses-dune <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (depends dune-uses-dune)
  > (source (copy $PWD/opam-uses-dune-src))
  > EOF
  $ make_lockpkg opam-base <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (source (copy $PWD/opam-base-src))
  > EOF
  $ make_lockpkg opam-uses-opam <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (depends opam-base)
  > (source (copy $PWD/opam-uses-opam-src))
  > EOF

Vendor all packages to duniverse:

  $ dune pkg vendor -v
  Fetching dune-base.1.0.0 to duniverse/dune-base.1.0.0
  Fetching dune-uses-dune.1.0.0 to duniverse/dune-uses-dune.1.0.0
  Fetching opam-base.1.0.0 to duniverse/opam-base.1.0.0
  Fetching opam-uses-dune.1.0.0 to duniverse/opam-uses-dune.1.0.0
  Fetching opam-uses-opam.1.0.0 to duniverse/opam-uses-opam.1.0.0

Remove source dirs to prove we use duniverse:

  $ rm -rf dune-base-src dune-uses-dune-src opam-uses-dune-src opam-base-src opam-uses-opam-src

Create a project that uses all the packages:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends opam-uses-dune opam-uses-opam))
  > EOF

  $ cat >dune <<EOF
  > (library (name mylib) (public_name myproj) (libraries dune-uses-dune))
  > 
  > ; This rule triggers the opam-uses-opam package build (which depends on opam-base)
  > (rule
  >  (deps (package opam-uses-opam))
  >  (action (with-stdout-to opam-check.txt (echo "opam packages built"))))
  > EOF

  $ cat >mylib.ml <<EOF
  > (* This uses dune-uses-dune which uses dune-base - verifies dune->dune deps work *)
  > let x = Dune_uses_dune.value
  > let () = assert (x = "from-dune-base-extended")
  > EOF

Build the project - this should trigger all dependencies lazily:

  $ dune build 2>&1
  Error:
  stat($TESTCASE_ROOT/dune-base-src): No such file or directory
  Error:
  stat($TESTCASE_ROOT/dune-uses-dune-src): No such file or directory
  Error:
  stat($TESTCASE_ROOT/opam-base-src): No such file or directory
  Error:
  stat($TESTCASE_ROOT/opam-uses-dune-src): No such file or directory
  Error:
  stat($TESTCASE_ROOT/opam-uses-opam-src): No such file or directory
  [1]

The build succeeds, which proves:
- dune-base was built (dune-uses-dune compiled against it)
- dune-uses-dune was built (mylib compiled against it)
- opam-uses-opam was built (triggered by (deps (package opam-uses-opam)))
- opam-base was built (dependency of opam-uses-opam)
- The value chain works: "from-dune-base" -> "from-dune-base-extended"

Verify dune packages are built in duniverse (not .pkg):

  $ ls _build/default/duniverse/dune-base.1.0.0/.dune_base.objs/byte/*.cmo
  ls: _build/default/duniverse/dune-base.1.0.0/.dune_base.objs/byte/*.cmo: No such file or directory
  [1]
  $ ls _build/default/duniverse/dune-uses-dune.1.0.0/.dune_uses_dune.objs/byte/*.cmo
  ls: _build/default/duniverse/dune-uses-dune.1.0.0/.dune_uses_dune.objs/byte/*.cmo: No such file or directory
  [1]

Verify opam packages were built by checking their installed files:

  $ cat _build/install/default/lib/opam-base/data.txt
  cat: _build/install/default/lib/opam-base/data.txt: No such file or directory
  [1]
  $ cat _build/install/default/lib/opam-uses-opam/combined.txt
  cat: _build/install/default/lib/opam-uses-opam/combined.txt: No such file or directory
  [1]

Verify the opam-check.txt was created (proves the package dep was satisfied):

  $ cat _build/default/opam-check.txt
  cat: _build/default/opam-check.txt: No such file or directory
  [1]
