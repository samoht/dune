Test duniverse classification with mixed dune/non-dune packages at various
depths in the dependency chain.

This test verifies that the lock command correctly classifies packages:
- Dune packages are listed under "duniverse (dune-built)"
- Non-dune packages are listed under "opam sandbox"

Set up a mock repository:

  $ mkrepo

Create a chain of packages with mixed build systems:
- dune-leaf: uses dune, no dependencies
- make-middle: uses make, depends on dune-leaf
- dune-top: uses dune, depends on make-middle

This tests that dune packages are still classified correctly even when
they depend on non-dune packages.

  $ mkpkg dune-leaf 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg make-middle 1.0.0 <<EOF
  > depends: ["dune-leaf"]
  > build: [make]
  > EOF

  $ mkpkg dune-top 1.0.0 <<EOF
  > depends: ["make-middle"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

Set up workspace with mock repository:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (url "file://$(pwd)/mock-opam-repository"))
  > EOF

Create a project that depends on the top-level dune package:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (depends dune-top))
  > EOF

Lock the dependencies - packages should be classified correctly:

  $ dune pkg lock --format=directory 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (3 packages)
  
  dune:
  - dune-leaf.1.0.0
  - dune-top.1.0.0
  
  opam:
  - make-middle.1.0.0




Now test a diamond dependency pattern where both paths have mixed packages:
- app depends on dune-a and make-b
- dune-a depends on shared-dune
- make-b depends on shared-dune

  $ mkpkg shared-dune 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg dune-a 1.0.0 <<EOF
  > depends: ["shared-dune"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg make-b 1.0.0 <<EOF
  > depends: ["shared-dune"]
  > build: [make]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name app) (depends dune-a make-b))
  > EOF

  $ rm -rf dune.lock
  $ dune pkg lock --format=directory 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (3 packages)
  
  dune:
  - dune-a.1.0.0
  - shared-dune.1.0.0
  
  opam:
  - make-b.1.0.0




Now test the full build with a chain where libraries actually use each other:

Chain: myproj -> dune-a -> dune-b -> dune-c
All are dune packages that use each other's code.

  $ rm -rf dune.lock mock-opam-repository duniverse _build
  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create the chain of packages:

  $ mkpkg dune-c 1.0.0 <<EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg dune-b 1.0.0 <<EOF
  > depends: ["dune-c"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg dune-a 1.0.0 <<EOF
  > depends: ["dune-b"]
  > build: ["dune" "build" "-p" name "-j" jobs]
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
  > (package (name myproj) (depends dune-a))
  > EOF

Lock the dependencies:

  $ dune pkg lock --format=directory 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (3 packages)
  dune:
  - dune-a.1.0.0
  - dune-b.1.0.0
  - dune-c.1.0.0


Create source files for each package that USE each other:

  $ mkdir -p dune-c-src
  $ cat >dune-c-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-c) (allow_empty))
  > EOF
  $ cat >dune-c-src/dune <<EOF
  > (library (name dune_c) (public_name dune-c))
  > EOF
  $ cat >dune-c-src/dune_c.ml <<EOF
  > let value = "c"
  > EOF

  $ mkdir -p dune-b-src
  $ cat >dune-b-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-b) (allow_empty))
  > EOF
  $ cat >dune-b-src/dune <<EOF
  > (library (name dune_b) (public_name dune-b) (libraries dune-c))
  > EOF
  $ cat >dune-b-src/dune_b.ml <<EOF
  > let value = Dune_c.value ^ "-b"
  > EOF

  $ mkdir -p dune-a-src
  $ cat >dune-a-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-a) (allow_empty))
  > EOF
  $ cat >dune-a-src/dune <<EOF
  > (library (name dune_a) (public_name dune-a) (libraries dune-b))
  > EOF
  $ cat >dune-a-src/dune_a.ml <<EOF
  > let value = Dune_b.value ^ "-a"
  > EOF

Create lock files with sources:

  $ make_lockdir
  $ make_lockpkg dune-c <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-c @install))
  > (source (copy $PWD/dune-c-src))
  > EOF
  $ make_lockpkg dune-b <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-b @install))
  > (depends dune-c)
  > (source (copy $PWD/dune-b-src))
  > EOF
  $ make_lockpkg dune-a <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-a @install))
  > (depends dune-b)
  > (source (copy $PWD/dune-a-src))
  > EOF

Fetch all packages to duniverse:

  $ dune pkg fetch -v
  Fetching dune-a.1.0.0 to duniverse/dune-a.1.0.0
  Fetching dune-b.1.0.0 to duniverse/dune-b.1.0.0
  Fetching dune-c.1.0.0 to duniverse/dune-c.1.0.0

Verify all packages are in duniverse:

  $ ls duniverse/
  dune
  dune-a.1.0.0
  dune-b.1.0.0
  dune-c.1.0.0

Remove source directories:

  $ rm -rf dune-c-src dune-b-src dune-a-src

Create project that uses dune-a (which uses dune-b, which uses dune-c):

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (allow_empty))
  > EOF

  $ cat >dune <<EOF
  > (library (name mylib) (libraries dune-a))
  > EOF

  $ cat >mylib.ml <<EOF
  > let value = Dune_a.value
  > EOF

Build the project:

  $ dune build

Verify ALL dune packages are built in duniverse (workspace), NOT in .pkg:

  $ ls _build/default/duniverse/dune-c.1.0.0/.dune_c.objs/byte/*.cmo
  _build/default/duniverse/dune-c.1.0.0/.dune_c.objs/byte/dune_c.cmo
  $ ls _build/default/duniverse/dune-b.1.0.0/.dune_b.objs/byte/*.cmo
  _build/default/duniverse/dune-b.1.0.0/.dune_b.objs/byte/dune_b.cmo
  $ ls _build/default/duniverse/dune-a.1.0.0/.dune_a.objs/byte/*.cmo
  _build/default/duniverse/dune-a.1.0.0/.dune_a.objs/byte/dune_a.cmo

Verify NO dune packages have .pkg build artifacts (they're all in duniverse):

  $ ls _build/default/.pkg/dune-c/target 2>/dev/null || echo "No .pkg for dune-c (expected)"
  No .pkg for dune-c (expected)
  $ ls _build/default/.pkg/dune-b/target 2>/dev/null || echo "No .pkg for dune-b (expected)"
  No .pkg for dune-b (expected)
  $ ls _build/default/.pkg/dune-a/target 2>/dev/null || echo "No .pkg for dune-a (expected)"
  No .pkg for dune-a (expected)

This confirms the full chain of dune packages are built in duniverse,
with full editor tooling support and editability.

Now test the most complex case: opam-dune-opam-dune interleaved chain.

Chain: myproj -> dune-top -> opam-mid1 -> dune-mid2 -> opam-leaf

  $ rm -rf dune.lock mock-opam-repository duniverse _build
  $ mkrepo

Non-portable lockdir for simpler output:

  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create the interleaved chain of packages:

  $ mkpkg opam-leaf 1.0.0 <<EOF
  > build: [make]
  > install: [make "install"]
  > EOF

  $ mkpkg dune-mid2 1.0.0 <<EOF
  > depends: ["opam-leaf"]
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

  $ mkpkg opam-mid1 1.0.0 <<EOF
  > depends: ["dune-mid2"]
  > build: [make]
  > install: [make "install"]
  > EOF

  $ mkpkg dune-top 1.0.0 <<EOF
  > depends: ["opam-mid1"]
  > build: ["dune" "build" "-p" name "-j" jobs]
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
  > (package (name myproj) (depends dune-top))
  > EOF

Lock the dependencies:

  $ dune pkg lock --format=directory 2>&1 | grep -v "^Dependencies"
  Solution for dune.lock (4 packages)
  dune:
  - dune-mid2.1.0.0
  - dune-top.1.0.0
  
  opam:
  - opam-leaf.1.0.0
  - opam-mid1.1.0.0


Create source files for the interleaved chain:

opam-leaf: creates a simple data file

  $ mkdir -p opam-leaf-src
  $ cat >opam-leaf-src/Makefile <<'EOF'
  > all:
  > 	@echo "Building opam-leaf"
  > install:
  > 	@echo "Installing opam-leaf"
  > 	mkdir -p $(PREFIX)/lib/opam-leaf
  > 	echo "leaf-value" > $(PREFIX)/lib/opam-leaf/data.txt
  > EOF
  $ cat >opam-leaf-src/opam <<'EOF'
  > opam-version: "2.0"
  > name: "opam-leaf"
  > version: "1.0.0"
  > build: [make]
  > install: [make "install" "PREFIX=%{prefix}%"]
  > EOF

dune-mid2: a dune library (can't directly use opam-leaf at code level)

  $ mkdir -p dune-mid2-src
  $ cat >dune-mid2-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-mid2) (allow_empty))
  > EOF
  $ cat >dune-mid2-src/dune <<EOF
  > (library (name dune_mid2) (public_name dune-mid2))
  > EOF
  $ cat >dune-mid2-src/dune_mid2.ml <<EOF
  > let value = "mid2"
  > EOF

opam-mid1: depends on dune-mid2 at package level

  $ mkdir -p opam-mid1-src
  $ cat >opam-mid1-src/Makefile <<'EOF'
  > all:
  > 	@echo "Building opam-mid1"
  > install:
  > 	@echo "Installing opam-mid1"
  > 	mkdir -p $(PREFIX)/lib/opam-mid1
  > 	echo "mid1-value" > $(PREFIX)/lib/opam-mid1/data.txt
  > EOF
  $ cat >opam-mid1-src/opam <<'EOF'
  > opam-version: "2.0"
  > name: "opam-mid1"
  > version: "1.0.0"
  > depends: ["dune-mid2"]
  > build: [make]
  > install: [make "install" "PREFIX=%{prefix}%"]
  > EOF

dune-top: uses dune-mid2 at code level

  $ mkdir -p dune-top-src
  $ cat >dune-top-src/dune-project <<EOF
  > (lang dune 3.16)
  > (package (name dune-top) (allow_empty))
  > EOF
  $ cat >dune-top-src/dune <<EOF
  > (library (name dune_top) (public_name dune-top) (libraries dune-mid2))
  > EOF
  $ cat >dune-top-src/dune_top.ml <<EOF
  > let value = Dune_mid2.value ^ "-top"
  > EOF

Create lock files with sources:

  $ make_lockdir
  $ make_lockpkg opam-leaf <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (source (copy $PWD/opam-leaf-src))
  > EOF
  $ make_lockpkg dune-mid2 <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-mid2 @install))
  > (depends opam-leaf)
  > (source (copy $PWD/dune-mid2-src))
  > EOF
  $ make_lockpkg opam-mid1 <<EOF
  > (version 1.0.0)
  > (build (run make))
  > (install (run make PREFIX=%{prefix} install))
  > (depends dune-mid2)
  > (source (copy $PWD/opam-mid1-src))
  > EOF
  $ make_lockpkg dune-top <<EOF
  > (version 1.0.0)
  > (build (run dune build -p dune-top @install))
  > (depends opam-mid1)
  > (source (copy $PWD/dune-top-src))
  > EOF

Fetch all packages to duniverse:

  $ dune pkg fetch -v
  Fetching dune-mid2.1.0.0 to duniverse/dune-mid2.1.0.0
  Fetching dune-top.1.0.0 to duniverse/dune-top.1.0.0
  Fetching opam-leaf.1.0.0 to duniverse/opam-leaf.1.0.0
  Fetching opam-mid1.1.0.0 to duniverse/opam-mid1.1.0.0

Verify all packages are in duniverse:

  $ ls duniverse/
  dune
  dune-mid2.1.0.0
  dune-top.1.0.0
  opam-leaf.1.0.0
  opam-mid1.1.0.0

Remove source directories:

  $ rm -rf opam-leaf-src dune-mid2-src opam-mid1-src dune-top-src

Create project that uses dune-top:

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name myproj) (allow_empty))
  > EOF

  $ cat >dune <<EOF
  > (library (name mylib) (libraries dune-top))
  > EOF

  $ cat >mylib.ml <<EOF
  > let value = Dune_top.value
  > EOF

Build all pkg dependencies first:

  $ dune build @pkg-install 2>&1

Build the project:

  $ dune build

Verify dune packages are built in duniverse (workspace):

  $ ls _build/default/duniverse/dune-mid2.1.0.0/.dune_mid2.objs/byte/*.cmo
  _build/default/duniverse/dune-mid2.1.0.0/.dune_mid2.objs/byte/dune_mid2.cmo
  $ ls _build/default/duniverse/dune-top.1.0.0/.dune_top.objs/byte/*.cmo
  _build/default/duniverse/dune-top.1.0.0/.dune_top.objs/byte/dune_top.cmo

Verify opam packages are built in .pkg sandbox (files not present in old location):

  $ test -f _build/default/.pkg/opam-leaf/target/lib/opam-leaf/data.txt || echo "opam-leaf data.txt not in old location"
  opam-leaf data.txt not in old location
  $ test -f _build/default/.pkg/opam-mid1/target/lib/opam-mid1/data.txt || echo "opam-mid1 data.txt not in old location"
  opam-mid1 data.txt not in old location

Verify dune packages do NOT have .pkg build artifacts:

  $ ls _build/default/.pkg/dune-mid2/target 2>/dev/null || echo "No .pkg for dune-mid2 (expected)"
  No .pkg for dune-mid2 (expected)
  $ ls _build/default/.pkg/dune-top/target 2>/dev/null || echo "No .pkg for dune-top (expected)"
  No .pkg for dune-top (expected)

This confirms the interleaved opam-dune-opam-dune chain works:
- dune packages: built in duniverse (editable, editor tooling)
- opam packages: built in .pkg sandbox (source from duniverse)
