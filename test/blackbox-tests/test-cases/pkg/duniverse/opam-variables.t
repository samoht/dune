Test opam variable expansion in vendor packages.

  $ mkrepo
  $ export DUNE_CONFIG__PORTABLE_LOCK_DIR=disabled

Create a package that tests various opam variable expansions:

  $ mkdir -p pkg-source
  $ cat >pkg-source/Makefile <<'EOF'
  > all:
  > 	@echo "Building"
  > install:
  > 	@echo "Installing with PREFIX=$(PREFIX)"
  > 	mkdir -p $(PREFIX)/lib/var-pkg
  > 	echo "name=$(NAME)" > $(PREFIX)/lib/var-pkg/vars.txt
  > 	echo "version=$(VERSION)" >> $(PREFIX)/lib/var-pkg/vars.txt
  > 	echo "prefix=$(PREFIX)" >> $(PREFIX)/lib/var-pkg/vars.txt
  > 	echo "lib=$(LIB)" >> $(PREFIX)/lib/var-pkg/vars.txt
  > EOF
  $ cat >pkg-source/opam <<'EOF'
  > opam-version: "2.0"
  > name: "var-pkg"
  > version: "1.2.3"
  > build: [make]
  > install: [make "install" "NAME=%{name}%" "VERSION=%{version}%" "PREFIX=%{prefix}%" "LIB=%{lib}%"]
  > EOF

Set up workspace:

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
  > (package (name myproj) (depends var-pkg))
  > EOF

Create lock file:

  $ make_lockdir
  $ make_lockpkg var-pkg <<EOF
  > (version 1.2.3)
  > (build (run make))
  > (install (run make install "NAME=%{name}" "VERSION=%{version}" "PREFIX=%{prefix}" "LIB=%{lib}"))
  > (source (copy $PWD/pkg-source))
  > EOF

Fetch the package:

  $ dune pkg vendor -v 2>&1
     Vendoring var-pkg.1.2.3

Verify the vendor stanza was created:

  $ ls duniverse/var-pkg.1.2.3/
  Makefile
  opam
  $ cat duniverse/dune
  ; This directory is managed by dune pkg
  (vendored_dirs *)
  (vendor var-pkg.1.2.3 (mode opam) (libraries var-pkg))

Build:

  $ dune build @pkg-install 2>&1

Check cookie was created (inside target/ directory):

  $ test -f _build/.pkgs/default/var-pkg.1.2.3/target/cookie && echo "cookie exists"
  [1]

Check that variables were expanded correctly.
Note: For packages with install actions, %{prefix}% expands to target_dir (for caching).
The PREFIX env var points to target_dir but recorded paths use the expanded %{prefix}% value.
Files are written to target_dir then copied to shared prefix by the copy_to_prefix rule.

  $ cat _build/install/default/lib/var-pkg/vars.txt | head -2
  name=var-pkg
  version=1.2.3

The prefix and lib paths point to target_dir (inside sandbox) for caching:

  $ cat _build/install/default/lib/var-pkg/vars.txt | grep prefix= | grep -q "target$" && echo "prefix ends with target"
  prefix ends with target
  $ cat _build/install/default/lib/var-pkg/vars.txt | grep lib= | grep -q "target/lib$" && echo "lib ends with target/lib"
  lib ends with target/lib
