Test that .install files are tracked in target/ for package management.

This test verifies the design from doc/dev/tool-caching.md:
1. Package builds generate <pkg>.install in source/
2. The .install file is copied to target/<pkg>.install
3. This allows tracking which files each package installed

  $ mkrepo

Create a package that generates a .install file (dune packages do this automatically).

  $ mkdir test-pkg
  $ cat > test-pkg/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name test-pkg))
  > EOF
  $ cat > test-pkg/main.ml <<EOF
  > let () = print_endline "Hello"
  > EOF
  $ cat > test-pkg/dune <<EOF
  > (executable (public_name test-pkg))
  > EOF
  $ tar cf test-pkg.tar test-pkg
  $ rm -rf test-pkg

Create opam package that builds with dune (generates .install file).

  $ mkpkg test-pkg <<EOF
  > build: [["dune" "build" "-p" name "@install"]]
  > url {
  >   src: "file://$PWD/test-pkg.tar"
  >   checksum: ["md5=$(md5sum test-pkg.tar | cut -f1 -d' ')"]
  > }
  > EOF

Set up project.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name myproject) (depends test-pkg))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir (repositories mock))
  > (repository (name mock) (url "file://$PWD/mock-opam-repository"))
  > EOF

Lock and build.

  $ dune pkg lock --format=directory 2>&1 | head -5
  Solution for dune.lock (1 package)

  Dependencies common to all supported platforms:
  dune:
  - test-pkg.0.0.1

  $ dune build @install 2>&1 | grep -v "Entering\|Leaving" || true

Check that the package was built and .install file exists in target/.

  $ show_pkg test-pkg

  /source
  /source/test-pkg
  /source/test-pkg/_build
  /source/test-pkg/_build/.dune
  /source/test-pkg/_build/.dune/dune-project-digest-db
  /source/test-pkg/_build/default
  /source/test-pkg/_build/default/.dune
  /source/test-pkg/_build/default/.dune/configurator.v2
  /source/test-pkg/_build/default/main.exe
  /source/test-pkg/_build/default/main.ml
  /source/test-pkg/_build/default/test-pkg.install
  /source/test-pkg/dune
  /source/test-pkg/dune-project
  /source/test-pkg/main.ml
  /target
  /target/cookie
  /target/test-pkg.install

The .install file in target/ lists what this package installed.

  $ cat "$(get_build_pkg_dir test-pkg)/target/test-pkg.install"
  bin: [
    "_build/default/main.exe" {"test-pkg"}
  ]

The binary was installed to the shared install prefix.

  $ ls _build/install/default/bin
  test-pkg
