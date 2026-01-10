Test opam's var?default syntax and version lookup for non-dependencies:

The syntax %{pkg:name:var?default} means "if var is truthy, return default, else empty".
This is used by relocatable-compiler to detect which compiler variant is installed.

  $ make_lockdir

Create two packages - only one will be a dependency:

  $ make_lockpkg compiler-a <<EOF
  > (version 1.2.3)
  > (build (run mkdir -p %{prefix}))
  > EOF

  $ make_lockpkg compiler-b <<EOF
  > (version 4.5.6)
  > (build (run mkdir -p %{prefix}))
  > EOF

Create a package that uses the conditional syntax and version lookup:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (depends compiler-a)
  > (build
  >  (progn
  >   (system "echo installed?a: '%{pkg:compiler-a:installed?a-is-here}'")
  >   (system "echo installed?b: '%{pkg:compiler-b:installed?b-is-here}'")
  >   (system "echo version-a: '%{pkg:compiler-a:version}'")
  >   (system "echo version-b: '%{pkg:compiler-b:version}'")
  >   (system "echo combined: '%{pkg:compiler-a:installed?a}%{pkg:compiler-b:installed?b}'")
  >   (run mkdir -p %{prefix})))
  > EOF

Build and check outputs - compiler-a is a dependency so installed is true,
compiler-b is in lock but not a dependency so installed should also be true:

  $ build_pkg test
  installed?a: a-is-here
  installed?b: b-is-here
  version-a: 1.2.3
  version-b: 4.5.6
  combined: ab

Test with a package that doesn't exist in the lock at all:

  $ make_lockpkg test2 <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "echo installed: %{pkg:nonexistent:installed}")
  >   (system "echo installed?x: %{pkg:nonexistent:installed?was-installed}")
  >   (system "echo version: %{pkg:nonexistent:version}")
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg test2
  installed: false
  installed?x:
  version:
