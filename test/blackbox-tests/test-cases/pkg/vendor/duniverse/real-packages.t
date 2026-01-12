Test classification of real-world packages to verify dune packages go to duniverse.

Many popular OCaml packages use dune. Let's verify they're correctly classified.

  $ mkrepo
  $ add_mock_repo_if_needed

Create mock packages that mimic real-world build patterns:

fmt (modern versions use dune):
  $ mkpkg fmt 0.9.0 << EOF
  > build: [
  >   ["dune" "subst"] {dev}
  >   ["dune" "build" "-p" name "-j" jobs]
  > ]
  > EOF

cmdliner (uses dune):
  $ mkpkg cmdliner 1.2.0 << EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

logs (uses dune):
  $ mkpkg logs 0.7.0 << EOF
  > build: ["dune" "build" "-p" name "-j" jobs]
  > EOF

An older package using topkg (non-dune):
  $ mkpkg old-pkg 1.0.0 << EOF
  > build: [
  >   ["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]
  > ]
  > EOF

A package using make (non-dune):
  $ mkpkg make-pkg 1.0.0 << EOF
  > build: [make]
  > EOF

Create a project that depends on all of them:

  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package (name myapp) (depends fmt cmdliner logs old-pkg make-pkg))
  > EOF

Lock and check classification:

  $ dune pkg lock 2>&1 | grep -E "^(dune:|opam:|- )" | head -20
  dune:
  - cmdliner.1.2.0
  - fmt.0.9.0
  - logs.0.7.0
  opam:
  - make-pkg.1.0.0
  - old-pkg.1.0.0

This shows:
- fmt, cmdliner, logs → "dune:" (will go to duniverse, editable)
- old-pkg, make-pkg → "opam:" (will go to .pkg sandbox)
