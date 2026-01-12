Demonstrate the build command we construct for different types of projects:

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir _template _dune-only _mixed _opam-only

  $ cat >_template/dune-project <<EOF
  > (lang dune 3.13)
  > (generate_opam_files true)
  > (package (name template)  (allow_empty))
  > EOF
  $ cat >_template/mixed.opam.template <<EOF
  > build: [ "echo" "template" ]
  > EOF

  $ cat >_dune-only/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name dune-only)  (allow_empty))
  > EOF

  $ cat >_mixed/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >_mixed/mixed.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "mixed" ]
  > EOF

  $ cat > _opam-only/opam-only.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "opam only" ]
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "$PWD/_template")
  >  (package (name template)))
  > (pin
  >  (url "$PWD/_dune-only")
  >  (package (name dune-only)))
  > (pin
  >  (url "$PWD/_mixed")
  >  (package (name mixed)))
  > (pin
  >  (url "$PWD/_opam-only")
  >  (package (name opam-only)))
  > (package
  >  (name main)
  >  (allow_empty)
  >  (depends dune-only mixed template opam-only))
  > EOF

  $ dune_pkg_lock_normalized --format=directory
  Solution for dune.lock (4 packages):
  dune:
  - dune-only.dev
  - template.dev
  
  opam:
  - mixed.dev
  - opam-only.dev
  $ build_command() {
  > grep "$1" "${default_lock_dir}/$2.dev.pkg"
  > }
  $ build_command "(dune)" dune-only
   (all_platforms ((dune))))
  $ build_command "(dune)" template
   (all_platforms ((dune))))
  $ build_command "(build" mixed
  (build
  (build_id 889b2b69eadcd957a1e5400753c9c7b6)
  $ build_command "(build" opam-only
  (build
  (build_id c15dfc97bd2c1c73b94d6b5502746997)

If we build the deps, everything works fine and we see the output of the opam
pins:
  $ dune build @pkg-install
  mixed
  opam only
