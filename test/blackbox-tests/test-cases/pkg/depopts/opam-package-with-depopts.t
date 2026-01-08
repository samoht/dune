We test how opam files with depopts fields are translated into dune.lock files:

  $ mkrepo

Make a package with a depopts field
  $ mkpkg with-depopts <<'EOF'
  > depopts: [ "foo" ]
  > EOF
  $ mkpkg foo

  $ solve with-depopts
  Solution for dune.lock (1 package):
  opam:
  - with-depopts.0.0.1

The package is included in the lock file:

  $ grep with-depopts dune.lock
  (packages with-depopts.0.0.1)

Depopts should not be selected if they conflict with other constraints:

  $ mkpkg no-foo <<'EOF'
  > depends: [ "with-depopts" ]
  > conflicts: [ "foo" ]
  > EOF

  $ solve no-foo
  Solution for dune.lock (2 packages):
  opam:
  - no-foo.0.0.1
  - with-depopts.0.0.1

  $ dune pkg validate-lockdir
