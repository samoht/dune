Selecting depopts

  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar

  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo)
  >  (depopts bar))
  > EOF
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1
