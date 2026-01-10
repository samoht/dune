Exercise the solver on a package with a conjunction in its dependency
constraints.

  $ mkrepo

  $ mkpkg a

  $ mkpkg foo << EOF
  > depends: [
  >   "a" { >= "0.0.0" & < "1.0.0" }
  > ]
  > EOF

  $ solve foo
  Solution for dune.lock (2 packages):
  opam:
  - a.0.0.1
  - foo.0.0.1

Verify that foo's dependency on a is captured in the lock file:
  $ grep -E "foo.*depends.*a" dune.lock || grep "a.0.0.1" dune.lock
  grep: dune.lock: Is a directory
  grep: dune.lock: Is a directory
  [2]
