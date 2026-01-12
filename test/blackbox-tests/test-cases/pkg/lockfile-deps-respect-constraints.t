When populating the "deps" field of a lockfile, only packages which have locked
versions compatible with the lockfile's package's dependency version
constraints should be included.

  $ mkrepo

  $ mkpkg a 0.0.1
  $ mkpkg a 0.0.2
  $ mkpkg b

The package "c" can be satisfied by b or by a.0.0.1.
  $ mkpkg c <<EOF
  > depends: [ "b" | "a" {= "0.0.1"} ]
  > EOF

The package "d" can only be satisfied by "a.0.0.2" which will force the package
"c" to depend on "b" rather than "a" and ensure "a.0.0.2" is in the solution
rather than "a.0.0.1".
  $ mkpkg d <<EOF
  > depends: [ "a" {= "0.0.2"} ]
  > EOF

  $ solve_project --format=directory <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (depends c d))
  > EOF
  Solution for dune.lock (4 packages):
  opam:
  - a.0.0.2
  - b.0.0.1
  - c.0.0.1
  - d.0.0.1

Confirm that we locked "a.0.0.2".
  $ cat ${default_lock_dir}/a.0.0.2.pkg
  (version 0.0.2)
  
  (build_id 272b60136235bc06f3ee753e9f48893e)

The deps in the lockfile for "c" shouldn't contain "a" since the only version
of "a" that "c" could depend on is "a.0.0.1" which isn't part of the solution.
  $ cat ${default_lock_dir}/c.0.0.1.pkg
  (version 0.0.1)
  
  (depends
   (all_platforms (b)))
  
  (build_id f24ddffefaf3456b55f390ee684eb769)
