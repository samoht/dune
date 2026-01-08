Demonstrate the translation of filtered dependencies

  $ mkrepo

  $ mkpkg pkg-post <<EOF
  > EOF
  $ mkpkg pkg-build <<EOF
  > EOF
  $ mkpkg pkg-dev <<EOF
  > EOF
  $ mkpkg pkg-test <<EOF
  > EOF

  $ mkpkg bar <<EOF
  > depends: [
  >   "pkg-post" {post}
  >   "pkg-dev" {dev}
  >   "pkg-test" {test}
  >   "pkg-doc" {doc}
  >   "pkg-build" {build}
  > ]
  > EOF

  $ solve --format=directory bar 2>/dev/null
  Solution for dune.lock (3 packages):
  opam:
  - bar.0.0.1
  - pkg-build.0.0.1
  - pkg-post.0.0.1

  $ cat ${default_lock_dir}/bar.0.0.1.pkg
  (version 0.0.1)
  
  (depends
   (all_platforms (pkg-build)))
  
  (post_depends
   (all_platforms (pkg-post)))


