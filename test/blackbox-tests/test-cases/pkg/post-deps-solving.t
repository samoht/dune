Solving for post dependencies:

  $ mkrepo

  $ mkpkg bar

  $ barfile="bar.file"
  $ bardir="$mock_packages/bar/bar.0.0.1"
  $ mkdir -p $bardir/files/dir
  $ cat >$bardir/files/$barfile <<EOF
  > foo patch
  > EOF

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

We don't need bar, so we skip it

  $ solve foo
  Solution for dune.lock (2 packages):
  opam:
  - bar.0.0.1
  - foo.0.0.1

Verify foo has post_depends on bar:
  $ grep -A1 "foo.0.0.1" dune.lock | grep post_depends
   (post_depends (all_platforms (bar))))

Self dependency

  $ mkpkg foo <<EOF
  > depends: [ "foo" {post} ]
  > EOF

  $ solve foo
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1

Verify foo has post_depends on itself:
  $ grep -A1 "foo.0.0.1" dune.lock | grep post_depends
   (post_depends (all_platforms (foo))))

Using post to break cycle:

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF

  $ solve bar
  Solution for dune.lock (2 packages):
  opam:
  - bar.0.0.1
  - foo.0.0.1

Verify foo has post_depends on bar, and bar depends on foo:
  $ grep -E "post_depends|depends" dune.lock
   (depends (all_platforms (foo))))
   (post_depends (all_platforms (bar))))

post "cycle":

  $ mkpkg foo <<EOF
  > depends: [ "bar" {post} ]
  > EOF

  $ mkpkg bar <<EOF
  > depends: [ "foo" {post} ]
  > EOF

  $ solve foo
  Solution for dune.lock (2 packages):
  opam:
  - bar.0.0.1
  - foo.0.0.1

Verify both have post_depends:
  $ grep post_depends dune.lock
   (post_depends (all_platforms (foo))))
   (post_depends (all_platforms (bar))))

In depopts:

  $ mkpkg foo <<EOF
  > depopts: [ "bar" {post} ]
  > EOF

  $ mkpkg bar

  $ solve foo
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1
