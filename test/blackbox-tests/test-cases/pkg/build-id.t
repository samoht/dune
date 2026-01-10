Tests for package build-id computation and storage.

Build IDs are computed at lock time and stored in .pkg files. They form a
Merkle tree where each package's build_id = hash(pkg_content, deps' build_ids, platforms).

Setup a mock opam repository with some packages:

  $ mkrepo
  $ mkpkg foo <<EOF
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF
  $ mkpkg baz <<EOF
  > depends: [ "bar" ]
  > EOF

Create a project that depends on baz (which transitively depends on bar and foo).

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends baz))
  > EOF

  $ add_mock_repo_if_needed

Solve and check that build_id is stored in the .pkg files.

  $ dune_pkg_lock_normalized
  Solution for dune.lock (3 packages):
  opam:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1

Check that each package has a build_id field:

  $ grep build_id dune.lock/*.pkg
  dune.lock/bar.0.0.1.pkg:(build_id c9968762c541b6ce79781be30e7b2826)
  dune.lock/baz.0.0.1.pkg:(build_id 07880970fac78b92ce546681a4be2f46)
  dune.lock/foo.0.0.1.pkg:(build_id 386979f1769f82fc34ca1a9323aa68c8)

Save the build_ids for comparison:

  $ grep build_id dune.lock/foo.0.0.1.pkg > foo_id1.txt
  $ grep build_id dune.lock/bar.0.0.1.pkg > bar_id1.txt
  $ grep build_id dune.lock/baz.0.0.1.pkg > baz_id1.txt

Now change foo's content (add a build command) and re-lock:

  $ mkpkg foo <<EOF
  > build: [ "echo" "hello" ]
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock (3 packages):
  opam:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1

  $ grep build_id dune.lock/foo.0.0.1.pkg > foo_id2.txt
  $ grep build_id dune.lock/bar.0.0.1.pkg > bar_id2.txt
  $ grep build_id dune.lock/baz.0.0.1.pkg > baz_id2.txt

foo's build_id should change because its content changed:

  $ diff foo_id1.txt foo_id2.txt > /dev/null && echo "same" || echo "different"
  different

bar's build_id should change because foo (its dependency) changed:

  $ diff bar_id1.txt bar_id2.txt > /dev/null && echo "same" || echo "different"
  different

baz's build_id should change because bar (its dependency) changed:

  $ diff baz_id1.txt baz_id2.txt > /dev/null && echo "same" || echo "different"
  different

Now add a new independent package qux (no deps on foo/bar/baz):

  $ mkpkg qux <<EOF
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends baz qux))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock (4 packages):
  opam:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1
  - qux.0.0.1

  $ grep build_id dune.lock/foo.0.0.1.pkg > foo_id3.txt
  $ grep build_id dune.lock/bar.0.0.1.pkg > bar_id3.txt
  $ grep build_id dune.lock/baz.0.0.1.pkg > baz_id3.txt

foo/bar/baz build_ids should NOT change because qux is independent:

  $ diff foo_id2.txt foo_id3.txt > /dev/null && echo "same" || echo "different"
  same
  $ diff bar_id2.txt bar_id3.txt > /dev/null && echo "same" || echo "different"
  same
  $ diff baz_id2.txt baz_id3.txt > /dev/null && echo "same" || echo "different"
  same

Test that packages with the same content but different versions have different build_ids:

  $ mkpkg foo 0.0.2 <<EOF
  > build: [ "echo" "hello" ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends (foo (= 0.0.2))))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.2

  $ grep build_id dune.lock/foo.0.0.2.pkg > foo_v2_id.txt

Version is part of Pkg.Info and included in digest, so different version = different build_id:

  $ diff foo_id2.txt foo_v2_id.txt > /dev/null && echo "same" || echo "different"
  different

Test that build_id is reproducible (re-locking with same inputs gives same build_id):

  $ dune_pkg_lock_normalized
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.2

  $ grep build_id dune.lock/foo.0.0.2.pkg > foo_v2_id2.txt
  $ diff foo_v2_id.txt foo_v2_id2.txt > /dev/null && echo "same" || echo "different"
  same

Test that platform-independent packages have the SAME build_id across platforms.
This is correct behavior - the package can be cached and reused since its content
is identical regardless of platform.

Create a fresh package plat for this test:

  $ mkpkg plat <<EOF
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends plat))
  > EOF

Lock for linux/x86_64:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os linux)
  >   (arch x86_64)))
  > (repository
  >  (name mock)
  >  (url "git+file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock --format=directory 2>&1 | grep -E '^(Solution|opam:|-)' | head -3
  Solution for dune.lock (1 package)
  opam:
  - plat.0.0.1

  $ grep build_id dune.lock/plat.0.0.1.pkg > plat_linux.txt

Lock for macos/arm64:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os macos)
  >   (arch arm64)))
  > (repository
  >  (name mock)
  >  (url "git+file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock --format=directory 2>&1 | grep -E '^(Solution|opam:|-)' | head -3
  Solution for dune.lock (1 package)
  opam:
  - plat.0.0.1

  $ grep build_id dune.lock/plat.0.0.1.pkg > plat_macos.txt

Platform-independent packages have the same build_id across platforms
(correct - can be cached and reused):

  $ diff plat_linux.txt plat_macos.txt > /dev/null && echo "same" || echo "different"
  same

Test that platform-DEPENDENT packages have DIFFERENT build_ids on different
platforms. This tests that enabled_on_platforms is correctly included in the hash.

Create a package only available on linux:

  $ mkpkg linonly <<EOF
  > available: [ os = "linux" ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends linonly))
  > EOF

Lock for linux (linonly is available):

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os linux)
  >   (arch x86_64)))
  > (repository
  >  (name mock)
  >  (url "git+file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock --format=directory 2>&1 | grep -E '^(Solution|opam:|-|Platforms)' | head -10
  Solution for dune.lock (1 package)
  opam:
  - linonly.0.0.1
  Platforms with no solution:
  - arch = arm64; os = macos
  - arch = x86_64; os = macos

  $ grep build_id dune.lock/linonly.0.0.1.pkg > linonly_linux.txt

Now create a package only available on macos:

  $ mkpkg maconly <<EOF
  > available: [ os = "macos" ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (depends maconly))
  > EOF

Lock for macos (maconly is available):

  $ cat > dune-workspace <<EOF
  > (lang dune 3.20)
  > (lock_dir
  >  (path dune.lock)
  >  (repositories mock)
  >  (solver_env
  >   (os macos)
  >   (arch arm64)))
  > (repository
  >  (name mock)
  >  (url "git+file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock --format=directory 2>&1 | grep -E '^(Solution|opam:|-|Platforms)' | head -10
  Solution for dune.lock (1 package)
  opam:
  - maconly.0.0.1
  Platforms with no solution:
  - arch = arm64; os = linux
  - arch = x86_64; os = linux

  $ grep build_id dune.lock/maconly.0.0.1.pkg > maconly_macos.txt

Platform-dependent packages should have different build_ids because they have
different enabled_on_platforms values:

  $ diff linonly_linux.txt maconly_macos.txt > /dev/null && echo "same" || echo "different"
  different
