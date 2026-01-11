Solving would add opam 'depext' field to lock directory packages

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package for the library with depexts:
  $ mkpkg foo <<EOF
  > depexts: [["unzip" "gnupg"]]
  > EOF

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

With single-file format, depexts are derived from opam repo:
  $ dune_pkg_lock_normalized --format=file
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1
  $ cat dune.lock | strip_sandbox | dune_cmd subst '[a-f0-9]{40}' 'COMMIT_HASH' | grep -v "^$" | head -10
  (lang package 0.1)
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/depexts/mock-opam-repository
    COMMIT_HASH))
  (packages foo.0.0.1)
  (platforms
   (linux
    (arch arm64 x86_64))
   (macos
    (arch arm64 x86_64)))




With directory format, the opam 'depext' field is stored in foo.pkg:
  $ rm -rf dune.lock
  $ dune_pkg_lock_normalized --format=directory
  Solution for dune.lock (1 package):
  opam:
  - foo.0.0.1
  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   (unzip gnupg))
  
  (build_id 0b88bf484185391c75e1c82fce8ff8b5)

