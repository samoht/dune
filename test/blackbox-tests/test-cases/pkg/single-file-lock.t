Test the single-file lock format (--format=single-file)

  $ mkrepo
  $ add_mock_repo_if_needed

Create a simple package:
  $ mkpkg foo << 'EOF'
  > build: ["echo" "building foo"]
  > EOF

Create a project that depends on foo:
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Lock with single-file format:
  $ dune pkg lock --format=single-file 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1

Check that dune.lock is a file (not a directory):
  $ test -f dune.lock && echo "dune.lock is a file"
  dune.lock is a file

Check the contents of the single-file lock:
  $ cat dune.lock
  (lang package 0.1)
  
  (repos)
  
  (packages foo.0.0.1)
  
  (platforms linux macos)





Lock again with directory format (default):
  $ rm dune.lock
  $ dune pkg lock --format=directory 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1

Check that dune.lock is a directory:
  $ test -d dune.lock && echo "dune.lock is a directory"
  dune.lock is a directory
