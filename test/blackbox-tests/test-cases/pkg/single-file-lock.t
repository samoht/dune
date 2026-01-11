Test the single-file lock format (--format=file)

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
  $ dune pkg lock --format=file 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


Check that dune.lock is a file (not a directory):
  $ test -f dune.lock && echo "dune.lock is a file"
  dune.lock is a file

Check the contents of the single-file lock:
  $ cat dune.lock | strip_sandbox | sed 's/[a-f0-9]\{40\}/HASH/'
  (lang package 0.1)
  
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository
    HASH))
  
  (packages foo.0.0.1)
  
  (platforms
   (linux
    (arch arm64 x86_64))
   (macos
    (arch arm64 x86_64)))







Lock with directory format (explicit):
  $ rm dune.lock
  $ dune pkg lock --format=directory 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


Check that dune.lock is a directory:
  $ test -d dune.lock && echo "dune.lock is a directory"
  dune.lock is a directory

Test pins in single-file lock format:
  $ rm -rf dune.lock
  $ mkdir -p _my_pin
  $ cat > _my_pin/my-pinned-pkg.opam << EOF
  > opam-version: "2.0"
  > build: ["echo" "building pinned"]
  > EOF
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_my_pin")
  >  (package
  >   (name my-pinned-pkg)))
  > (package
  >  (name bar)
  >  (depends my-pinned-pkg))
  > EOF

Lock with single-file format (expect pins section):
  $ dune pkg lock --format=file 2>&1 | head -10
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - my-pinned-pkg.dev



Check the single-file lock contents include pins:
  $ cat dune.lock | strip_sandbox | sed 's/[a-f0-9]\{40\}/HASH/'
  (lang package 0.1)
  
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository
    HASH))
  
  (packages my-pinned-pkg.dev)
  
  (platforms
   (linux
    (arch arm64 x86_64))
   (macos
    (arch arm64 x86_64)))



Test --platform CLI option:
  $ rm -rf dune.lock _my_pin
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name myapp)
  >  (depends foo))
  > EOF

Lock with specific platform:
  $ dune pkg lock --format=file --platform=linux 2>&1 | head -10
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1



Check lock has linux platform only:
  $ cat dune.lock | strip_sandbox | sed 's/[a-f0-9]\{40\}/HASH/'
  (lang package 0.1)
  
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository
    HASH))
  
  (packages foo.0.0.1)
  
  (platforms
   (linux
    (arch arm64 x86_64)))







Lock with platform including archs:
  $ rm dune.lock
  $ dune pkg lock --format=file --platform=linux-x86_64 2>&1 | head -10
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1



Check lock has linux with only x86_64 arch:
  $ cat dune.lock | strip_sandbox | sed 's/[a-f0-9]\{40\}/HASH/'
  (lang package 0.1)
  
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository
    HASH))
  
  (packages foo.0.0.1)
  
  (platforms
   (linux
    (arch x86_64)))







Lock with multiple platforms:
  $ rm dune.lock
  $ dune pkg lock --format=file --platform=linux --platform=macos-arm64 2>&1 | head -10
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


Check lock has both platforms:
  $ cat dune.lock | strip_sandbox | sed 's/[a-f0-9]\{40\}/HASH/'
  (lang package 0.1)
  
  (repos
   ($SANDBOX/default/test/blackbox-tests/test-cases/pkg/mock-opam-repository
    HASH))
  
  (packages foo.0.0.1)
  
  (platforms
   (linux
    (arch arm64 x86_64))
   (macos
    (arch arm64)))




Test switching between formats (single-file to directory):
First create a single-file lock:
  $ rm -rf dune.lock
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package
  >  (name switch-test)
  >  (depends foo))
  > EOF
  $ dune pkg lock --format=file 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


  $ test -f dune.lock && echo "dune.lock is a file"
  dune.lock is a file

Now switch to directory format (should work, replacing the file):
  $ dune pkg lock --format=directory 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


  $ test -d dune.lock && echo "dune.lock is now a directory"
  dune.lock is now a directory

And switch back to single-file format:
  $ dune pkg lock --format=file 2>&1 | head -5
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1


  $ test -f dune.lock && echo "dune.lock is back to a file"
  dune.lock is back to a file
