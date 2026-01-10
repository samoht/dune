Demonstrate the case where a project can only be solved for a subset of platforms.

  $ mkrepo
  $ add_mock_repo_if_needed

Make a package that is only available on macos.
  $ mkpkg foo <<EOF
  > available: os = "macos"
  > build: [
  >   ["mkdir" "-p" "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

Solving will still succeed, but there'll be a warning because dune will attempt
to solve for macos, linux, and windows by default.
  $ dune pkg lock --format=directory --trace-file trace.csexp
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1
  
  No package solution was found for some requsted platforms.
  
  Platforms with no solution:
  - arch = arm64; os = linux
  - arch = x86_64; os = linux
  
  See the trace file with --trace-file for more details. Configure platforms to
  solve for in the dune-workspace file.

The log file will contain errors about the package being unavailable.
  $ jqScript=$(mktemp)
  $ cat >$jqScript <<EOF
  > select(.cat == "log" and .args.message != "ocamlparam" and (.args.message | contains("Shared cache") | not)) |
  > .args
  > EOF
  $ dune trace cat --trace-file trace.csexp | jq -f $jqScript
  {
    "message": "Workspace root",
    "root": "$TESTCASE_ROOT"
  }
  {
    "message": "Package management",
    "auto_lock": "Auto",
    "auto_fetch": true
  }
  {
    "message": "Git repository cache location",
    "dir": [
      "External",
      "$TESTCASE_ROOT/.cache/dune/git-repo"
    ]
  }
  {
    "message": "Revision store cache",
    "status": "Disabled"
  }
  {
    "message": "Solver found partial solution",
    "error_count": 1
  }
  {
    "message": "Dependency solution",
    "lock_dir": "dune.lock",
    "packages": [
      "foo.0.0.1"
    ]
  }

The lockdir will contain a list of the platforms where solving succeeded.
  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (dependency_hash 36e640fbcda71963e7e2f689f6c96c3e)
  
  (repositories
   (complete true)
   (used
    ((source
      file:///Users/samoht/git/dune/_build/.sandbox/cf4644c05249addacb8f7c9bf11d99a7/default/test/blackbox-tests/test-cases/pkg/portable-lockdirs/mock-opam-repository#dc58c0c9550a43a64579c35fa89346e94a869dd2))))
  
  (solved_for_platforms
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

No errors when you try to build the platform on macos.
  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]

Building on linux fails because the lockdir doesn't contain a compatible solution.
  $ DUNE_CONFIG__OS=linux DUNE_CONFIG__ARCH=arm64 DUNE_CONFIG__OS_FAMILY=debian DUNE_CONFIG__OS_DISTRIBUTION=ubuntu DUNE_CONFIG__OS_VERSION=24.11 dune build
  File "dune.lock/lock.dune", lines 12-15, characters 1-58:
  12 |  ((arch x86_64)
  13 |   (os macos))
  14 |  ((arch arm64)
  15 |   (os macos)))
  Error: The lockdir does not contain a solution compatible with the current
  platform.
  The current platform is:
  - arch = arm64
  - os = linux
  - os-distribution = ubuntu
  - os-family = debian
  - os-version = 24.11
  - sys-ocaml-version = 5.4.0+fake
  Hint: Try adding the following to dune-workspace:
  Hint: (lock_dir (solve_for_platforms ((arch arm64) (os linux))))
  Hint: ...and then rerun 'dune pkg lock'
  [1]
