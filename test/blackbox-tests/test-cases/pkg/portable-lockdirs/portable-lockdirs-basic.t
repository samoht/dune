Basic usage of portable lockdirs.

  $ mkrepo
  $ add_mock_repo_if_needed

Create a package that writes a different value to some files depending on the os and arch.
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo Darwin > %{share}%/kernel"] { os = "macos" }
  >   ["sh" "-c" "echo Linux > %{share}%/kernel"] { os = "linux" }
  >   ["sh" "-c" "echo x86_64 > %{share}%/machine"] { arch = "x86_64" }
  >   ["sh" "-c" "echo arm64 > %{share}%/machine"] { arch = "arm64" }
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

  $ dune pkg lock --format=directory
  Solution for dune.lock (1 package)
  
  Dependencies common to all supported platforms:
  opam:
  - foo.0.0.1

  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (dependency_hash 36e640fbcda71963e7e2f689f6c96c3e)
  
  (repositories
   (complete true)
   (used
    ((source
      file:///Users/samoht/git/dune/_build/.sandbox/bf059f20117192d201d3602f9ec6568d/default/test/blackbox-tests/test-cases/pkg/portable-lockdirs/mock-opam-repository#b1377c291a62015e711d4906668b1204c9634991))))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (choice
    ((((arch x86_64) (os linux)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Linux > %{share}/kernel")
        (run sh -c "echo x86_64 > %{share}/machine")))))
    ((((arch arm64) (os linux)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Linux > %{share}/kernel")
        (run sh -c "echo arm64 > %{share}/machine")))))
    ((((arch x86_64) (os macos)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Darwin > %{share}/kernel")
        (run sh -c "echo x86_64 > %{share}/machine")))))
    ((((arch arm64) (os macos)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Darwin > %{share}/kernel")
        (run sh -c "echo arm64 > %{share}/machine")))))))
  
  (build_id 5e27d13fee96f58e7e42d014cefe73f0)

  $ DUNE_CONFIG__ARCH=arm64 dune build
  File "dune", line 3, characters 12-15:
  3 |  (libraries foo))
                  ^^^
  Error: Library "foo" not found.
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  -> required by alias default
  [1]
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/kernel
  cat: _build/.pkgs/default/foo/target/share/kernel: No such file or directory
  [1]
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/machine
  cat: _build/.pkgs/default/foo/target/share/machine: No such file or directory
  [1]

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
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/kernel
  cat: _build/.pkgs/default/foo/target/share/kernel: No such file or directory
  [1]
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/machine
  cat: _build/.pkgs/default/foo/target/share/machine: No such file or directory
  [1]
