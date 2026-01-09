Test that BUILD_PATH_PREFIX_MAP is properly set for relocatable builds,
and that prefix paths don't affect cache keys.

Create a package that echoes environment variables to verify BUILD_PATH_PREFIX_MAP is set:

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo BUILD_PATH_PREFIX_MAP is set: \${BUILD_PATH_PREFIX_MAP:+yes}"))
  > EOF

  $ build_pkg test
  BUILD_PATH_PREFIX_MAP is set: yes

Test that a package with a run action also has BUILD_PATH_PREFIX_MAP:

  $ make_lockpkg runtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX_MAP_SET=\${BUILD_PATH_PREFIX_MAP:+yes}"))
  > EOF

  $ build_pkg runtest
  PREFIX_MAP_SET=yes

Test that PREFIX and OPAM_SWITCH_PREFIX are available during build:

  $ make_lockpkg envtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX_SET=\${PREFIX:+yes}; echo OPAM_PREFIX_SET=\${OPAM_SWITCH_PREFIX:+yes}"))
  > EOF

  $ build_pkg envtest
  PREFIX_SET=yes
  OPAM_PREFIX_SET=yes

Test that a package with install action uses target_dir as PREFIX during build:

  $ make_lockpkg installtest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX contains target: \$(echo \$PREFIX | grep -q target && echo yes || echo no)"))
  > (install
  >  (run sh -c "echo Installing to PREFIX: \$(echo \$PREFIX | grep -q target && echo target_dir || echo shared)"))
  > EOF

  $ build_pkg installtest
  PREFIX contains target: yes
  Installing to PREFIX: target_dir

Test that a package WITHOUT install action uses shared PREFIX during build
(needed to find dependencies):

  $ make_lockpkg noinstall <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "echo PREFIX contains install: \$(echo \$PREFIX | grep -q install && echo yes || echo no)"))
  > EOF

  $ build_pkg noinstall
  PREFIX contains install: yes

Test that the installed marker file contains metadata:

  $ make_lockpkg markertest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "mkdir -p \$PREFIX/bin && echo test > \$PREFIX/bin/test"))
  > (install
  >  (run sh -c "echo installed"))
  > EOF

  $ build_pkg markertest
  installed

  $ cat "$(get_build_pkg_dir markertest)/installed"
  package: markertest
  version: 0.0.1
  prefix: _build/install/default

Test that files written to PREFIX during build end up in target_dir:

  $ make_lockpkg targettest <<EOF
  > (version 0.0.1)
  > (build
  >  (run sh -c "mkdir -p \$PREFIX/bin && echo 'hello' > \$PREFIX/bin/mytool"))
  > (install
  >  (run sh -c "echo installed"))
  > EOF

  $ build_pkg targettest
  installed

The file should exist in the target directory:

  $ cat "$(get_build_pkg_dir targettest)/target/bin/mytool"
  hello

Test BUILD_PATH_PREFIX_MAP contains the expected mappings:

  $ make_lockpkg maptest <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo \$BUILD_PATH_PREFIX_MAP | tr ':' '\n' | grep -c OPAMROOT"))
  > EOF

  $ build_pkg maptest
  2
