Test the (verbose <mode>) config option.

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune<<EOF
  > (rule
  >  (alias default)
  >  (action (run echo "Hello, world!")))
  > EOF

Test verbose option in config file with different modes:

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose quiet)
  > EOF
  $ dune build --config-file=dune-config

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose short)
  > EOF
  $ dune build -f --config-file=dune-config

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose progress)
  > EOF
  $ dune build -f --config-file=dune-config

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose verbose)
  > EOF
  $ dune build -f --config-file=dune-config 2>&1 | grep Hello | sed 's/&&.*echo/\&\& echo/'

Test "auto" and "default" aliases (both map to quiet):

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose auto)
  > EOF
  $ dune build -f --config-file=dune-config

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose default)
  > EOF
  $ dune build -f --config-file=dune-config

Test invalid verbose mode:

  $ cat >dune-config<<EOF
  > (lang dune 3.20)
  > (verbose invalid)
  > EOF
  $ dune build --config-file=dune-config
  File "$TESTCASE_ROOT/dune-config", line 2, characters 9-16:
  2 | (verbose invalid)
               ^^^^^^^
  Error: Unknown value invalid
  [1]
