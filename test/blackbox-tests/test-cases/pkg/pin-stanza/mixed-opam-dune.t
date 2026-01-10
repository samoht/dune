We try to use a project that has both opam files and a dune-project file. We
should favor the dune metadata in such a case.

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_source")
  >  (package (name foo))
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends foo bar))
  > EOF

  $ mkdir _source
  $ cat >_source/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > (package (name bar))
  > EOF
  $ cat >_source/bar.opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "bar" ]
  > EOF

  $ dune_pkg_lock_normalized --format=directory
  Solution for dune.lock (2 packages):
  dune:
  - bar.dev
  - foo.dev

  $ dune_cmd delete-between 'source' '^$' < ${default_lock_dir}/bar.dev.pkg 
  (version dev)
  
  (build
   (all_platforms ((dune))))
  
  (dev)
  
  (build_id f7acf542d23b85f2831d6d4dd1555ff3)
  $ dune_cmd delete-between 'source' '^$' < ${default_lock_dir}/foo.dev.pkg 
  (version dev)
  
  (build
   (all_platforms ((dune))))
  
  (dev)
  
  (build_id 65ba173dba376bcc10dea9d79aefe528)
