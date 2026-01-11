Multiple opam repositories that define the same package:

  $ pkg="packages/foo"
  $ mkdir -p repo1/$pkg repo2/$pkg

  $ mkpkg() {
  > local p="$1/$pkg/foo.$2"
  > mkdir -p $p
  > cat >$p/opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "$1" ]
  > EOF
  > }

  $ repos12=`cat <<EOF
  > (repository
  >  (name repo1)
  >  (url "file://$PWD/repo1"))
  > (repository
  >  (name repo2)
  >  (url "file://$PWD/repo2"))
  > EOF
  > `

  $ cat >dune-workspace <<EOF
  > (lang dune 3.20)
  > (pkg enabled)
  > (lock_dir
  >  (repositories repo1 repo2))
  > $repos12
  > EOF

  $ make_project foo | cat >dune-project

  $ runtest () {
  >   version=$1
  >   dune_pkg_lock_normalized
  >   cat ${default_lock_dir}/foo.$version.pkg
  > }

Define 1.0.0 in repo1 and 2.0.0 in repo2 for the same package:

  $ mkpkg repo1 1.0.0
  $ runtest 1.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.1.0.0
  (version 1.0.0)
  
  (build
   (all_platforms ((action (run echo repo1)))))
  
  (build_id 31fcce150dfdcf9672ef0ea3006ed5a4)

  $ mkpkg repo2 2.0.0
  $ runtest 2.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.2.0.0
  (version 2.0.0)
  
  (build
   (all_platforms ((action (run echo repo2)))))
  
  (build_id dfdc535b6eefbaf82719d1a6982ee8d5)

We define 2.0.0 in both repo1 and repo2, but repo1 is listed first, so it
should take priority

  $ mkpkg repo1 2.0.0
  $ runtest 2.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.2.0.0
  (version 2.0.0)
  
  (build
   (all_platforms ((action (run echo repo1)))))
  
  (build_id 68df83695f0b2c010c4821095be99ebe)

Even though repo2 is of lesser priority, it has the best version so it should
be selected:

  $ mkpkg repo2 3.0.0
  $ runtest 3.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (all_platforms ((action (run echo repo2)))))
  
  (build_id a71eb99e701b4550992915b783462039)

Now we repeat the tests but with a git repo:

  $ mkdir -p git-repo
  $ mkpkg git-repo 3.0.0
  $ cd git-repo
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

  $ mkworkspace() {
  > cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (repositories $@))
  > $repos12
  > (repository
  >  (name git-repo)
  >  (url "git+file://$PWD/git-repo"))
  > EOF
  > }

  $ mkworkspace "repo1 repo2 git-repo"
  $ runtest 3.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (all_platforms ((action (run echo repo2)))))
  
  (build_id a71eb99e701b4550992915b783462039)

  $ mkworkspace "git-repo repo1 repo2"
  $ runtest 3.0.0
  Solution for dune.lock (1 package):
  opam:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (all_platforms ((action (run echo git-repo)))))
  
  (build_id a3aea326619a83cbf85fb9a1bbd98eed)
