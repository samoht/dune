Here we test the translation and implementation of global opam variables. OS specific
variables can be found in `opam-var-os.t`.

  $ mkrepo
  > mkpkg testpkg << EOF
  > build: [
  >   [ "echo" jobs ]
  >   [ "echo" make ]
  >   [ "echo" user ] 
  >   [ "echo" group ]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
  $ cat ${default_lock_dir}/testpkg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{jobs})
       (run echo %{make})
       (run echo %{user})
       (run echo %{group}))))))
  
  (build_id 6ae2c0ba17f2de64493df9ab799e2a59)


- The implementation of %{user} uses Unix.getlogin which doesn't work in our Linux CI job.
- The implementation of %{make} prefers gmake over make and is tested in `make.t`.

Therefore we modify the lockfile here to remove these from the opam file:

  $ mkpkg testpkg << EOF
  > build: [
  >   [ "echo" jobs ]
  >   [ "echo" group ]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock (1 package):
  opam:
  - testpkg.0.0.1
The value for "jobs" should always be 1.

  $ GROUP="$(id -gn)"
  > build_pkg testpkg 2>&1 | dune_cmd subst "$GROUP" 'GROUP'
  Error: Don't know how to build _build/.pkgs/default/testpkg/installed
