We test an repository with a package cycle that has an alternative solution which avoids a
cycle. We have the following packages:

>      a --> b --> c

Now c depends either on a or on a fourth package d. Therefore there is a valid solution
available that avoids a cycle.

  $ mkrepo

  $ mkpkg a <<EOF
  > depends: [ "b" ]
  > EOF
  $ mkpkg b <<EOF
  > depends: [ "c" ]
  > EOF
  $ mkpkg c <<EOF
  > depends: [ "a" | "d" ]
  > EOF
  $ mkpkg d

Solver finds the invalid solution as it doesn't check cycles.

  $ solve c
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Cycle in package dependencies", { cycle = [ "a"; "b"; "c"; "a" ] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Lock.compute_build_ids in file "src/dune_pkg/lock.ml",
    lines 258-260, characters 6-55
  Called from Dune_pkg__Lock.create_latest_version in file
    "src/dune_pkg/lock.ml", line 299, characters 17-43
  Called from Stdune__Result.map in file "otherlibs/stdune/src/result.ml", line
    45, characters 15-20
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 1951-1995, characters 9-29
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Cycle in package dependencies", { cycle = [ "a"; "b"; "c"; "a" ] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Lock.compute_build_ids in file "src/dune_pkg/lock.ml",
    lines 258-260, characters 6-55
  Called from Dune_pkg__Lock.create_latest_version in file
    "src/dune_pkg/lock.ml", line 299, characters 17-43
  Called from Stdune__Result.map in file "otherlibs/stdune/src/result.ml", line
    45, characters 15-20
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 1951-1995, characters 9-29
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Cycle in package dependencies", { cycle = [ "a"; "b"; "c"; "a" ] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Lock.compute_build_ids in file "src/dune_pkg/lock.ml",
    lines 258-260, characters 6-55
  Called from Dune_pkg__Lock.create_latest_version in file
    "src/dune_pkg/lock.ml", line 299, characters 17-43
  Called from Stdune__Result.map in file "otherlibs/stdune/src/result.ml", line
    45, characters 15-20
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 1951-1995, characters 9-29
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Cycle in package dependencies", { cycle = [ "a"; "b"; "c"; "a" ] })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune/src/code_error.ml", line 10, characters 30-62
  Called from Dune_pkg__Lock.compute_build_ids in file "src/dune_pkg/lock.ml",
    lines 258-260, characters 6-55
  Called from Dune_pkg__Lock.create_latest_version in file
    "src/dune_pkg/lock.ml", line 299, characters 17-43
  Called from Stdune__Result.map in file "otherlibs/stdune/src/result.ml", line
    45, characters 15-20
  Called from Dune_pkg__Opam_solver.solve_lock_dir.(fun) in file
    "src/dune_pkg/opam_solver.ml", lines 1951-1995, characters 9-29
  Called from Fiber__Scheduler.exec in file "src/fiber/src/scheduler.ml", line
    76, characters 8-11
  [1]
