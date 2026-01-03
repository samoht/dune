open Stdune

(** Add values for expanding [%{name}] for a package *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamTypes.full_variable -> OpamVariable.variable_contents option)
  -> OpamTypes.full_variable
  -> OpamVariable.variable_contents option

(** Convert a selected opam package to a package that dune can save to the lock
    directory *)
val opam_package_to_lock_file_pkg
  :  Solver_env.t
  -> Solver_stats.Updater.t
  -> Package_version.t Package_name.Map.t
  -> OpamPackage.t
  -> pinned:bool
  -> Resolved_package.t
  -> portable_lock_dir:bool
  -> (Lock.Pkg.t, User_message.t) result

(** [file_to_lock ~loc ~solver_env file] loads the repos at their pinned hashes,
    looks up each package from the single-file lock format, and converts them
    to a full [Lock.t] using the provided solver environment for evaluating
    platform-specific formulas. *)
val file_to_lock : loc:Loc.t -> solver_env:Solver_env.t -> Lock.File.t -> Lock.t Fiber.t

(** [read_disk_fiber ~solver_env path] reads a lock from either directory or
    single-file format. For directory format, it uses the synchronous reader.
    For single-file format, it parses the file and derives full package metadata
    from the opam repos specified in the file.

    @param solver_env Used for platform-specific dependency evaluation
    @param path Path to either dune.lock directory or dune.lock file *)
val read_disk_fiber : solver_env:Solver_env.t -> Path.t -> Lock.t Fiber.t
