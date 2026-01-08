open Stdune

(** Add values for expanding [%{name}] for a package *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamTypes.full_variable -> OpamVariable.variable_contents option)
  -> OpamTypes.full_variable
  -> OpamVariable.variable_contents option

(** Convert a selected opam package to a package that dune can save to the lock
    directory.

    @param allow_missing_deps When true, skip dependencies not in the version
    map instead of failing. Use this when deriving from a lock file where
    optional/virtual deps may be filtered out. *)
val opam_package_to_lock_file_pkg
  :  Solver_env.t
  -> Solver_stats.Updater.t
  -> Package_version.t Package_name.Map.t
  -> OpamPackage.t
  -> pinned:bool
  -> Resolved_package.t
  -> portable_lock_dir:bool
  -> allow_missing_deps:bool
  -> (Pkg.t, User_message.t) result

(** [file_to_lock ~loc ~solver_env file] loads the repos at their pinned hashes,
    looks up each package from the single-file lock format, and converts them
    to a full [Lock.t] using the provided solver environment for evaluating
    platform-specific formulas. *)
val file_to_lock : loc:Loc.t -> solver_env:Solver_env.t -> Lock.File.t -> Lock.t Fiber.t

(** Like [file_to_lock] but also returns the opam file content for each package.
    Used by fetch to write opam files to duniverse. Returns the lock and a list
    of (package_name, opam_file_content) pairs. *)
val file_to_lock_with_opam_files
  :  loc:Loc.t
  -> solver_env:Solver_env.t
  -> Lock.File.t
  -> (Lock.t * (Package_name.t * string) list) Fiber.t

(** [read_disk ~solver_env path] reads a lock from either directory or
    single-file format. For directory format, it uses the synchronous reader.
    For single-file format, it parses the file and derives full package metadata
    from the opam repos specified in the file.

    @param solver_env Used for platform-specific dependency evaluation
    @param path Path to either dune.lock directory or dune.lock file *)
val read_disk : solver_env:Solver_env.t -> Path.t -> Lock.t Fiber.t

(** Like [read_disk] but also returns opam file contents for each package.
    Used by fetch to write opam files to duniverse. For single-file format,
    returns the opam file content derived from the repo. For directory format,
    returns an empty list (opam files not available).

    @param solver_env Used for platform-specific dependency evaluation
    @param path Path to either dune.lock directory or dune.lock file *)
val read_disk_with_opam_files
  :  solver_env:Solver_env.t
  -> Path.t
  -> (Lock.t * (Package_name.t * string) list) Fiber.t

(** [pkg_of_local_opam_file ~loc ~name ~version ~opam_file ~source] creates a Pkg.t
    from a local opam file. Used for vendored opam packages that don't go through
    the solver. Build and install commands are extracted directly from the opam file. *)
val pkg_of_local_opam_file
  :  loc:Loc.t
  -> name:Package_name.t
  -> version:Package_version.t
  -> opam_file:OpamFile.OPAM.t
  -> source:Source.t
  -> (Pkg.t, User_message.t) result
