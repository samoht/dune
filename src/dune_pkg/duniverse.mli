open Import

(** Duniverse integration for dune package management.

    This module handles the classification and management of packages that can
    be built in the same dune context (duniverse) vs those that require the
    opam sandbox approach. *)

(** Where a package's source should be placed *)
type package_target =
  | Duniverse (** Package uses dune and will be placed in duniverse/<name>.<version>/ *)
  | Opam_sandbox
  (** Package doesn't use dune and will be built via opam sandbox in _build/.pkg/ *)

val package_target_to_dyn : package_target -> Dyn.t

(** The marker file that indicates a directory is a managed duniverse *)
val marker_filename : string

(** The directory name for duniverse (just the basename, not a path) *)
val marker_dirname : string

(** [duniverse_dir] returns the path to the duniverse directory relative to
    the project root. *)
val duniverse_dir : Path.Source.t

(** [package_dir name version] returns the path where a package's sources
    should be placed in the duniverse directory. *)
val package_dir : Package_name.t -> Package_version.t -> Path.Source.t

(** [classify pkg] determines whether a package should go to duniverse or
    opam sandbox based on whether it uses dune as its build system. *)
val classify : Lock_dir.Pkg.t -> package_target

(** [classify_all pkgs] classifies all packages, returning a map from
    package name to target. Packages that use dune go to duniverse,
    others go to opam sandbox. *)
val classify_all : Lock_dir.Pkg.t Package_name.Map.t -> package_target Package_name.Map.t

(** [get_patches pkg ~platform] extracts patch file paths from the package's
    build command for the given platform. Patches are returned in the order
    they should be applied. *)
val get_patches : Lock_dir.Pkg.t -> platform:Solver_env.t -> String_with_vars.t list
