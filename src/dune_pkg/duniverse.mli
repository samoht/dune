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

(** A group of packages that share the same source. Multiple packages from
    the same repository (e.g., uri and uri-sexp from ocaml-uri) are grouped
    together to avoid duplicate fetching. *)
type source_group =
  { packages : (Package_name.t * Package_version.t) list
  ; source : Source.t
  ; primary_name : Package_name.t
  ; primary_version : Package_version.t
  }

(** [group_by_source pkgs] groups packages by their source URL and checksum.
    Packages with the same source are grouped together, allowing them to be
    fetched once and placed in a shared directory. *)
val group_by_source : Lock.Pkg.t list -> source_group list

(** [source_group_dir group] returns the directory path for a source group.
    Uses the primary (first alphabetically) package name. *)
val source_group_dir : source_group -> Path.Source.t

(** [source_group_packages group] returns all packages in a source group. *)
val source_group_packages : source_group -> (Package_name.t * Package_version.t) list

(** [classify pkg] determines whether a package should go to duniverse or
    opam sandbox based on whether it uses dune as its build system. *)
val classify : Lock.Pkg.t -> package_target

(** [classify_all pkgs] classifies all packages, returning a map from
    package name to target. Packages that use dune go to duniverse,
    others go to opam sandbox. *)
val classify_all : Lock.Pkg.t Package_name.Map.t -> package_target Package_name.Map.t

(** [get_patches pkg ~platform] extracts patch file paths from the package's
    build command for the given platform. Patches are returned in the order
    they should be applied. *)
val get_patches : Lock.Pkg.t -> platform:Solver_env.t -> String_with_vars.t list
