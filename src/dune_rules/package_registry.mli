open Import

(** Unified package registry that merges lock file packages, vendor packages,
    and workspace packages.

    This provides a single lookup point for all packages available in a build
    context, regardless of whether they come from lock files, vendor stanzas,
    or workspace dune-project files.

    Precedence order: vendor > workspace > lock *)

(** Source of a package. *)
module Source : sig
  type t =
    | From_vendor of
        { source_dir : Path.Source.t
        ; stanza : Dune_lang.Vendor_stanza.t
        }
    | From_lock of { pkg : Dune_pkg.Pkg.t }
    | From_workspace of
        { pkg : Package.t
        ; source_dir : Path.Source.t
        }
end

(** A package entry in the registry. *)
type entry =
  { name : Package.Name.t
  ; version : Package_version.t
  ; source : Source.t
  }

(** The package registry for a build context. *)
type t

(** Get the package registry for a context. Merges lock file packages, vendor
    packages, and workspace packages with precedence: vendor > workspace > lock. *)
val of_ctx : Context_name.t -> t Memo.t

(** Find a package by name. Returns [None] if not found. *)
val find : t -> Package.Name.t -> entry option

(** Find a package by name and version. Returns [None] if not found or
    if the version doesn't match. *)
val find_by_name_version
  :  t
  -> name:Package.Name.t
  -> version:Package_version.t
  -> entry option

(** List all packages in the registry. *)
val to_list : t -> entry list

(** Check if a package is registered. *)
val mem : t -> Package.Name.t -> bool

(** Get the version of a package if it exists. *)
val version : t -> Package.Name.t -> Package_version.t option

(** Find which package provides a given library name.
    Only works for vendor packages that expose library information. *)
val package_for_library : t -> string -> Package.Name.t option

(** Check if a package needs a marker file for dependency tracking.
    Lock packages and opam-sandboxed vendor packages need markers. *)
val needs_marker : entry -> bool

(** Check if a package should install to the shared prefix.
    Most packages do, except vendor packages with install=false. *)
val install_to_prefix : entry -> bool

(** Get the compiler name provided by a package, if any.
    Only vendor packages can declare compiler providers via [(compiler ...)]. *)
val compiler : entry -> Package.Name.t option

(** Find the package that provides a given compiler name. *)
val find_compiler : t -> Package.Name.t -> entry option

(** Get the toolchain name provided by a package, if any.
    Only vendor packages can declare toolchain providers via [(toolchain ...)]. *)
val toolchain : entry -> string option

(** Find the package that provides a given toolchain name. *)
val find_toolchain : t -> string -> entry option

(** Create a registry from a map of lock file packages.
    Used for dev tool contexts which don't have a workspace context. *)
val of_lock_packages : Dune_pkg.Pkg.t Package.Name.Map.t -> t
