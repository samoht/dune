open Import

(** Unified package registry that merges lock file packages and vendor packages.

    This provides a single lookup point for all external packages available
    in a build context, regardless of whether they come from lock files or
    vendor stanzas. *)

(** Source of a package. *)
module Source : sig
  type t =
    | From_vendor of
        { source_dir : Path.Source.t
        ; stanza : Dune_lang.Vendor_stanza.t
        }
    | From_lock of { pkg : Dune_pkg.Pkg.t }
end

(** A package entry in the registry. *)
type entry =
  { name : Package.Name.t
  ; version : Package_version.t
  ; source : Source.t
  }

(** The package registry for a build context. *)
type t

(** Get the package registry for a context. Merges lock file packages with
    vendor packages, with vendor packages taking precedence. *)
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
