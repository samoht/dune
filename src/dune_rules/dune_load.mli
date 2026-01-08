(** Loads dune files from the workspace and query the workspace for various
    global data such as dune files, projects, pcakages.

    All the functions here are memoized. *)

open Import

(** Key for packages: (name, version option) to support multi-version vendoring *)
module Package_key : sig
  type t = Package.Name.t * Package_version.t option

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
end

module Package_key_map : Map.S with type key = Package_key.t

val dune_files : Context_name.t -> Dune_file.t list Memo.t
val projects_by_root : unit -> Dune_project.t Path.Source.Map.t Memo.t
val find_project : dir:Path.Build.t -> Dune_project.t Memo.t
val stanzas_in_dir : Path.Build.t -> Dune_file.t option Memo.t
val mask : unit -> Only_packages.t Memo.t

(** Returns packages indexed by name. For vendored packages with the same name
    but different versions, only the first one is included here. *)
val packages : unit -> Package.t Package.Name.Map.t Memo.t

(** Returns all packages indexed by (name, version). This includes all versions
    of vendored packages with the same name. *)
val all_packages : unit -> Package.t Package_key_map.t Memo.t

val projects : unit -> Dune_project.t list Memo.t

(** Returns the set of vendored package names *)
val vendored_packages : unit -> Package.Name.Set.t Memo.t
