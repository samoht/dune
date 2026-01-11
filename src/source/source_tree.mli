(** Dune representation of the source tree *)

open Import

module Dir : sig
  type t

  val path : t -> Path.Source.t
  val filenames : t -> Filename.Set.t

  type sub_dir

  val sub_dirs : t -> sub_dir Filename.Map.t
  val sub_dir_as_t : sub_dir -> t Memo.t

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) : sig
    (** Traverse sub-directories recursively, pass them to [f] and combine
        intermediate results into a single one via [M.combine]. *)
    val map_reduce
      :  t
      -> traverse:Source_dir_status.Set.t
      -> trace_event_name:string
      -> f:(t -> Outcome.t M.t)
      -> Outcome.t M.t
  end

  val sub_dir_names : t -> Filename.Set.t
  val status : t -> Source_dir_status.t

  (** Return the contents of the dune (or jbuild) file in this directory *)
  val dune_file : t -> Dune_file.t option

  (** Return the project this directory is part of *)
  val project : t -> Dune_project.t

  val to_dyn : t -> Dyn.t
end

val root : unit -> Dir.t Memo.t

module Make_map_reduce_with_progress (M : Memo.S) (Outcome : Monoid) : sig
  (** Traverse starting from the root and report progress in the status line *)
  val map_reduce
    :  traverse:Source_dir_status.Set.t
    -> trace_event_name:string
    -> f:(Dir.t -> Outcome.t M.t)
    -> Outcome.t M.t
end

val find_dir : Path.Source.t -> Dir.t option Memo.t

(** [nearest_dir t fn] returns the directory with the longest path that is an
    ancestor of [fn]. *)
val nearest_dir : Path.Source.t -> Dir.t Memo.t

val files_of : Path.Source.t -> Path.Source.Set.t Memo.t

(** [true] iff the path is a vendored directory *)
val is_vendored : Path.Source.t -> bool Memo.t

(** [vendor_stanza dir] returns the vendor stanza for [dir] if one exists
    in a parent directory's dune file. *)
val vendor_stanza : Path.Source.t -> Vendor_stanza.t option Memo.t

(** [vendor_stanzas dir] returns all vendor stanzas defined in the dune file
    at [dir], as a list of (subdirectory_name, stanza) pairs. *)
val vendor_stanzas : Path.Source.t -> (Filename.t * Vendor_stanza.t) list Memo.t

(** [all_vendor_stanzas ()] returns all vendor stanzas in the entire workspace,
    as a list of (full_path, stanza) pairs where full_path is the absolute
    source path to the vendored directory. *)
val all_vendor_stanzas : unit -> (Path.Source.t * Vendor_stanza.t) list Memo.t

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)
val nearest_vcs : Path.Source.t -> Vcs.t option Memo.t

(** [all_workspace_packages ()] returns all packages defined in the workspace
    via (package ...) stanzas in dune-project files. Returns a map from package
    name to (package, source_dir) pairs. This function is designed to be called
    from Package_registry without creating dependency cycles. *)
val all_workspace_packages
  :  unit
  -> (Dune_lang.Package.t * Path.Source.t) Dune_lang.Package.Name.Map.t Memo.t
