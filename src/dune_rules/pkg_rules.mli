(** rules for packages built by dune *)

open Import

val build_dir : Context_name.t -> Path.Build.t

(** Set up package build rules for a directory under _build/.pkgs/<ctx>/...
    Handles both vendor packages and lock file packages. *)
val setup_pkg_context_rules
  :  Context_name.t
  -> dir:Path.Build.t
  -> components:string list
  -> Build_config.Gen_rules.t Memo.t

val lock_dir_path : Context_name.t -> Path.t option Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val ocaml_toolchain : Context_name.t -> Ocaml_toolchain.t Action_builder.t option Memo.t
val which : Context_name.t -> (Filename.t -> Path.t option Memo.t) Staged.t
val exported_env : Context_name.t -> Env.t Memo.t
val project_ocamlpath : Context_name.t -> Path.t list Memo.t
val dev_tool_ocamlpath : Dune_pkg.Dev_tool.t -> Path.t list Memo.t
val find_package : Context_name.t -> Package.Name.t -> unit Action_builder.t option Memo.t
val dev_tool_env : Dune_pkg.Dev_tool.t -> Env.t Memo.t
val all_filtered_depexts : Context_name.t -> string list Memo.t

val all_filtered_depexts_with_origins
  :  Context_name.t
  -> (string * Package.Name.t * Dune_pkg.Package_version.t) list Memo.t

val setup_pkg_install_alias
  :  dir:Path.Build.t
  -> Context_name.t
  -> Build_config.Gen_rules.t

module Pkg_digest : sig
  type t

  val to_string : t -> string
end

val pkg_digest_of_project_dependency
  :  Context_name.t
  -> Package.Name.t
  -> Pkg_digest.t option Memo.t
