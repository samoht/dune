open Import

(** Opam variable expansion for package builds.

    This module provides the canonical implementation for expanding opam variables
    like %{prefix}%, %{lib}%, %{pkg:installed}%, etc. *)

(** {1 Variable Types} *)

module Variable : sig
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  val dyn_of_value : value -> Dyn.t
  val dune_value : value -> Value.t list
  val of_values : dir:Path.t -> Value.t list -> value
  val to_dyn : t -> Dyn.t
end

(** {1 Shared Install Paths} *)

module Shared_install : sig
  val dir : context:Context_name.t -> Path.Build.t
  val roots_build : context:Context_name.t -> Path.Build.t Install.Roots.t
  val roots : context:Context_name.t -> Path.t Install.Roots.t

  val roots_for_package
    :  pkg_name:Package.Name.t
    -> context:Context_name.t
    -> Path.t Install.Roots.t
end

(** {1 Section Directory Mapping} *)

val section_dir_of_root : Path.t Install.Roots.t -> Pform.Var.Pkg.Section.t -> Path.t

(** {1 System Variable Polling} *)

val sys_poll_var
  :  (Lock_dir.Sys_vars.t -> string option Memo.Lazy.t)
  -> Value.t list Memo.t

(** {1 Package Variable Expansion} *)

(** Expand package-level pform variables like %{prefix}%, %{lib}%, %{jobs}%, etc. *)
val expand_pkg
  :  context:Context_name.t
  -> source_dir:Path.t
  -> Pform.Var.Pkg.t
  -> Value.t list Memo.t

(** Resolve builtin package variables that don't come from the package itself *)
val resolve_builtin_var
  :  context:Context_name.t
  -> package_name:Package.Name.t
  -> all_versions:Package_version.t Package.Name.Map.t
  -> present:bool
  -> scope:Dune_pkg.Package_variable.Scope.t
  -> self_source_dir:Path.t
  -> dep_source_dir:Path.t option
  -> Package_variable_name.t
  -> (Value.t list, [> `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t
       option

(** Apply opam's var?default semantics *)
val apply_default_if_true
  :  string option
  -> (Value.t list, 'a) result
  -> (Value.t list, 'a) result

(** {1 String-based Variable Expansion}

    For opam builds that work directly with opam file commands. *)

(** Expand a bare identifier (CIdent in opam commands).
    Returns None if the variable is not recognized. *)
val expand_ident
  :  context:Context_name.t
  -> pkg_name:Package.Name.t
  -> pkg_version:Package_version.t
  -> prefix:Path.t
  -> ocamlfind_destdir:Path.t
  -> string
  -> string option

(** Expand %{var}% and %{pkg:var}% patterns in a string *)
val expand_string
  :  context:Context_name.t
  -> pkg_name:Package.Name.t
  -> pkg_version:Package_version.t
  -> all_packages:Package_version.t Package.Name.Map.t
  -> prefix:Path.t
  -> ocamlfind_destdir:Path.t
  -> string
  -> string Memo.t
