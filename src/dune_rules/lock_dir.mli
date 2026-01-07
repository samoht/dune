open Import
module Pkg = Dune_pkg.Pkg
module Pkg_info = Dune_pkg.Pkg.Info
module Build_command = Dune_pkg.Pkg.Build_command
module Depexts = Dune_pkg.Pkg.Depexts
module Conditional_choice = Dune_pkg.Pkg.Conditional_choice
module Dependency = Dune_pkg.Pkg.Dependency

type t := Dune_pkg.Lock.t

val context : Build_context.t
val build_dir : Context_name.t -> Path.Build.t
val get_with_path : Context_name.t -> (Path.t * t, User_message.t) result Memo.t
val get : Context_name.t -> (t, User_message.t) result Memo.t
val get_exn : Context_name.t -> t Memo.t
val load_exn : Path.t -> t Memo.t
val load_if_exists : Path.t -> t option Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val get_path : Context_name.t -> Path.t option Memo.t
val default_path : Path.t
val default_source_path : Path.Source.t
val select_lock_dir : Workspace.Lock_dir_selection.t -> Path.Source.t Memo.t

(** Returns the lock directories present in the given workspace. *)
val lock_dirs_of_workspace : Workspace.t -> Path.Source.Set.t Memo.t

module Sys_vars : sig
  type t =
    { os : string option Memo.Lazy.t
    ; os_version : string option Memo.Lazy.t
    ; os_distribution : string option Memo.Lazy.t
    ; os_family : string option Memo.Lazy.t
    ; arch : string option Memo.Lazy.t
    ; sys_ocaml_version : string option Memo.Lazy.t
    }

  val os : t -> Dune_lang.Pform.Var.Os.t -> string option Memo.t
  val poll : t
  val solver_env : Dune_pkg.Solver_env.t Memo.t
end

val source_kind
  :  Dune_pkg.Source.t
  -> [ `Local of [ `Directory | `File ] * Path.External.t | `Fetch ] Memo.t
