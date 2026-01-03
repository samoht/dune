open Import

(** Collect all pins from all projects in the workspace. *)
val project_pins : Dune_pkg.Pin.DB.t Memo.t

module Lock_format : sig
  type t = Dune_pkg.Lock.format =
    | Directory
    | Single_file
end

val solve
  :  Workspace.t
  -> local_packages:Dune_pkg.Local_package.t Package_name.Map.t
  -> project_pins:Dune_pkg.Pin.DB.t
  -> solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> version_preference:Dune_pkg.Version_preference.t option
  -> lock_dirs:Path.t list
  -> print_perf_stats:bool
  -> portable_lock_dir:bool
  -> format:Lock_format.t
  -> unit Fiber.t

(** Command to create lock directory *)
val command : unit Cmd.t
