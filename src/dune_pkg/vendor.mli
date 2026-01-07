open Import

(** How a vendored package should be built.

    - [Dune_native]: Built as vendored code in the main dune context.
      Libraries are directly available to the build system.
    - [Opam_sandboxed]: Built in an isolated opam-style sandbox using
      the package's opam build commands. *)
type build_method =
  | Dune_native
  | Opam_sandboxed

val build_method_to_dyn : build_method -> Dyn.t
val default_dir_name : string
val default_dir : Path.Source.t
val marker_filename : string
val package_dir : Package_name.t -> Package_version.t -> Path.Source.t

type source_group =
  { packages : (Package_name.t * Package_version.t) list
  ; source : Source.t
  ; primary_name : Package_name.t
  ; primary_version : Package_version.t
  }

val group_by_source : Lock.Pkg.t list -> source_group list
val source_group_dir : source_group -> Path.Source.t
val source_group_packages : source_group -> (Package_name.t * Package_version.t) list
val classify_build_method : Lock.Pkg.t -> build_method

val classify_build_method_all
  :  Lock.Pkg.t Package_name.Map.t
  -> build_method Package_name.Map.t

val get_patches : Lock.Pkg.t -> platform:Solver_env.t -> String_with_vars.t list
