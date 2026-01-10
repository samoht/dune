(** Package specification used by dune's package management.
    A Pkg.t can come from a lock file or from a vendor directory's opam file. *)

open Import

module Solver_env_disjunction : sig
  type t = Solver_env.t list

  val singleton : Solver_env.t -> t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val hash : t -> int
  val digest_feed : t Dune_digest.Feed.t
  val encode : t -> Dune_sexp.t
  val decode : t Dune_sexp.Decoder.t
  val matches_platform : t -> platform:Solver_env.t -> bool
end

module Conditional : sig
  type 'a t =
    { condition : Solver_env_disjunction.t
    ; value : 'a
    }

  val make : Solver_env.t -> 'a -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val hash : 'a t -> f:('a -> int) -> int
  val digest_feed : 'a Dune_digest.Feed.t -> 'a t Dune_digest.Feed.t
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  val decode : 'a Dune_sexp.Decoder.t -> 'a t Dune_sexp.Decoder.t
  val encode : ('a -> Dune_sexp.t) -> 'a t -> Dune_sexp.t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val evaluate : 'a t -> platform:Solver_env.t -> 'a option
end

module Conditional_choice : sig
  (** A sequence of values, each conditional on an environment. *)
  type 'a t = 'a Conditional.t list

  val empty : 'a t
  val singleton : Solver_env.t -> 'a -> 'a t
  val singleton_all_platforms : 'a -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val hash : 'a t -> f:('a -> int) -> int
  val digest_feed : 'a Dune_digest.Feed.t -> 'a t Dune_digest.Feed.t
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  (** Returns the first value whose associated environment is a subset of the
      specified environment. *)
  val choose_for_platform : 'a t -> platform:Solver_env.t -> 'a option

  (** [exists t ~f] returns true if [f] returns true for any value in [t]. *)
  val exists : 'a t -> f:('a -> bool) -> bool

  val find_platform : 'a t -> f:('a -> bool) -> bool
  val merge_combining_conditions : value_equal:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
  val get_value_ensuring_at_most_one_choice : 'a t -> 'a option
end

module Info : sig
  (** Representation of the parsed package info.
      The [dev] field configures the "dev" filter from OPAM formulae. *)
  type t =
    { name : Package_name.t
    ; version : Package_version.t
    ; dev : bool
    ; avoid : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  val equal : t -> t -> bool
  val hash : t -> int
  val digest_feed : t Dune_digest.Feed.t
  val to_dyn : t -> Dyn.t
  val default_version : Package_version.t
  val variables : t -> OpamVariable.variable_contents Package_variable_name.Map.t
  val remove_locs : t -> t
end

module Build_command : sig
  type t =
    | Action of Action.t
    | Dune (** pinned dune packages do not need to define a command *)

  val equal : t -> t -> bool
  val remove_locs : t -> t
  val to_dyn : t -> Dyn.t
  val encode_non_portable : t option -> Dune_sexp.Encoder.field
  val encode_portable : t -> Dune_sexp.t
  val decode_portable : t Dune_sexp.Decoder.t
end

module Dependency : sig
  type t =
    { loc : Loc.t
    ; name : Package_name.t
    }

  val equal : t -> t -> bool
  val remove_locs : t -> t
  val to_dyn : t -> Dyn.t
  val decode : t Dune_sexp.Decoder.t
  val encode : t -> Dune_sexp.t
end

module Dependencies : sig
  type t = Dependency.t list

  val equal : t -> t -> bool
  val remove_locs : t -> t
  val to_dyn : t -> Dyn.t
  val encode : t -> Dune_sexp.t
end

module Depexts : sig
  type t =
    { external_package_names : string list
    ; enabled_if : [ `Always | `Conditional of Slang.Blang.t ]
    }

  val equal : t -> t -> bool
  val remove_locs : t -> t
  val to_dyn : t -> Dyn.t
  val encode : t -> Dune_sexp.t
  val decode : t Dune_sexp.Decoder.t
end

module Conditional_choice_or_all_platforms : sig
  type 'a t =
    | Choice of 'a Conditional_choice.t
    | All_platforms of 'a

  val of_conditional_choice
    :  solved_for_platforms:Solver_env_disjunction.t
    -> 'a Conditional_choice.t
    -> 'a t option

  val to_conditional_choice
    :  solved_for_platforms:Solver_env_disjunction.t
    -> 'a t
    -> 'a Conditional_choice.t

  val decode : 'a Dune_sexp.Decoder.t -> 'a t Dune_sexp.Decoder.t
  val encode : ('a -> Dune_sexp.t) -> 'a t -> Dune_sexp.t

  val encode_field
    :  solved_for_platforms:Solver_env_disjunction.t
    -> string
    -> ('a -> Dune_sexp.t)
    -> 'a Conditional_choice.t
    -> Dune_sexp.Encoder.field
end

module Enabled_on_platforms : sig
  type t =
    | All
    | Only of Solver_env_disjunction.t

  val of_solver_env_disjunction
    :  solved_for_platforms:Solver_env_disjunction.t
    -> Solver_env_disjunction.t
    -> t

  val to_solver_env_disjunction
    :  solved_for_platforms:Solver_env_disjunction.t
    -> t
    -> Solver_env_disjunction.t

  val encode : t -> Dune_sexp.t
  val decode : t Dune_sexp.Decoder.t
end

val decode_build_command_fields
  :  portable_lock_dir:bool
  -> Build_command.t Conditional_choice_or_all_platforms.t option
       Dune_sexp.Decoder.fields_parser

type t =
  { build_command : Build_command.t Conditional_choice.t
  ; install_command : Action.t Conditional_choice.t
  ; depends : Dependencies.t Conditional_choice.t
  ; post_depends : Dependencies.t Conditional_choice.t
    (** Post deps are installed WITH the package but don't affect build order.
        They're tracked separately to avoid creating false dependency cycles. *)
  ; depexts : Depexts.t list
  ; info : Info.t
  ; exported_env : String_with_vars.t Action.Env_update.t list
  ; enabled_on_platforms : Solver_env_disjunction.t
  ; build_id : Dune_digest.t option
    (** Recursive build-id: hash(content, deps' build_ids).
        Computed at lock time for deterministic toolchain/dev-tool caching.
        None for lock files created before this feature. *)
  }

val equal : t -> t -> bool
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val to_dyn : t -> Dyn.t
val remove_locs : t -> t
val is_enabled_on_platform : t -> platform:Solver_env.t -> bool
val compute_missing_checksum : t -> pinned:bool -> t Fiber.t

module Fields : sig
  val version : string
  val build : string
  val install : string
  val depends : string
  val post_depends : string
  val depexts : string
  val source : string
  val dev : string
  val avoid : string
  val exported_env : string
  val extra_sources : string
  val enabled_on_platforms : string
  val build_id : string
end

val decode
  :  portable_lock_dir:bool
  -> (lock_dir:Path.t
      -> solved_for_platforms:Solver_env_disjunction.t
      -> Package_name.t
      -> t)
       Dune_sexp.Decoder.t

val encode
  :  portable_lock_dir:bool
  -> solved_for_platforms:Solver_env_disjunction.t
  -> t
  -> Dune_sexp.t list

val files_dir : Package_name.t -> Package_version.t option -> lock_dir:Path.t -> Path.t

val source_files_dir
  :  Package_name.t
  -> Package_version.t option
  -> lock_dir:Path.t
  -> Path.Source.t

val merge_conditionals : t -> t -> t

(** Helper module for converting opam files to Pkg.t *)
module Opam_conversion : sig
  (** Simplify a filter by partially evaluating it with solver variables. *)
  val simplify_filter
    :  (Package_variable_name.t -> Variable_value.t option)
    -> OpamTypes.filter
    -> OpamTypes.filter

  (** Partially evaluate a filter, returning [`Skip] if definitely false. *)
  val partial_eval_filter
    :  OpamTypes.filter option
    -> [ `Skip | `Filter of OpamTypes.filter option ]

  (** Convert opam filter to Blang. *)
  val filter_to_blang
    :  package:OpamPackage.t
    -> loc:Loc.t
    -> OpamTypes.filter
    -> (Slang.Blang.t, User_message.t) result

  (** Convert opam commands to actions. *)
  val opam_commands_to_actions
    :  get_solver_var:(Package_variable_name.t -> Variable_value.t option)
    -> loc:Loc.t
    -> package:OpamPackage.t
    -> OpamTypes.command list
    -> (Action.t list, User_message.t) result

  (** Combine a list of actions into a single action. *)
  val make_action : Action.t list -> Action.t option

  (** Extract all package names from an opam dependency formula. *)
  val extract_dep_names : OpamTypes.filtered_formula -> OpamPackage.Name.t list

  (** Convert opam depexts to Depexts.t list. *)
  val depexts_to_list
    :  package:OpamPackage.t
    -> (OpamSysPkg.Set.t * OpamTypes.filter) list
    -> (Depexts.t list, User_message.t) result

  (** Convert opam env updates to dune's env update format. *)
  val opam_env_update_to_env_update
    :  string * Action.Env_update.Op.t * string * 'a
    -> String_with_vars.t Action.Env_update.t

  (** Convert substitution files to actions. *)
  val substs_to_actions : OpamFile.OPAM.t -> Action.t list

  (** Convert patches to actions. *)
  val patches_to_actions
    :  package:OpamPackage.t
    -> OpamFile.OPAM.t
    -> (Action.t list, User_message.t) result

  (** Wrap an action with build environment updates from opam file. *)
  val wrap_with_build_env : OpamFile.OPAM.t -> Action.t -> Action.t

  (** Extract extra sources from opam file. *)
  val extra_sources_of_opam_file : OpamFile.OPAM.t -> (Path.Local.t * Source.t) list

  (** Extract source URL from opam file. *)
  val source_of_opam_file : OpamFile.OPAM.t -> Source.t option
end

(** [of_opam_file ~name ~version ~source ~opam ()] constructs a [t] from opam file
    metadata. This extracts:
    - build_command from [OpamFile.OPAM.build]
    - install_command from [OpamFile.OPAM.install]
    - depends from [OpamFile.OPAM.depends]
    - depexts from [OpamFile.OPAM.depexts]

    @param source Source location for the package (local path or fetch URL).
    Required for source_rules to copy/fetch the source to the build directory. *)
val of_opam_file
  :  name:Package_name.t
  -> version:Package_version.t
  -> source:Source.t
  -> opam:OpamFile.OPAM.t
  -> unit
  -> (t, User_message.t) result
