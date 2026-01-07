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
