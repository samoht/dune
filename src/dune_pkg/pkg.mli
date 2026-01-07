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
end

module Build_command : sig
  type t =
    | Action of Action.t
    | Dune (** pinned dune packages do not need to define a command *)

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module Dependency : sig
  type t = Package_name.t

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module Dependencies : sig
  type t = Dependency.t list

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module Depexts : sig
  type t =
    { external_package_names : string list
    ; enabled_if : [ `Always | `Conditional of Slang.Blang.t ]
    }

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

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
val is_enabled_on_platform : t -> platform:Solver_env.t -> bool
