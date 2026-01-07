(* Package specification used by dune's package management.
   A Pkg.t can come from a lock file or from a vendor directory's opam file. *)

open Import
module Digest_feed = Dune_digest.Feed

module Solver_env_disjunction = struct
  (* A disjunction of solver envs consisting of only platform-specific solver variables. *)
  type t = Solver_env.t list

  let singleton solver_env : t =
    let solver_env_with_only_platform_specific_vars =
      Solver_env.remove_all_except_platform_specific solver_env
    in
    [ solver_env_with_only_platform_specific_vars ]
  ;;

  let to_dyn = Dyn.list Solver_env.to_dyn

  let equal a b =
    let a = List.sort a ~compare:Solver_env.compare in
    let b = List.sort b ~compare:Solver_env.compare in
    List.equal Solver_env.equal a b
  ;;

  let hash t = List.hash Solver_env.hash t
  let digest_feed = Digest_feed.list Solver_env.digest_feed

  let encode t =
    let open Encoder in
    list sexp (List.map ~f:Solver_env.encode t)
  ;;

  let decode =
    let open Decoder in
    enter @@ repeat (enter Solver_env.decode)
  ;;

  (* [matches_platform t ~platform] is true iff there exists a solver env in
     [t] whose bindings are a subset of those in [platform] *)
  let matches_platform t ~platform = List.exists t ~f:(Solver_env.is_subset ~of_:platform)
end

module Conditional = struct
  type 'a t =
    { condition : Solver_env_disjunction.t
    ; value : 'a
    }

  let make solver_env value =
    let condition = Solver_env_disjunction.singleton solver_env in
    { condition; value }
  ;;

  let equal value_equal { condition; value } t =
    Solver_env_disjunction.equal condition t.condition && value_equal value t.value
  ;;

  let hash { condition; value } ~f =
    Tuple.T2.hash Solver_env_disjunction.hash f (condition, value)
  ;;

  let digest_feed feed_value =
    Digest_feed.tuple2 Solver_env_disjunction.digest_feed feed_value
    |> Digest_feed.contramap ~f:(fun { condition; value } -> condition, value)
  ;;

  let to_dyn value_to_dyn { condition; value } =
    Dyn.record
      [ "condition", Solver_env_disjunction.to_dyn condition
      ; "value", value_to_dyn value
      ]
  ;;

  let decode value_decode =
    let open Decoder in
    enter
      (let+ condition = Solver_env_disjunction.decode
       and+ value = value_decode in
       { condition; value })
  ;;

  let encode encode_value { condition; value } =
    Dune_lang.List [ Solver_env_disjunction.encode condition; encode_value value ]
  ;;

  let map t ~f = { t with value = f t.value }

  let evaluate { condition; value } ~platform =
    if Solver_env_disjunction.matches_platform condition ~platform
    then Some value
    else None
  ;;
end

module Conditional_choice = struct
  type 'a t = 'a Conditional.t list

  let empty = []
  let singleton condition value = [ Conditional.make condition value ]

  (* A choice where a given value will be chosen unconditionally. This is only
     used to help support both portable and non-portable lockdirs with the same
     code. Eventually when portable lockdirs become the default (and hence the
     only choice) this function will likely be removed. *)
  let singleton_all_platforms value = [ { Conditional.condition = []; value } ]
  let equal value_equal = List.equal (Conditional.equal value_equal)
  let hash t ~f = List.hash (Conditional.hash ~f) t
  let digest_feed feed_value = Digest_feed.list (Conditional.digest_feed feed_value)
  let map ~f = List.map ~f:(Conditional.map ~f)
  let to_dyn value_to_dyn = Dyn.list (Conditional.to_dyn value_to_dyn)

  let choose_for_platform t ~platform =
    List.find_map t ~f:(Conditional.evaluate ~platform)
  ;;

  let exists t ~f =
    List.exists t ~f:(fun (conditional : _ Conditional.t) -> f conditional.value)
  ;;

  let find_platform t ~f =
    List.exists t ~f:(fun (conditional : _ Conditional.t) ->
      f conditional.value && not (List.is_empty conditional.condition))
  ;;

  (* Append a [_ Conditional.t] to the choice [t]. If the new conditional has a
     value equal to the previous element of the choice (according to
     [value_equal]) then combine the two conditionals into one. *)
  let append_combining_equal_values
        ~value_equal
        (t : 'a t)
        ({ Conditional.condition; value } as conditional)
    =
    match List.rev t with
    | [] -> [ conditional ]
    | x :: xs ->
      if value_equal x.Conditional.value value
      then List.rev ({ Conditional.condition = x.condition @ condition; value } :: xs)
      else t @ [ conditional ]
  ;;

  let merge_combining_conditions ~value_equal a b =
    List.fold_left b ~init:a ~f:(append_combining_equal_values ~value_equal)
  ;;

  let get_value_ensuring_at_most_one_choice t =
    if List.length t > 1
    then
      Code_error.raise
        "Expected at most one choice"
        [ ( "conditions"
          , List.map t ~f:(fun { Conditional.condition; _ } -> condition)
            |> Dyn.list Solver_env_disjunction.to_dyn )
        ];
    List.hd_opt t |> Option.map ~f:(fun { Conditional.value; _ } -> value)
  ;;
end

module Info = struct
  type t =
    { name : Package_name.t
    ; version : Package_version.t
    ; dev : bool
    ; avoid : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  let equal { name; version; dev; avoid; source; extra_sources } t =
    Package_name.equal name t.name
    && Package_version.equal version t.version
    && Bool.equal dev t.dev
    && Bool.equal avoid t.avoid
    && Option.equal Source.equal source t.source
    && List.equal
         (Tuple.T2.equal Path.Local.equal Source.equal)
         extra_sources
         t.extra_sources
  ;;

  let hash { name; version; dev; avoid; source; extra_sources } =
    Poly.hash
      ( Package_name.hash name
      , Package_version.hash version
      , Bool.hash dev
      , Bool.hash avoid
      , Option.hash Source.hash source
      , List.hash (Tuple.T2.hash Path.Local.hash Source.hash) extra_sources )
  ;;

  let digest_feed hasher { name; version; dev; avoid; source; extra_sources } =
    Package_name.digest_feed hasher name;
    Package_version.digest_feed hasher version;
    Digest_feed.bool hasher dev;
    Digest_feed.bool hasher avoid;
    Digest_feed.option Source.digest_feed hasher source;
    Digest_feed.list
      (Digest_feed.tuple2 Digest_feed.generic Source.digest_feed)
      hasher
      extra_sources
  ;;

  let to_dyn { name; version; dev; avoid; source; extra_sources } =
    Dyn.record
      [ "name", Package_name.to_dyn name
      ; "version", Package_version.to_dyn version
      ; "dev", Dyn.bool dev
      ; "avoid", Dyn.bool avoid
      ; "source", Dyn.option Source.to_dyn source
      ; "extra_sources", Dyn.list (Dyn.pair Path.Local.to_dyn Source.to_dyn) extra_sources
      ]
  ;;

  let default_version = Package_version.dev

  let variables t =
    let module Variable = OpamVariable in
    Package_variable_name.Map.of_list_exn
      [ Package_variable_name.name, Variable.S (Package_name.to_string t.name)
      ; Package_variable_name.version, S (Package_version.to_string t.version)
      ; Package_variable_name.dev, B t.dev
      ]
  ;;
end

module Build_command = struct
  type t =
    | Action of Action.t
    | Dune

  let equal x y =
    match x, y with
    | Dune, Dune -> true
    | Action x, Action y -> Action.equal x y
    | _, _ -> false
  ;;

  let to_dyn = function
    | Dune -> Dyn.variant "Dune" []
    | Action a -> Dyn.variant "Action" [ Action.to_dyn a ]
  ;;
end

module Dependency = struct
  type t = Package_name.t

  let equal = Package_name.equal
  let to_dyn = Package_name.to_dyn
end

module Dependencies = struct
  type t = Dependency.t list

  let equal = List.equal Dependency.equal
  let to_dyn = Dyn.list Dependency.to_dyn
end

module Depexts = struct
  module Enabled_if = struct
    type t =
      [ `Always
      | `Conditional of Slang.Blang.t
      ]

    let to_dyn = function
      | `Always -> Dyn.variant "Always" []
      | `Conditional condition ->
        Dyn.variant "Conditional" [ Slang.Blang.to_dyn condition ]
    ;;

    let equal a b =
      match a, b with
      | `Always, `Always -> true
      | `Conditional a, `Conditional b -> Slang.Blang.equal a b
      | _, _ -> false
    ;;
  end

  type t =
    { external_package_names : string list
    ; enabled_if : Enabled_if.t
    }

  let to_dyn { external_package_names; enabled_if } =
    Dyn.record
      [ "external_package_names", Dyn.list Dyn.string external_package_names
      ; "enabled_if", Enabled_if.to_dyn enabled_if
      ]
  ;;

  let equal { external_package_names; enabled_if } t =
    List.equal String.equal external_package_names t.external_package_names
    && Enabled_if.equal enabled_if t.enabled_if
  ;;
end

type t =
  { build_command : Build_command.t Conditional_choice.t
  ; install_command : Action.t Conditional_choice.t
  ; depends : Dependencies.t Conditional_choice.t
  ; post_depends : Dependencies.t Conditional_choice.t
    (* Post deps are installed WITH the package but don't affect build order.
       They're tracked separately to avoid creating false dependency cycles. *)
  ; depexts : Depexts.t list
  ; info : Info.t
  ; exported_env : String_with_vars.t Action.Env_update.t list
  ; enabled_on_platforms : Solver_env_disjunction.t
  }

let equal
      { build_command
      ; install_command
      ; depends
      ; post_depends
      ; depexts
      ; info
      ; exported_env
      ; enabled_on_platforms
      }
      t
  =
  Conditional_choice.equal Build_command.equal build_command t.build_command
  && Conditional_choice.equal Action.equal_no_locs install_command t.install_command
  && Conditional_choice.equal Dependencies.equal depends t.depends
  && Conditional_choice.equal Dependencies.equal post_depends t.post_depends
  && List.equal Depexts.equal depexts t.depexts
  && Info.equal info t.info
  && List.equal
       (Action.Env_update.equal String_with_vars.equal)
       exported_env
       t.exported_env
  && Solver_env_disjunction.equal enabled_on_platforms t.enabled_on_platforms
;;

let hash
      { build_command
      ; install_command
      ; depends
      ; post_depends
      ; depexts
      ; info
      ; exported_env
      ; enabled_on_platforms
      }
  =
  Poly.hash
    ( Conditional_choice.hash ~f:Poly.hash build_command
    , Conditional_choice.hash ~f:Poly.hash install_command
    , Conditional_choice.hash ~f:Poly.hash depends
    , Conditional_choice.hash ~f:Poly.hash post_depends
    , depexts
    , Info.hash info
    , exported_env
    , Solver_env_disjunction.hash enabled_on_platforms )
;;

let digest_feed
      hasher
      { build_command
      ; install_command
      ; depends
      ; post_depends
      ; depexts
      ; info
      ; exported_env
      ; enabled_on_platforms
      }
  =
  Conditional_choice.digest_feed Digest_feed.generic hasher build_command;
  Conditional_choice.digest_feed Digest_feed.generic hasher install_command;
  Conditional_choice.digest_feed Digest_feed.generic hasher depends;
  Conditional_choice.digest_feed Digest_feed.generic hasher post_depends;
  Digest_feed.generic hasher depexts;
  Info.digest_feed hasher info;
  Digest_feed.generic hasher exported_env;
  Solver_env_disjunction.digest_feed hasher enabled_on_platforms
;;

let to_dyn
      { build_command
      ; install_command
      ; depends
      ; post_depends
      ; depexts
      ; info
      ; exported_env
      ; enabled_on_platforms
      }
  =
  Dyn.record
    [ "build_command", Conditional_choice.to_dyn Build_command.to_dyn build_command
    ; "install_command", Conditional_choice.to_dyn Action.to_dyn install_command
    ; "depends", Conditional_choice.to_dyn Dependencies.to_dyn depends
    ; "post_depends", Conditional_choice.to_dyn Dependencies.to_dyn post_depends
    ; "depexts", Dyn.list Depexts.to_dyn depexts
    ; "info", Info.to_dyn info
    ; ( "exported_env"
      , Dyn.list (Action.Env_update.to_dyn String_with_vars.to_dyn) exported_env )
    ; "enabled_on_platforms", Solver_env_disjunction.to_dyn enabled_on_platforms
    ]
;;

let is_enabled_on_platform t ~platform =
  (* XXX: currently treat empty lists of platforms as if the platform is
     enabled on all platforms to simplify supporting both portable and
     non-portable lockdirs with the same code. *)
  List.is_empty t.enabled_on_platforms
  || Solver_env_disjunction.matches_platform t.enabled_on_platforms ~platform
;;
