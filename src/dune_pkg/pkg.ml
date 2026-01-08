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
    (* Normalize: empty solver env means "all platforms", same as empty list *)
    if Solver_env.is_empty solver_env_with_only_platform_specific_vars
    then []
    else [ solver_env_with_only_platform_specific_vars ]
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
     [t] whose bindings are a subset of those in [platform].
     An empty list means "all platforms" and always matches. *)
  let matches_platform t ~platform =
    match t with
    | [] -> true (* Empty condition means "all platforms" *)
    | _ -> List.exists t ~f:(Solver_env.is_subset ~of_:platform)
  ;;
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

  let remove_locs t =
    { t with
      source = Option.map ~f:Source.remove_locs t.source
    ; extra_sources =
        List.map t.extra_sources ~f:(fun (local, source) ->
          local, Source.remove_locs source)
    }
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

  let remove_locs = function
    | Dune -> Dune
    | Action a -> Action (Action.remove_locs a)
  ;;

  let to_dyn = function
    | Dune -> Dyn.variant "Dune" []
    | Action a -> Dyn.variant "Action" [ Action.to_dyn a ]
  ;;

  module Fields = struct
    let dune = "dune"
    let action = "action"
    let build = "build"
  end

  let encode_non_portable t =
    let open Encoder in
    match t with
    | None -> field_o Fields.build Encoder.unit None
    | Some Dune -> field_b Fields.dune true
    | Some (Action a) -> field Fields.build Action.encode a
  ;;

  let encode_portable t =
    let open Encoder in
    Dune_lang.List
      (record_fields
         [ (match t with
            | Dune -> field_b Fields.dune true
            | Action a -> field Fields.action Action.encode a)
         ])
  ;;

  let decode_portable =
    let open Decoder in
    enter
    @@ fields
    @@ fields_mutually_exclusive
         [ ( Fields.action
           , let+ pkg = Action.decode_pkg in
             Action pkg )
         ; ( Fields.dune
           , let+ () = return () in
             Dune )
         ]
  ;;
end

module Dependency = struct
  type t =
    { loc : Loc.t
    ; name : Package_name.t
    }

  let equal { loc; name } t = Loc.equal loc t.loc && Package_name.equal name t.name
  let remove_locs { name; loc = _ } = { name; loc = Loc.none }

  let to_dyn { loc; name } =
    Dyn.record [ "loc", Loc.to_dyn_hum loc; "name", Package_name.to_dyn name ]
  ;;

  let decode =
    let open Decoder in
    let+ loc, name = located Package_name.decode in
    { loc; name }
  ;;

  let encode { name; loc = _ } = Package_name.encode name
end

module Dependencies = struct
  type t = Dependency.t list

  let equal = List.equal Dependency.equal
  let remove_locs = List.map ~f:Dependency.remove_locs
  let to_dyn = Dyn.list Dependency.to_dyn
  let encode t = Dune_lang.List (List.map t ~f:Dependency.encode)
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

    let remove_locs = function
      | `Always -> `Always
      | `Conditional condition -> `Conditional (Slang.Blang.remove_locs condition)
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

  let remove_locs t = { t with enabled_if = Enabled_if.remove_locs t.enabled_if }

  let encode { external_package_names; enabled_if } =
    let open Encoder in
    let external_package_names = list string external_package_names in
    match enabled_if with
    | `Always -> external_package_names
    | `Conditional condition ->
      Dune_lang.List [ external_package_names; Slang.Blang.encode condition ]
  ;;

  let decode =
    let open Decoder in
    enter
      ((let+ external_package_names = enter @@ repeat string
        and+ condition = Slang.Blang.decode in
        { external_package_names; enabled_if = `Conditional condition })
       <|>
       let+ external_package_names = repeat string in
       { external_package_names; enabled_if = `Always })
  ;;
end

module Conditional_choice_or_all_platforms = struct
  (* Either a choice of value or a single value to use in all cases. The
     [All_platforms _] case will be used to reduce the verbosity of lockfiles
     where a value is the same for all solver environments under which the
     lockdir is valid. This type is a convenience for encoding and decoding
     lockfiles but doesn't appear in the representation of a package. *)
  type 'a t =
    | Choice of 'a Conditional_choice.t
    | All_platforms of 'a

  let of_conditional_choice ~solved_for_platforms = function
    | [] -> None
    | [ { Conditional.condition; value } ] as choice ->
      if Solver_env_disjunction.equal condition solved_for_platforms
      then Some (All_platforms value)
      else Some (Choice choice)
    | choice -> Some (Choice choice)
  ;;

  let to_conditional_choice ~solved_for_platforms = function
    | Choice choice -> choice
    | All_platforms value -> [ { Conditional.value; condition = solved_for_platforms } ]
  ;;

  let decode decode_value =
    let open Decoder in
    sum
      [ ( "choice"
        , let+ choice = repeat (Conditional.decode decode_value) in
          Choice choice )
      ; ( "all_platforms"
        , let+ value = decode_value in
          All_platforms value )
      ]
  ;;

  let encode encode_value t =
    let open Encoder in
    match t with
    | Choice choice ->
      Dune_lang.List
        (string "choice" :: List.map ~f:(Conditional.encode encode_value) choice)
    | All_platforms value -> Dune_lang.List [ string "all_platforms"; encode_value value ]
  ;;

  let encode_field ~solved_for_platforms name encode_value conditional_choice =
    let open Encoder in
    field_o
      name
      (encode encode_value)
      (of_conditional_choice ~solved_for_platforms conditional_choice)
  ;;
end

module Enabled_on_platforms = struct
  (* A package's availability on various platforms. Either it's available on
     all platforms the lockdir was solved for or it's only available on a
     subset of these platforms. *)
  type t =
    | All
    | Only of Solver_env_disjunction.t

  let of_solver_env_disjunction ~solved_for_platforms solver_env_disjunction =
    if Solver_env_disjunction.equal solver_env_disjunction solved_for_platforms
    then All
    else Only solver_env_disjunction
  ;;

  let to_solver_env_disjunction ~solved_for_platforms = function
    | All -> solved_for_platforms
    | Only solver_envs -> solver_envs
  ;;

  let encode t =
    let open Encoder in
    match t with
    | All -> string "all"
    | Only solver_envs ->
      Dune_lang.List (string "only" :: List.map ~f:Solver_env.encode solver_envs)
  ;;

  let decode =
    let open Decoder in
    sum
      [ "all", return All
      ; ( "only"
        , let+ solver_envs = repeat (enter Solver_env.decode) in
          Only solver_envs )
      ]
  ;;
end

let decode_build_command_fields ~portable_lock_dir =
  let open Decoder in
  let parse_action =
    if portable_lock_dir
    then Conditional_choice_or_all_platforms.decode Build_command.decode_portable
    else
      let+ action = Action.decode_pkg in
      Conditional_choice_or_all_platforms.Choice
        (Conditional_choice.singleton_all_platforms (Build_command.Action action))
  in
  fields_mutually_exclusive
    ~default:None
    [ ( Build_command.Fields.build
      , let+ action = parse_action in
        Some action )
    ; ( Build_command.Fields.dune
      , let+ () = return () in
        Some
          (Conditional_choice_or_all_platforms.Choice
             (Conditional_choice.singleton_all_platforms Build_command.Dune)) )
    ]
;;

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

let remove_locs
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
  { info = Info.remove_locs info
  ; exported_env =
      List.map exported_env ~f:(Action.Env_update.map ~f:String_with_vars.remove_locs)
  ; depends = Conditional_choice.map depends ~f:Dependencies.remove_locs
  ; post_depends = Conditional_choice.map post_depends ~f:Dependencies.remove_locs
  ; depexts = List.map depexts ~f:Depexts.remove_locs
  ; build_command = Conditional_choice.map build_command ~f:Build_command.remove_locs
  ; install_command = Conditional_choice.map install_command ~f:Action.remove_locs
  ; enabled_on_platforms
  }
;;

let is_enabled_on_platform t ~platform =
  (* XXX: currently treat empty lists of platforms as if the platform is
     enabled on all platforms to simplify supporting both portable and
     non-portable lockdirs with the same code. *)
  List.is_empty t.enabled_on_platforms
  || Solver_env_disjunction.matches_platform t.enabled_on_platforms ~platform
;;

let in_source_tree path =
  match (path : Path.t) with
  | In_source_tree s -> s
  | In_build_dir b ->
    (match Path.Build.explode b with
     (* Lock dir: _build/.locks/<ctx>/<lock-dir-name>/... *)
     | locks_dir :: _ctx :: lock_dir_components
       when String.equal locks_dir Dpath.Build.locks_dir_basename ->
       Path.Source.L.relative Path.Source.root lock_dir_components
     | build_components ->
       Code_error.raise
         "Unexpected location of lock directory in build directory"
         [ "path", Path.Build.to_dyn b
         ; "build_components", Dyn.(list string) build_components
         ])
  | External e -> Workspace.dev_tool_path_to_source_dir e
;;

let compute_missing_checksum t ~pinned =
  let open Fiber.O in
  let+ source =
    match t.info.source with
    | None -> Fiber.return None
    | Some source ->
      Source.compute_missing_checksum source t.info.name ~pinned >>| Option.some
  in
  { t with info = { t.info with source } }
;;

module Fields = struct
  let version = "version"
  let build = "build"
  let install = "install"
  let depends = "depends"
  let post_depends = "post_depends"
  let depexts = "depexts"
  let source = "source"
  let dev = "dev"
  let avoid = "avoid"
  let exported_env = "exported_env"
  let extra_sources = "extra_sources"
  let enabled_on_platforms = "enabled_on_platforms"
end

let decode ~portable_lock_dir =
  let open Decoder in
  let parse_install_command =
    if portable_lock_dir
    then Conditional_choice_or_all_platforms.decode Action.decode_pkg
    else
      let+ action = Action.decode_pkg in
      Conditional_choice_or_all_platforms.Choice
        (Conditional_choice.singleton_all_platforms action)
  in
  let parse_depends =
    if portable_lock_dir
    then Conditional_choice_or_all_platforms.decode (enter @@ repeat Dependency.decode)
    else
      let+ depends = repeat Dependency.decode in
      Conditional_choice_or_all_platforms.Choice
        (Conditional_choice.singleton_all_platforms depends)
  in
  let parse_depexts =
    if portable_lock_dir
    then repeat Depexts.decode
    else
      let+ external_package_names = repeat string in
      [ { Depexts.external_package_names; enabled_if = `Always } ]
  in
  let empty_choice = Conditional_choice_or_all_platforms.Choice [] in
  enter
  @@ fields
  @@ let+ version = field Fields.version Package_version.decode
     and+ install_command =
       field ~default:empty_choice Fields.install parse_install_command
     and+ build_command = decode_build_command_fields ~portable_lock_dir
     and+ depends = field ~default:empty_choice Fields.depends parse_depends
     and+ post_depends = field ~default:empty_choice Fields.post_depends parse_depends
     and+ depexts = field ~default:[] Fields.depexts parse_depexts
     and+ source = field_o Fields.source Source.decode
     and+ dev = field_b Fields.dev
     and+ avoid = field_b Fields.avoid
     and+ exported_env =
       field Fields.exported_env ~default:[] (repeat Action.Env_update.decode)
     and+ extra_sources =
       field
         Fields.extra_sources
         ~default:[]
         (repeat (pair (plain_string Path.Local.parse_string_exn) Source.decode))
     and+ enabled_on_platforms =
       field
         Fields.enabled_on_platforms
         ~default:Enabled_on_platforms.All
         Enabled_on_platforms.decode
     in
     fun ~lock_dir ~solved_for_platforms name ->
       let install_command =
         Conditional_choice_or_all_platforms.to_conditional_choice
           ~solved_for_platforms
           install_command
       in
       let build_command =
         match build_command with
         | None -> []
         | Some build_command ->
           Conditional_choice_or_all_platforms.to_conditional_choice
             ~solved_for_platforms
             build_command
       in
       let depends =
         Conditional_choice_or_all_platforms.to_conditional_choice
           ~solved_for_platforms
           depends
       in
       let post_depends =
         Conditional_choice_or_all_platforms.to_conditional_choice
           ~solved_for_platforms
           post_depends
       in
       let info =
         let make_source f =
           lock_dir |> Path.to_absolute_filename |> Path.External.of_string |> f
         in
         let source = Option.map source ~f:make_source in
         let extra_sources =
           List.map extra_sources ~f:(fun (path, source) -> path, make_source source)
         in
         { Info.name; version; dev; avoid; source; extra_sources }
       in
       let enabled_on_platforms =
         Enabled_on_platforms.to_solver_env_disjunction
           ~solved_for_platforms
           enabled_on_platforms
       in
       { build_command
       ; depends
       ; post_depends
       ; depexts
       ; install_command
       ; info
       ; exported_env
       ; enabled_on_platforms
       }
;;

let encode_extra_source (local, source) : Dune_sexp.t =
  List
    [ Dune_sexp.atom_or_quoted_string (Path.Local.to_string local); Source.encode source ]
;;

let encode
      ~portable_lock_dir
      ~solved_for_platforms
      { build_command
      ; install_command
      ; depends
      ; post_depends
      ; depexts
      ; info = { Info.name = _; extra_sources; version; dev; avoid; source }
      ; exported_env
      ; enabled_on_platforms
      }
  =
  let open Encoder in
  let encode_deps_field name deps =
    let deps =
      match deps with
      | [ { Conditional.value = []; _ } ] ->
        (* Omit the dependencies field to reduce noise in the case
           where there is explicitly an empty list of dependencies. *)
        []
      | other -> other
    in
    Conditional_choice_or_all_platforms.encode_field
      ~solved_for_platforms
      name
      Dependencies.encode
      deps
  in
  let install_command, build_command, depends, post_depends, depexts, enabled_on_platforms
    =
    if portable_lock_dir
    then (
      let encode_field n v c =
        Conditional_choice_or_all_platforms.encode_field ~solved_for_platforms n v c
      in
      ( encode_field Fields.install Action.encode install_command
      , encode_field Fields.build Build_command.encode_portable build_command
      , encode_deps_field Fields.depends depends
      , encode_deps_field Fields.post_depends post_depends
      , field_l Fields.depexts Depexts.encode depexts
      , match
          Enabled_on_platforms.of_solver_env_disjunction
            ~solved_for_platforms
            enabled_on_platforms
        with
        | All ->
          (* Omit the field if it's enabled everywhere to reduce noise. The
             parser will assume [All] by default. *)
          []
        | other -> [ field Fields.enabled_on_platforms Enabled_on_platforms.encode other ]
      ))
    else
      ( field_o
          Fields.install
          Action.encode
          (Conditional_choice.get_value_ensuring_at_most_one_choice install_command)
      , Build_command.encode_non_portable
          (Conditional_choice.get_value_ensuring_at_most_one_choice build_command)
      , field_l
          Fields.depends
          Package_name.encode
          (Conditional_choice.get_value_ensuring_at_most_one_choice depends
           |> Option.value ~default:[]
           |> List.map ~f:(fun { Dependency.name; _ } -> name))
      , field_l
          Fields.post_depends
          Package_name.encode
          (Conditional_choice.get_value_ensuring_at_most_one_choice post_depends
           |> Option.value ~default:[]
           |> List.map ~f:(fun { Dependency.name; _ } -> name))
      , field_l
          Fields.depexts
          string
          (match depexts with
           | [] -> []
           | [ { Depexts.external_package_names; _ } ] -> external_package_names
           | _ ->
             Code_error.raise
               "When using non-portable lockdirs it's expected that at most a single set \
                of depexts will be stored in each lockfile."
               [ "depexts", Dyn.list Depexts.to_dyn depexts ])
      , [] )
  in
  record_fields
    ([ field Fields.version Package_version.encode version
     ; install_command
     ; build_command
     ; depends
     ; post_depends
     ; depexts
     ; field_o Fields.source Source.encode source
     ; field_b Fields.dev dev
     ; field_b Fields.avoid avoid
     ; field_l Fields.exported_env Action.Env_update.encode exported_env
     ; field_l Fields.extra_sources encode_extra_source extra_sources
     ]
     @ enabled_on_platforms)
;;

(* More general version of [files_dir] which works on generic paths *)
let files_dir package_name maybe_package_version ~lock_dir =
  (* TODO(steve): Once portable lockdirs are enabled by default, make the
     package version non-optional *)
  let extension = ".files" in
  match maybe_package_version with
  | None -> Path.relative lock_dir (Package_name.to_string package_name ^ extension)
  | Some package_version ->
    Path.relative
      lock_dir
      (Package_name.to_string package_name
       ^ "."
       ^ Package_version.to_string package_version
       ^ extension)
;;

let source_files_dir package_name maybe_package_version ~lock_dir =
  let source = in_source_tree lock_dir in
  let package_name = Package_name.to_string package_name in
  match maybe_package_version with
  | Some package_version ->
    Path.Source.relative
      source
      (sprintf "%s.%s.files" package_name (Package_version.to_string package_version))
  | None -> Path.Source.relative source (sprintf "%s.files" package_name)
;;

(* Combine the platform-specific parts of a pair of [t]s, raising a code
   error if the packages differ in any way apart from their platform-specific
   fields. *)
let merge_conditionals a b =
  let build_command =
    Conditional_choice.merge_combining_conditions
      ~value_equal:Build_command.equal
      a.build_command
      b.build_command
  in
  let install_command =
    Conditional_choice.merge_combining_conditions
      ~value_equal:Action.equal
      a.install_command
      b.install_command
  in
  let depends =
    Conditional_choice.merge_combining_conditions
      ~value_equal:Dependencies.equal
      a.depends
      b.depends
  in
  let post_depends =
    Conditional_choice.merge_combining_conditions
      ~value_equal:Dependencies.equal
      a.post_depends
      b.post_depends
  in
  let enabled_on_platforms = a.enabled_on_platforms @ b.enabled_on_platforms in
  let ret =
    { a with build_command; install_command; depends; post_depends; enabled_on_platforms }
  in
  if
    not
      (equal
         ret
         { b with
           build_command
         ; install_command
         ; depends
         ; post_depends
         ; enabled_on_platforms
         })
  then
    Code_error.raise
      "Packages differ in a non-platform-specific field"
      [ "package_1", to_dyn a; "package_2", to_dyn b ];
  ret
;;

(* Helper module for converting opam files to Pkg.t *)
module Opam_conversion = struct
  let is_valid_global_variable_name = function
    | "root" -> false
    | _ -> true
  ;;

  (* CR-rgrinberg: we need this validation in substitution actions as well *)
  (* build-id is allowed and will be computed from the package digest *)
  let is_valid_package_variable_name = function
    | "hash" | "misc" | "opam-version" | "depends" | "build" | "opamfile" -> false
    | _ -> true
  ;;

  let invalid_variable_error ~loc variable =
    User_error.make
      ~loc
      [ Pp.textf "Variable %S is not supported." (OpamVariable.to_string variable) ]
  ;;

  let opam_variable_to_slang ~loc packages variable =
    let variable_string = OpamVariable.to_string variable in
    let convert_with_package_name package_name =
      match is_valid_package_variable_name variable_string with
      | false -> Error (invalid_variable_error ~loc variable)
      | true ->
        let pform =
          let name = Package_variable_name.of_string variable_string in
          let scope : Package_variable.Scope.t =
            match package_name with
            | None -> Self
            | Some p -> Package (Package_name.of_opam_package_name p)
          in
          Package_variable.to_pform
            { Package_variable.name; scope; default_if_true = None }
        in
        Ok (Slang.pform pform)
    in
    match packages with
    | [] ->
      (match is_valid_global_variable_name variable_string with
       | false ->
         (* Note that there's no syntactic distinction between global variables
            and package variables in the current package. This check will prevent
            invalid global variable names from being used for package variables in the
            current package where the optional qualifier "_:" is omitted. *)
         Error (invalid_variable_error ~loc variable)
       | true ->
         (match Pform.Var.of_opam_global_variable_name variable_string with
          | Some global_var -> Ok (Slang.pform (Pform.Var global_var))
          | None -> convert_with_package_name None))
    | [ package_name ] -> convert_with_package_name package_name
    | many ->
      let open Result.O in
      let+ many = Result.List.map many ~f:convert_with_package_name in
      Slang.blang (Blang.And (List.map many ~f:(fun slang -> Blang.Expr slang)))
  ;;

  (* Handles the special case for packages whose names contain '+' characters
     where a special form of string interpolation is used. From the opam manual:
     Warning: if the package name contains a + character (e.g. conf-g++), their
     variables may only be accessed using opam 2.2 via string interpolation,
     with the following syntax:

       "%{?conf-g++:your-variable:}%"
  *)
  let desugar_special_string_interpolation_syntax
        ((packages, variable, string_converter) as fident)
    =
    match string_converter with
    | Some (package_and_variable, "")
      when List.is_empty packages && String.is_empty (OpamVariable.to_string variable) ->
      (match String.lsplit2 package_and_variable ~on:':' with
       | Some (package, variable) ->
         ( [ Some (OpamPackage.Name.of_string package) ]
         , OpamVariable.of_string variable
         , None )
       | None -> fident)
    | _ -> fident
  ;;

  let opam_fident_to_slang ~loc fident =
    let open Result.O in
    let packages, variable, string_converter =
      OpamFilter.desugar_fident fident |> desugar_special_string_interpolation_syntax
    in
    let+ slang = opam_variable_to_slang ~loc packages variable in
    match string_converter with
    | None -> slang
    | Some (then_, else_) ->
      (* The "else" case is also used when evaluating the condition would expand
         an undefined variable. The catch_undefined_var operator is used to
         convert expressions that throw undefined variable exceptions into false.
      *)
      let condition =
        Blang.Expr (Slang.catch_undefined_var slang ~fallback:(Slang.bool false))
      in
      Slang.if_ condition ~then_:(Slang.text then_) ~else_:(Slang.text else_)
  ;;

  let opam_raw_fident_to_slang ~loc raw_ident =
    OpamTypesBase.filter_ident_of_string raw_ident |> opam_fident_to_slang ~loc
  ;;

  let opam_string_to_slang ~package ~loc opam_string =
    Re.Seq.split_full OpamFilter.string_interp_regex opam_string
    |> Seq.map ~f:(function
      | `Text text -> Ok (Slang.text text)
      | `Delim group ->
        (match Re.Group.get group 0 with
         | "%%" -> Ok (Slang.text "%")
         | interp
           when String.is_prefix ~prefix:"%{" interp
                && String.is_suffix ~suffix:"}%" interp ->
           let ident = String.sub ~pos:2 ~len:(String.length interp - 4) interp in
           opam_raw_fident_to_slang ~loc ident
         | other ->
           Error
             (User_error.make
                ~loc
                [ Pp.textf
                    "Encountered malformed variable interpolation while processing \
                     commands for package %s."
                    (OpamPackage.to_string package)
                ; Pp.text "The variable interpolation:"
                ; Pp.text other
                ])))
    |> List.of_seq
    |> Result.List.all
    |> Result.map ~f:Slang.concat
  ;;

  (* Translate an Opam filter into Dune's "Slang" DSL. The main difference between
     the two languages is in their treatment of undefined package variables. In
     Opam filters, undefined variables take on the value <undefined> which
     is "falsey" in some contexts and propagates through boolean operators if
     their result could be affected by the <undefined> term. Slang doesn't have an
     <undefined> value but raises an exception when an undefined variable is
     expanded. There are two operators in Slang for handling exceptions:

     - "(has_undefined_var <arg>)" evaluates <arg>, discarding the result, and
       returns a boolean which is true iff evaluating <arg> failed due to an
       undefined variable
     - "(catch_undefined_var <value> <fallback>)" evaluates <value> and returns
       the result unless evaluation failed due to an undefined variable, in which
       case the result of <fallback> is returned

     These two Slang operators are used to emulate Opam's undefined value
     semantics.
  *)
  let rec filter_to_blang ~package ~loc filter =
    let filter_to_slang (filter : OpamTypes.filter) =
      match filter with
      | FString s -> opam_string_to_slang ~package ~loc s
      | FIdent fident -> opam_fident_to_slang ~loc fident
      | other ->
        Code_error.raise
          "The opam file parser should only allow identifiers and strings in places \
           where strings are expected"
          [ "package", Dyn.string (OpamPackage.to_string package)
          ; "full filter", Dyn.string (OpamFilter.to_string filter)
          ; "non-string filter", Dyn.string (OpamFilter.to_string other)
          ]
    in
    let open Result.O in
    match (filter : OpamTypes.filter) with
    | FBool true -> Ok Blang.Ast.true_
    | FBool false -> Ok Blang.Ast.false_
    | (FString _ | FIdent _) as slangable ->
      let+ slang = filter_to_slang slangable in
      Blang.Expr slang
    | FOp (lhs, op, rhs) ->
      let op = Package_dependency.Constraint.Op.of_opam op in
      let+ lhs = filter_to_slang lhs
      and+ rhs = filter_to_slang rhs in
      Blang.Compare (op, lhs, rhs)
    | FAnd (lhs, rhs) ->
      let+ lhs = filter_to_blang ~package ~loc lhs
      and+ rhs = filter_to_blang ~package ~loc rhs in
      Blang.Expr (Slang.and_absorb_undefined_var [ lhs; rhs ])
    | FOr (lhs, rhs) ->
      let+ lhs = filter_to_blang ~package ~loc lhs
      and+ rhs = filter_to_blang ~package ~loc rhs in
      Blang.Expr (Slang.or_absorb_undefined_var [ lhs; rhs ])
    | FNot f ->
      let+ blang = filter_to_blang ~package ~loc f in
      Blang.Not blang
    | FDefined f ->
      let+ blang = filter_to_blang ~package ~loc f in
      Blang.Not (Blang.Expr (Slang.has_undefined_var (Slang.blang blang)))
    | FUndef _ ->
      Code_error.raise
        "Encountered undefined filter which should not be possible since no filter \
         reduction has taken place."
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ]
  ;;

  (** Simplify a filter by partially evaluating it with solver variables. *)
  let simplify_filter get_solver_var =
    OpamFilter.partial_eval (fun var ->
      match OpamVariable.Full.scope var with
      | Global ->
        let name = OpamVariable.Full.variable var |> Package_variable_name.of_opam in
        if Package_variable_name.equal name Package_variable_name.with_test
        then
          (* We don't generate lockfiles for local packages, and we don't include
             test dependencies for non-local packages, so "with-test" always
             evaluates to "false". *)
          Some (B false)
        else get_solver_var name |> Option.map ~f:Variable_value.to_opam_variable_contents
      | _ -> None)
  ;;

  (** Partially evaluate a filter, returning [`Skip] if definitely false. *)
  let partial_eval_filter = function
    | None -> `Filter None
    | Some f ->
      let env = Fun.const None in
      (match OpamFilter.eval_to_bool env f with
       | exception Failure _ -> `Filter (Some f)
       | b -> if b then `Filter None else `Skip)
  ;;

  let opam_commands_to_actions
        ~get_solver_var
        ~loc
        ~package
        (commands : OpamTypes.command list)
    =
    let open Result.O in
    List.map commands ~f:(fun (args, filter) ->
      let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
      match partial_eval_filter filter with
      | `Skip -> Ok None
      | `Filter filter ->
        let* terms =
          List.filter_map args ~f:(fun ((simple_arg : OpamTypes.simple_arg), filter) ->
            let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
            match partial_eval_filter filter with
            | `Skip -> None
            | `Filter filter ->
              let slang =
                let+ slang =
                  match simple_arg with
                  | CString s -> opam_string_to_slang ~package ~loc s
                  | CIdent ident -> opam_raw_fident_to_slang ~loc ident
                in
                Slang.simplify slang
              in
              Some
                (let+ slang =
                   match filter with
                   | None -> slang
                   | Some filter ->
                     let+ filter_blang =
                       filter_to_blang ~package ~loc filter >>| Slang.simplify_blang
                     and+ slang = slang in
                     let filter_blang_handling_undefined =
                       (* Wrap the blang filter so that if any undefined
                          variables are expanded while evaluating the filter,
                          the filter will return false. *)
                       let slang =
                         Slang.catch_undefined_var
                           (Slang.blang filter_blang)
                           ~fallback:(Slang.bool false)
                       in
                       Blang.Expr slang
                     in
                     Slang.when_ filter_blang_handling_undefined slang
                 in
                 Slang.simplify slang))
          |> Result.List.all
        in
        if List.is_empty terms
        then Ok None
        else
          let+ action =
            let action = Action.Run terms in
            match filter with
            | None -> Ok action
            | Some filter ->
              let+ condition =
                filter_to_blang ~package ~loc filter >>| Slang.simplify_blang
              in
              Action.When (condition, action)
          in
          Some action)
    |> Result.List.all
    |> Result.map ~f:List.filter_opt
  ;;

  let make_action = function
    | [] -> None
    | [ action ] -> Some action
    | actions -> Some (Action.Progn actions)
  ;;

  let extract_dep_names (formula : OpamTypes.filtered_formula) =
    let names = ref [] in
    OpamFormula.iter (fun (name, _condition) -> names := name :: !names) formula;
    !names
  ;;

  (* Translate the entire depexts field from the opam file into the lockfile by
     way of the slang dsl. Note that this preserves platform variables such as
     "os" and "os-distribution", which is different from how the "build",
     "install" and "depends" fields are treated, where platform variables are
     substituded with concrete values at solve time. There are many different
     Linux distributions and it's possible that some depexts will have different
     names on each distro and possibly also for different versions of the same
     distro. Users are not expected to solve their project for each
     distribution/version as that would take too long, instead opting to solve
     without a distro/version specified to create a package solution that's
     likely to work across all distros (except perhaps some unconventional
     distros such as Alpine). However even when using a general package solution,
     it's important that Dune is able to tell users the names of depexts tailored
     specifically for their current distro at build time. Thus, information
     mapping distro/version to package names must be preserved in lockfiles when
     solving. Opam allows depexts to be filtered by arbitrary filter expressions,
     which is why the slang dsl is needed as opposed to (say) a map from
     distro/version to depext name. *)
  let depexts_to_list ~package depexts =
    let open Result.O in
    List.map depexts ~f:(fun (sys_pkgs, filter) ->
      let external_package_names =
        OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
      in
      let+ condition =
        filter_to_blang ~package ~loc:Loc.none filter >>| Slang.simplify_blang
      in
      let enabled_if =
        if Slang.Blang.equal condition Slang.Blang.true_
        then `Always
        else `Conditional condition
      in
      { Depexts.external_package_names; enabled_if })
    |> Result.List.all
  ;;

  let opam_env_update_to_env_update (var, env_op, value_string, _)
    : String_with_vars.t Action.Env_update.t
    =
    { Action.Env_update.op = env_op
    ; var
    ; value = String_with_vars.make_text Loc.none value_string
    }
  ;;

  let substs_to_actions opam_file =
    OpamFile.OPAM.substs opam_file
    |> List.map ~f:(fun x ->
      let x = OpamFilename.Base.to_string x in
      let input = String_with_vars.make_text Loc.none (x ^ ".in") in
      let output = String_with_vars.make_text Loc.none x in
      Action.Substitute (input, output))
  ;;

  let patches_to_actions ~package opam_file =
    let open Result.O in
    OpamFile.OPAM.patches opam_file
    |> List.map ~f:(fun (basename, filter) ->
      let action =
        Action.Patch
          (String_with_vars.make_text Loc.none (OpamFilename.Base.to_string basename))
      in
      match filter with
      | None -> Ok action
      | Some filter ->
        let+ blang =
          filter_to_blang ~package ~loc:Loc.none filter >>| Slang.simplify_blang
        in
        Action.When (blang, action))
    |> Result.List.all
  ;;

  let wrap_with_build_env opam_file action =
    let env_update =
      OpamFile.OPAM.build_env opam_file |> List.map ~f:opam_env_update_to_env_update
    in
    match env_update with
    | [] -> action
    | env_update -> Action.Withenv (env_update, action)
  ;;

  let extra_sources_of_opam_file opam_file =
    OpamFile.OPAM.extra_sources opam_file
    |> List.map ~f:(fun (opam_basename, opam_url) ->
      ( Path.Local.of_string (OpamFilename.Base.to_string opam_basename)
      , let url = Loc.none, OpamFile.URL.url opam_url in
        let checksum =
          match OpamFile.URL.checksum opam_url with
          | [] -> None
          | checksum :: _ -> Some (Loc.none, Checksum.of_opam_hash checksum)
        in
        { Source.url; checksum } ))
  ;;

  let source_of_opam_file opam_file =
    OpamFile.OPAM.url opam_file
    |> Option.map ~f:(fun (url : OpamFile.URL.t) ->
      let checksum =
        OpamFile.URL.checksum url
        |> List.hd_opt
        |> Option.map ~f:(fun hash -> Loc.none, Checksum.of_opam_hash hash)
      in
      let url = Loc.none, OpamFile.URL.url url in
      { Source.url; checksum })
  ;;
end

let of_opam_file ~name ~version ~source ~opam () =
  let open Result.O in
  let loc = Loc.none in
  let package =
    OpamPackage.create
      (Package_name.to_opam_package_name name)
      (Package_version.to_opam_package_version version)
  in
  (* For simple use cases, use a no-op solver var function *)
  let get_solver_var _ = None in
  let* build_actions =
    Opam_conversion.opam_commands_to_actions
      ~get_solver_var
      ~loc
      ~package
      (OpamFile.OPAM.build opam)
  in
  let* install_actions =
    Opam_conversion.opam_commands_to_actions
      ~get_solver_var
      ~loc
      ~package
      (OpamFile.OPAM.install opam)
  in
  let build_command =
    match Opam_conversion.make_action build_actions with
    | None -> Conditional_choice.empty
    | Some action ->
      Conditional_choice.singleton_all_platforms (Build_command.Action action)
  in
  let install_command =
    match Opam_conversion.make_action install_actions with
    | None -> Conditional_choice.empty
    | Some action -> Conditional_choice.singleton_all_platforms action
  in
  let dep_names = Opam_conversion.extract_dep_names (OpamFile.OPAM.depends opam) in
  let depends =
    let deps =
      dep_names
      |> List.filter_map ~f:(fun opam_name ->
        let name = Package_name.of_opam_package_name opam_name in
        (* Filter out dune - it's provided by the build system itself *)
        if Package_name.equal name (Package_name.of_string "dune")
        then None
        else Some { Dependency.loc = Loc.none; name })
    in
    Conditional_choice.singleton_all_platforms deps
  in
  let+ depexts = Opam_conversion.depexts_to_list ~package (OpamFile.OPAM.depexts opam) in
  let info =
    { Info.name
    ; version
    ; dev = true (* Packages from opam files are treated as dev packages *)
    ; avoid = List.mem (OpamFile.OPAM.flags opam) Pkgflag_AvoidVersion ~equal:Poly.equal
    ; source = Some source
    ; extra_sources = []
    }
  in
  let exported_env =
    OpamFile.OPAM.env opam |> List.map ~f:Opam_conversion.opam_env_update_to_env_update
  in
  { build_command
  ; install_command
  ; depends
  ; post_depends = Conditional_choice.empty
  ; depexts
  ; info
  ; exported_env
  ; enabled_on_platforms = []
  }
;;
