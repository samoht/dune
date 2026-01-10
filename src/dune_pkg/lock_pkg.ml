open Import

let add_self_to_filter_env package env variable =
  match OpamVariable.Full.scope variable with
  | Self | Package _ -> env variable
  | Global ->
    let var_name = Package_variable_name.of_opam (OpamVariable.Full.variable variable) in
    if Package_variable_name.(equal var_name name)
    then Some (OpamVariable.S (OpamPackage.Name.to_string (OpamPackage.name package)))
    else if Package_variable_name.(equal var_name version)
    then Some (S (OpamPackage.Version.to_string (OpamPackage.version package)))
    else env variable
;;

let opam_package_to_lock_file_pkg
      solver_env
      stats_updater
      version_by_package_name
      opam_package
      ~pinned
      resolved_package
      ~portable_lock_dir
      ~allow_missing_deps
  =
  let open Result.O in
  let name = Package_name.of_opam_package_name (OpamPackage.name opam_package) in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let opam_file = Resolved_package.opam_file resolved_package in
  let loc = Resolved_package.loc resolved_package in
  let extra_sources =
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
  in
  let info =
    let url = OpamFile.OPAM.url opam_file in
    let source =
      Option.map url ~f:(fun (url : OpamFile.URL.t) ->
        let checksum =
          OpamFile.URL.checksum url
          |> List.hd_opt
          |> Option.map ~f:(fun hash -> Loc.none, Checksum.of_opam_hash hash)
        in
        let url = Loc.none, OpamFile.URL.url url in
        { Source.url; checksum })
    in
    let dev =
      pinned
      ||
      match url with
      | None -> false
      | Some url -> List.is_empty (OpamFile.URL.checksum url)
    in
    let avoid = List.mem opam_file.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    { Pkg.Info.name; version; dev; avoid; source; extra_sources }
  in
  let depends, post_depends =
    let env =
      add_self_to_filter_env
        opam_package
        (Solver_env.add_sentinel_values_for_unset_platform_vars solver_env
         |> Solver_env.to_env)
    in
    let regular_deps, post_deps =
      if allow_missing_deps
      then (
        (* When deriving from a lock file, skip missing deps - they were
           filtered out during solving (optional deps, virtual packages, etc.) *)
        let { Resolve_opam_formula.regular; post } =
          Resolve_opam_formula.filtered_formula_to_package_names_allow_missing
            ~with_test:false
            ~packages:version_by_package_name
            ~env
            opam_file.depends
        in
        regular, post)
      else (
        match
          Resolve_opam_formula.filtered_formula_to_package_names
            ~with_test:false
            ~packages:version_by_package_name
            ~env
            opam_file.depends
        with
        | Ok { regular; post } -> regular, post
        | Error (`Formula_could_not_be_satisfied hints) ->
          Code_error.raise
            "Dependencies of package can't be satisfied from packages in solution"
            [ "package", Dyn.string (opam_package |> OpamPackage.to_string)
            ; "hints", Dyn.list Resolve_opam_formula.Unsatisfied_formula_hint.to_dyn hints
            ])
    in
    let depopts =
      (* For depopts, always use permissive resolution - they're optional *)
      let { Resolve_opam_formula.regular; post = _ } =
        Resolve_opam_formula.filtered_formula_to_package_names_allow_missing
          ~with_test:false
          ~packages:version_by_package_name
          ~env
          opam_file.depopts
      in
      List.filter regular ~f:(fun package_name ->
        not (List.mem regular_deps package_name ~equal:Package_name.equal))
    in
    let make_deps names =
      names
      (* Filter out dune - it's provided by the build system itself *)
      |> List.filter ~f:(fun name -> not (Package_name.equal name Dune_dep.name))
      |> List.map ~f:(fun name -> { Pkg.Dependency.loc = Loc.none; name })
    in
    make_deps (regular_deps @ depopts), make_deps post_deps
  in
  let build_env action =
    let env_update =
      OpamFile.OPAM.build_env opam_file
      |> List.map ~f:Pkg.Opam_conversion.opam_env_update_to_env_update
    in
    match env_update with
    | [] -> action
    | env_update -> Action.Withenv (env_update, action)
  in
  let get_solver_var variable_name =
    Solver_stats.Updater.expand_variable stats_updater variable_name;
    Solver_env.get solver_env variable_name
  in
  let* build_command =
    if Resolved_package.dune_build resolved_package
    then Ok (Some Pkg.Build_command.Dune)
    else (
      let subst_step =
        OpamFile.OPAM.substs opam_file
        |> List.map ~f:(fun x ->
          let x = OpamFilename.Base.to_string x in
          let input = String_with_vars.make_text Loc.none (x ^ ".in") in
          let output = String_with_vars.make_text Loc.none x in
          Action.Substitute (input, output))
      in
      let+ patch_step =
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
              Pkg.Opam_conversion.filter_to_blang
                ~package:opam_package
                ~loc:Loc.none
                filter
              >>| Slang.simplify_blang
            in
            Action.When (blang, action))
        |> Result.List.all
      and+ build_step =
        Pkg.Opam_conversion.opam_commands_to_actions
          ~get_solver_var
          ~loc
          ~package:opam_package
          (OpamFile.OPAM.build opam_file)
      in
      List.concat [ subst_step; patch_step; build_step ]
      |> Pkg.Opam_conversion.make_action
      |> Option.map ~f:build_env
      |> Option.map ~f:(fun action -> Pkg.Build_command.Action action))
  in
  (* Some lockfile fields contain a choice of values predicated on a set of
     platform variables to allow lockfiles to be portable across different
     platforms. Each invocation of the solver produces a solution associated
     with a single set of platform variables (those in [solver_env]).
     [lockfile_field_choice value] creates a choice with a single possible
     value predicated by the platform variables in [solver_env]. The
     solver may be run multiple times, and the choice fields of lockfiles
     will be merged such that different values can be chosen on different
     platforms. *)
  let lockfile_field_choice value = Pkg.Conditional_choice.singleton solver_env value in
  let build_command =
    Option.map build_command ~f:lockfile_field_choice
    |> Option.value ~default:Pkg.Conditional_choice.empty
  in
  let* depexts =
    if portable_lock_dir
    then
      Pkg.Opam_conversion.depexts_to_list
        ~package:opam_package
        (OpamFile.OPAM.depexts opam_file)
    else (
      (* In the non-portable case, only include depexts for the current platform. *)
      let external_package_names =
        OpamFile.OPAM.depexts opam_file
        |> List.concat_map ~f:(fun (sys_pkgs, filter) ->
          let env = Solver_env.to_env solver_env in
          if OpamFilter.eval_to_bool ~default:false env filter
          then OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
          else [])
      in
      let depexts =
        if List.is_empty external_package_names
        then []
        else [ { Pkg.Depexts.external_package_names; enabled_if = `Always } ]
      in
      Ok depexts)
  in
  let+ install_command =
    Pkg.Opam_conversion.opam_commands_to_actions
      ~get_solver_var
      ~loc
      ~package:opam_package
      (OpamFile.OPAM.install opam_file)
    >>| Pkg.Opam_conversion.make_action
    >>| Option.map ~f:(fun action -> lockfile_field_choice (build_env action))
    >>| Option.value ~default:Pkg.Conditional_choice.empty
  in
  let exported_env =
    OpamFile.OPAM.env opam_file
    |> List.map ~f:Pkg.Opam_conversion.opam_env_update_to_env_update
  in
  let depends = lockfile_field_choice depends in
  let post_depends = lockfile_field_choice post_depends in
  let enabled_on_platforms =
    [ Solver_env.remove_all_except_platform_specific solver_env ]
  in
  { Pkg.build_command
  ; install_command
  ; depends
  ; post_depends
  ; depexts
  ; info
  ; exported_env
  ; enabled_on_platforms
  ; build_id = None (* Computed later after all packages are resolved *)
  }
;;

(* Internal implementation that optionally collects opam file contents.
   When ~with_opam_files is true, returns opam file content for each package.
   Preserves the dependency_hash from the source file for out-of-sync detection. *)
let file_to_lock_impl
      ~loc
      ~solver_env
      ~local_packages
      ~with_opam_files
      (file : Lock.File.t)
  =
  let open Fiber.O in
  let portable_lock_dir = Lock.File.is_portable file in
  let* repos, resolved_packages = Lock.File.derive ~loc file in
  (* Build version map from the package list. Virtual packages must be
     included in file.packages because they affect opam variable resolution
     (for example: pkg:installed). *)
  let version_by_package_name =
    let dune_version = Package_version.of_opam_package_version Dune_dep.version in
    List.fold_left
      file.packages
      ~init:(Package_name.Map.singleton Dune_dep.name dune_version)
      ~f:(fun acc { Lock.File.Package_entry.name; version; platforms = _ } ->
        Package_name.Map.set acc name version)
  in
  let stats_updater = Solver_stats.Updater.init () in
  (* Convert each resolved package to Pkg.t, optionally collecting opam files *)
  let+ pkgs_with_opam =
    Fiber.parallel_map
      resolved_packages
      ~f:(fun (name, version, _platforms, resolved_package) ->
        (* TODO: Use platforms to set enabled_on_platforms on the pkg *)
        let opam_package =
          OpamPackage.create
            (Package_name.to_opam_package_name name)
            (Package_version.to_opam_package_version version)
        in
        let opam_content =
          if with_opam_files
          then
            Some
              ( name
              , Resolved_package.opam_file resolved_package
                |> OpamFile.OPAM.write_to_string )
          else None
        in
        match
          opam_package_to_lock_file_pkg
            solver_env
            stats_updater
            version_by_package_name
            opam_package
            ~pinned:false
            resolved_package
            ~portable_lock_dir
            ~allow_missing_deps:true
        with
        | Ok pkg -> Fiber.return (pkg, opam_content)
        | Error msg -> User_error.raise [ User_message.pp msg ])
  in
  let pkgs, opam_files_opts = List.split pkgs_with_opam in
  let opam_files = List.filter_map opam_files_opts ~f:Fun.id in
  let packages =
    List.fold_left pkgs ~init:Package_name.Map.empty ~f:(fun acc (pkg : Pkg.t) ->
      Package_name.Map.add_exn acc pkg.info.name pkg)
  in
  let stats = Solver_stats.Updater.snapshot stats_updater in
  let expanded_solver_variable_bindings =
    Solver_stats.Expanded_variable_bindings.of_variable_set
      stats.expanded_variables
      solver_env
  in
  (* Identify the compiler package by checking conflict_class for ocaml-core-compiler.
     This matches the heuristic used in opam_solver.ml:package_kind. *)
  let ocaml =
    let ocaml_core_compiler = OpamPackage.Name.of_string "ocaml-core-compiler" in
    List.find_map
      resolved_packages
      ~f:(fun (name, _version, _platforms, resolved_package) ->
        let opam_file = Resolved_package.opam_file resolved_package in
        if
          List.mem
            opam_file.conflict_class
            ocaml_core_compiler
            ~equal:OpamPackage.Name.equal
        then Some (Loc.none, name)
        else None)
  in
  let lock =
    Lock.create_latest_version
      packages
      ~local_packages (* dependency_hash computed from local_packages during derivation *)
      ~ocaml
      ~repos:(Some repos)
      ~expanded_solver_variable_bindings
      ~solved_for_platform:(Some solver_env)
      ~portable_lock_dir
  in
  lock, opam_files
;;

let file_to_lock ~loc ~solver_env ~local_packages file =
  let open Fiber.O in
  let+ lock, _ =
    file_to_lock_impl ~loc ~solver_env ~local_packages ~with_opam_files:false file
  in
  lock
;;

let file_to_lock_with_opam_files ~loc ~solver_env ~local_packages file =
  file_to_lock_impl ~loc ~solver_env ~local_packages ~with_opam_files:true file
;;

(* Cache for derived single-file locks.
   Stores the derived directory-format lock in _build/ for fast subsequent reads. *)
module Single_file_cache = struct
  let cache_dir = Path.Build.relative Path.Build.root ".pkg-lock-cache"

  (* Cache path for a given source lock file *)
  let cache_path_for source_path =
    let hash = Dune_digest.string (Path.to_string source_path) |> Dune_digest.to_string in
    Path.Build.relative cache_dir hash
  ;;

  let mtime_file cache_path = Path.Build.relative cache_path ".source-mtime"

  let get_source_mtime path =
    match Path.stat path with
    | Ok { Unix.st_mtime; _ } -> Some st_mtime
    | Error _ -> None
  ;;

  let read_cached_mtime cache_path : float option =
    let mtime_path = Path.build (mtime_file cache_path) in
    try
      let contents = Io.read_file mtime_path in
      Some (Stdlib.float_of_string (String.trim contents))
    with
    | _ -> None
  ;;

  let write_mtime cache_path mtime =
    let mtime_path = Path.build (mtime_file cache_path) in
    Io.write_file mtime_path (Stdlib.string_of_float mtime)
  ;;

  let is_cache_valid source_path cache_path =
    match get_source_mtime source_path, read_cached_mtime cache_path with
    | Some source_mtime, Some cached_mtime -> source_mtime = cached_mtime
    | _ -> false
  ;;

  let write_cache ~portable_lock_dir cache_path (lock : Lock.t) source_mtime =
    let cache_build_path = Path.build cache_path in
    (* Remove any existing cache directory unconditionally - this is just a
       cache, not user data, so we don't need to validate it *)
    Path.rm_rf cache_build_path;
    (* Create cache directory *)
    Path.mkdir_p cache_build_path;
    (* Write lock files directly without going through Write_disk.prepare
       (which does validation that can fail on stale caches) *)
    Lock.file_contents_by_path ~portable_lock_dir lock
    |> List.iter ~f:(fun (filename, contents) ->
      let path = Path.relative cache_build_path filename in
      Option.iter (Path.parent path) ~f:Path.mkdir_p;
      let cst =
        List.map contents ~f:(fun sexp ->
          Dune_sexp.Ast.add_loc ~loc:Loc.none sexp |> Dune_sexp.Cst.concrete)
      in
      let pp = Dune_lang.Format.pp_top_sexps ~version:(3, 11) cst in
      Format.asprintf "%a" Pp.to_fmt pp |> Io.write_file path);
    (* Write source mtime marker *)
    write_mtime cache_path source_mtime
  ;;

  let read_from_cache cache_path =
    try Some (Lock.read_disk_exn (Path.build cache_path)) with
    | _ -> None
  ;;
end

let derive_and_cache_lock ~solver_env ~local_packages path =
  let cache_path = Single_file_cache.cache_path_for path in
  let file =
    Io.with_lexbuf_from_file path ~f:(fun lexbuf ->
      Lock.Metadata.parse_contents lexbuf ~f:(fun _lang -> Lock.File.decode))
  in
  let portable_lock_dir = Lock.File.is_portable file in
  let loc = Loc.in_file path in
  let open Fiber.O in
  let+ lock = file_to_lock ~loc ~solver_env ~local_packages file in
  (* Write to cache for next time (same format as upstream lock dir) *)
  (match Single_file_cache.get_source_mtime path with
   | Some mtime -> Single_file_cache.write_cache ~portable_lock_dir cache_path lock mtime
   | None -> ());
  lock
;;

let read_disk ~solver_env ~local_packages path =
  match Lock.detect_format path with
  | None ->
    User_error.raise
      [ Pp.textf
          "%s is not a valid lock directory or lock file"
          (Path.to_string_maybe_quoted path)
      ]
  | Some Lock.Directory ->
    (* Directory format - use sync reader wrapped in Fiber *)
    Fiber.return (Lock.read_disk_exn path)
  | Some Lock.File ->
    (* Single-file format - check cache first, then derive if needed.
       The dependency_hash is computed from local_packages during derivation. *)
    let cache_path = Single_file_cache.cache_path_for path in
    if Single_file_cache.is_cache_valid path cache_path
    then (
      (* Cache valid by mtime - try to read from cached directory format *)
      match Single_file_cache.read_from_cache cache_path with
      | Some lock -> Fiber.return lock
      | None ->
        (* Cache read failed (stale/corrupt) - re-derive *)
        derive_and_cache_lock ~solver_env ~local_packages path)
    else (* Cache miss - derive from opam repo and cache result *)
      derive_and_cache_lock ~solver_env ~local_packages path
;;

(* Like read_disk but also returns opam files for fetch.
   Only returns opam files for single-file format (where we derive from repo). *)
let read_disk_with_opam_files ~solver_env ~local_packages path =
  match Lock.detect_format path with
  | None ->
    User_error.raise
      [ Pp.textf
          "%s is not a valid lock directory or lock file"
          (Path.to_string_maybe_quoted path)
      ]
  | Some Lock.Directory ->
    (* Directory format - no opam files available (already processed) *)
    Fiber.return (Lock.read_disk_exn path, [])
  | Some Lock.File ->
    (* Single-file format - derive from repo and return opam files *)
    let open Fiber.O in
    let file =
      Io.with_lexbuf_from_file path ~f:(fun lexbuf ->
        Lock.Metadata.parse_contents lexbuf ~f:(fun _lang -> Lock.File.decode))
    in
    let portable_lock_dir = Lock.File.is_portable file in
    let loc = Loc.in_file path in
    let+ lock, opam_files =
      file_to_lock_with_opam_files ~loc ~solver_env ~local_packages file
    in
    let cache_path = Single_file_cache.cache_path_for path in
    (* Also cache the result for subsequent non-fetch reads *)
    (match Single_file_cache.get_source_mtime path with
     | Some mtime ->
       Single_file_cache.write_cache ~portable_lock_dir cache_path lock mtime
     | None -> ());
    lock, opam_files
;;

let pkg_of_local_opam_file ~loc:_ ~name ~version ~opam_file ~source =
  Pkg.of_opam_file ~name ~version ~source ~opam:opam_file ()
;;

(* Create a minimal Pkg.t with just name/version for quick reading.
   Used by commands like 'outdated' that only need package identification. *)
let minimal_pkg ~name ~version =
  { Pkg.build_command = Pkg.Conditional_choice.empty
  ; install_command = Pkg.Conditional_choice.empty
  ; depends = Pkg.Conditional_choice.empty
  ; post_depends = Pkg.Conditional_choice.empty
  ; depexts = []
  ; info =
      { Pkg.Info.name
      ; version
      ; dev = false
      ; avoid = false
      ; source = None
      ; extra_sources = []
      }
  ; exported_env = []
  ; enabled_on_platforms = [] (* empty = enabled on all platforms *)
  ; build_id = None
  }
;;

(* Convert single-file lock to Lock.t without deriving from opam repos.
   Creates minimal Pkg.t entries with just name/version information.
   Used by commands that only need to identify packages, not build them. *)
let file_to_lock_minimal (file : Lock.File.t) ~local_packages =
  let packages =
    List.fold_left
      file.packages
      ~init:Package_name.Map.empty
      ~f:(fun acc { Lock.File.Package_entry.name; version; platforms = _ } ->
        let pkg = minimal_pkg ~name ~version in
        Package_name.Map.set acc name pkg)
  in
  let local_packages_for_solver =
    Package_name.Map.values local_packages |> List.map ~f:Local_package.for_solver
  in
  Lock.create_latest_version
    packages
    ~local_packages:local_packages_for_solver
    ~ocaml:None
    ~repos:None
    ~expanded_solver_variable_bindings:Solver_stats.Expanded_variable_bindings.empty
    ~solved_for_platform:None
    ~portable_lock_dir:(Lock.File.is_portable file)
;;

(* Read a lock file in minimal mode - only package names/versions, no opam derivation.
   For directory format, uses full read (already has all info).
   For single-file format, parses and creates minimal Pkg.t without fetching from repos.
   Used by commands like 'outdated' that only need to identify packages. *)
let read_disk_minimal ~(local_packages : Local_package.t Package_name.Map.t) path =
  match Lock.detect_format path with
  | None ->
    User_error.raise
      [ Pp.textf
          "%s is not a valid lock directory or lock file"
          (Path.to_string_maybe_quoted path)
      ]
  | Some Lock.Directory ->
    (* Directory format - use sync reader (has all info already) *)
    Lock.read_disk_exn path
  | Some Lock.File ->
    (* Single-file format - parse and create minimal Lock.t without derivation *)
    let file =
      Io.with_lexbuf_from_file path ~f:(fun lexbuf ->
        Lock.Metadata.parse_contents lexbuf ~f:(fun _lang -> Lock.File.decode))
    in
    file_to_lock_minimal file ~local_packages
;;
