open Import
open Memo.O

let build_dir ctx = Path.Build.relative Dpath.Build.pkgs_dir (Context_name.to_string ctx)

include struct
  open Dune_pkg
  module Pkg = Pkg
  module Dependency = Pkg.Dependency
  module Package_variable = Package_variable
  module Substs = Substs
  module Checksum = Checksum
  module Source = Source
  module Build_command = Pkg.Build_command
  module Display = Dune_engine.Display
  module Pkg_info = Pkg.Info
  module Depexts = Pkg.Depexts
  module Digest_feed = Dune_digest.Feed
  module Dune_dep = Dune_dep
  module Vendor = Vendor
end

module Vendor_stanza = Dune_lang.Vendor_stanza
module Variable = Pkg_opam.Variable

(* Relocatability check - ensures package outputs don't contain absolute paths.
   Packages that embed absolute paths in their outputs are not relocatable and
   cannot be shared across different project locations via the build cache. *)
module Relocatable_check = struct
  (* File extensions that are text files we should check for absolute paths *)
  let text_extensions = [ ".pc"; ".ml"; ".mli"; ".sh"; ".conf"; ".config" ]

  (* Filenames (without extension) that should be checked *)
  let text_filenames = [ "META"; "dune-package" ]

  (* Files that are exempt from relocatability checks.
     These files are known to contain absolute paths by design. *)
  let exempt_files =
    [ "Makefile.config" (* OCaml compiler config - contains installation prefix *) ]
  ;;

  let is_text_file path =
    let basename = Path.basename path in
    (* Skip exempt files *)
    if List.mem exempt_files basename ~equal:String.equal
    then false
    else (
      let has_text_ext =
        List.exists text_extensions ~f:(fun ext -> String.is_suffix basename ~suffix:ext)
      in
      let is_text_name = List.mem text_filenames basename ~equal:String.equal in
      let is_dune_file = String.is_prefix basename ~prefix:"dune-" in
      has_text_ext || is_text_name || is_dune_file)
  ;;

  let check_file ~prefix_path path =
    if is_text_file path && Path.Untracked.exists path
    then (
      let content = Io.read_file path in
      (* Use Re for substring matching *)
      let re = Re.str prefix_path |> Re.compile in
      if Re.execp re content then Some path else None)
    else None
  ;;

  let rec scan_dir ~prefix_path dir =
    match Path.Untracked.readdir_unsorted_with_kinds dir with
    | Error _ -> []
    | Ok entries ->
      List.concat_map entries ~f:(fun (name, kind) ->
        let path = Path.relative dir name in
        match kind with
        | Unix.S_REG -> Option.to_list (check_file ~prefix_path path)
        | Unix.S_DIR -> scan_dir ~prefix_path path
        | _ -> [])
  ;;

  let check ~pkg_name ~prefix ~install_dir =
    let prefix_path = Path.to_absolute_filename prefix in
    let bad_files = scan_dir ~prefix_path install_dir in
    match bad_files with
    | [] -> ()
    | files ->
      User_error.raise
        [ Pp.textf
            "Package %s is not relocatable. The following files contain absolute paths \
             that would prevent sharing via the build cache:"
            (Package.Name.to_string pkg_name)
        ; Pp.enumerate files ~f:(fun p -> Pp.verbatim (Path.to_string p))
        ; Pp.nop
        ; Pp.text
            "Packages should use relative paths or respect BUILD_PATH_PREFIX_MAP for \
             reproducible builds."
        ]
  ;;
end

module Package_universe = struct
  (* A type of group of packages that are co-installed. Multiple different
     versions of a package may be co-installed into the same universe.

     Note that a dev tool universe just contains the package for the dev tool
     itself and not its dependencies, which are installed into the
     [Dependencies _] universe for the default context so they may be
     shared with the project's dependencies. *)
  type t =
    | Dependencies of Context_name.t
    | Dev_tool of Dune_pkg.Dev_tool.t

  let equal a b =
    match a, b with
    | Dependencies a, Dependencies b -> Context_name.equal a b
    | Dev_tool a, Dev_tool b -> Dune_pkg.Dev_tool.equal a b
    | _ -> false
  ;;

  let hash t =
    match t with
    | Dependencies context_name ->
      Tuple.T2.hash Int.hash Context_name.hash (0, context_name)
    | Dev_tool dev_tool -> Tuple.T2.hash Int.hash Dune_pkg.Dev_tool.hash (1, dev_tool)
  ;;

  let context_name = function
    | Dependencies context_name -> context_name
    | Dev_tool dev_tool ->
      (* Each dev tool has its own isolated build context *)
      Dev_tool.context_name dev_tool
  ;;

  let lock_dir_path t =
    match t with
    | Dependencies ctx -> Lock_dir.get_path ctx
    | Dev_tool dev_tool ->
      dev_tool |> Dev_tool.lock_dir |> Path.build |> Option.some |> Memo.return
  ;;
end

(* Package identifier: a (name, version) pair used to uniquely identify packages
   and construct directory paths like "_build/.pkgs/<ctx>/pkg-name.1.0.0/" *)
module Pkg_id = struct
  module T = struct
    type t =
      { name : Package.Name.t
      ; version : Package_version.t
      }

    let compare { name; version } t =
      let open Ordering.O in
      let= () = Package.Name.compare name t.name in
      Package_version.compare version t.version
    ;;

    let to_dyn { name; version } =
      Dyn.record
        [ "name", Package.Name.to_dyn name; "version", Package_version.to_dyn version ]
    ;;
  end

  include T
  include Comparable.Make (T)

  let to_string { name; version } =
    sprintf "%s.%s" (Package.Name.to_string name) (Package_version.to_string version)
  ;;

  let of_string_opt s =
    Vendor_rules.parse_name_version s
    |> Option.map ~f:(fun (name, version) ->
      { name = Package.Name.of_string name; version = Package_version.of_string version })
  ;;

  let of_string s =
    match of_string_opt s with
    | Some t -> t
    | None ->
      { name = Package.Name.of_string s; version = Package_version.of_string "dev" }
  ;;

  let name_of_string s = Package.Name.of_string (Vendor_rules.parse_pkg_name_from_dir s)
  let create ~name ~version = { name; version }
end

module Paths = struct
  include Vendor_rules.Paths

  let extra_source t extra_source = Path.append_local t.extra_sources extra_source

  let extra_source_build t extra_source =
    Path.Build.append_local t.extra_sources extra_source
  ;;

  let make pkg_id universe =
    let root =
      let ctx =
        match (universe : Package_universe.t) with
        | Dependencies ctx -> ctx
        | Dev_tool dev_tool -> Dev_tool.context_name dev_tool
      in
      Path.Build.relative (build_dir ctx) (Pkg_id.to_string pkg_id)
    in
    of_root pkg_id.name ~root
  ;;

  (* Get the root directory (parent of target_dir) *)
  let root_of_target_dir target_dir = Path.Build.parent_exn target_dir
  let root_of_target_dir_path target_dir = Path.parent_exn target_dir

  (* Cookie path inside target_dir - gets cached with the installed files.
     The cookie contains the list of installed files and package variables. *)
  let install_cookie_build target_dir = Path.Build.relative target_dir "cookie"
  let install_cookie target_dir = Path.relative target_dir "cookie"

  (* Installed marker at root level - produced by copy_to_prefix rule.
     This marker indicates that files have been copied to the shared prefix. *)
  let installed_marker_build target_dir =
    Path.Build.relative (root_of_target_dir target_dir) "installed"
  ;;

  let installed_marker target_dir =
    Path.relative (root_of_target_dir_path target_dir) "installed"
  ;;

  let install_file t =
    Path.Build.relative
      t.source_dir
      (sprintf "%s.install" (Package.Name.to_string t.name))
  ;;

  let config_file t =
    Path.Build.relative t.source_dir (sprintf "%s.config" (Package.Name.to_string t.name))
  ;;
end

module Install_cookie = struct
  (* The install cookie represents a serialized representation of all the
     installed artifacts and variables.

     The install cookie of a package is the source of all data we must refer to
     address a package's artifacts.

     It is constructed after we've built and installed the packages. In this
     sense, it is the "installation trace" that we must refer to so that we
     don't have to know anything about the installation procedure.
  *)

  module Gen = struct
    type 'files t =
      { files : 'files
      ; variables : Variable.t list
      }

    let to_dyn f { files; variables } =
      let open Dyn in
      record [ "files", f files; "variables", list Variable.to_dyn variables ]
    ;;
  end

  type t = Path.t list Section.Map.t Gen.t

  module Persistent = Persistent.Make (struct
      type nonrec t = (Section.t * Path.t list) list Gen.t

      let name = "INSTALL-COOKIE"
      let version = 3

      let to_dyn =
        let open Dyn in
        Gen.to_dyn (list (pair Section.to_dyn (list Path.to_dyn)))
      ;;

      let test_example () = { Gen.files = []; variables = [] }
    end)

  let load_exn f =
    match Persistent.load f with
    | Some f -> { f with files = Section.Map.of_list_exn f.files }
    | None -> User_error.raise ~loc:(Loc.in_file f) [ Pp.text "unable to load" ]
  ;;

  let dump path (t : t) =
    Persistent.dump path { t with files = Section.Map.to_list t.files }
  ;;
end

module Value_list_env = struct
  (* A representation of an environment where each variable can hold a
     list of [Value.t]. Each variable will be encoded into a delimited
     string (in the style of the PATH variable). *)
  type t = Value.t list Env.Map.t

  let global : t Lazy.t =
    let parse_strings s = Bin.parse s |> List.map ~f:(fun s -> Value.String s) in
    let of_env env : t = Env.to_map env |> Env.Map.map ~f:parse_strings in
    lazy (of_env (Global.env ()))
  ;;

  (* Concatenate a list of values in the style of lists found in
     environment variables, such as PATH *)
  let string_of_env_values values =
    List.map values ~f:(function
      | Value.String s -> s
      | Dir s | Path s -> Path.to_absolute_filename s)
    |> Bin.encode_strings
  ;;

  let to_env (t : t) = Env.Map.map t ~f:string_of_env_values |> Env.of_map
  let get_path t = Env.Map.find t Env_path.var

  (* [extend_concat_path a b] adds all variables from [b] to [a]
     overwriting any existing values of those variables in [a] except for PATH
     which is set to the concatenation of the PATH variables from [a] and [b]
     with the PATH entries from [b] preceding the PATH entries from
     [a]. If only one of the arguments contains a PATH variable then
     its value will be the value of PATH in the result, however if
     neither argument contains a PATH variable then PATH will be unset
     in the result. *)
  let extend_concat_path a b =
    let extended = Env.Map.superpose b a in
    let concated_path =
      match get_path a, get_path b with
      | None, None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b -> Some (b @ a)
    in
    match concated_path with
    | None -> extended
    | Some concated_path -> Env.Map.set extended Env_path.var concated_path
  ;;

  (* Adds a path to an env where variables are associated with lists
     of paths. The path is prepended to the list associated with the
     given variable and a new binding is added to the env if the
     variable is not yet part of the env. *)
  let add_path (t : t) var path : t =
    Env.Map.update t var ~f:(fun paths ->
      let paths = Option.value paths ~default:[] in
      Some (Value.Dir path :: paths))
  ;;
end

module Env_update = struct
  include Dune_lang.Action.Env_update

  (* Handle the :=, +=, =:, and =+ opam environment update operators.

     The operators with colon character update a variable, adding a
     leading/trailing separator (e.g. the ':' chars in PATH on unix)
     if the variable was initially unset or empty, while the operators
     with a plus character add no leading/trailing separator in such a
     case.

     Updates where the newly added value is the empty string are
     ignored since opam refuses to add empty strings to list
     variables.*)
  let update kind ~new_v ~old_v ~f =
    if new_v = ""
    then old_v
    else (
      match kind with
      | `Colon ->
        let old_v = Option.value ~default:[] old_v in
        Some (f ~old_v ~new_v)
      | `Plus ->
        (match old_v with
         | None | Some [] -> Some [ Value.String new_v ]
         | Some old_v -> Some (f ~old_v ~new_v)))
  ;;

  let append = update ~f:(fun ~old_v ~new_v -> old_v @ [ Value.String new_v ])
  let prepend = update ~f:(fun ~old_v ~new_v -> Value.String new_v :: old_v)

  let set env { op; var = k; value = new_v } =
    Env.Map.update env k ~f:(fun old_v ->
      let append = append ~new_v ~old_v in
      let prepend = prepend ~new_v ~old_v in
      match op with
      | Eq ->
        if new_v = ""
        then if Sys.win32 then None else Some [ String "" ]
        else Some [ Value.String new_v ]
      | PlusEq -> prepend `Plus
      | ColonEq -> prepend `Colon
      | EqPlus -> append `Plus
      | EqColon -> append `Colon
      | EqPlusEq ->
        (* TODO nobody uses this AFAIK *)
        assert false)
  ;;
end

module Resolved_pkg = struct
  (* A resolved package ready for building. This is created from Pkg.t (lock file spec)
     after resolving platform conditionals and computing build paths. *)
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; build_command : Build_command.t option
    ; install_command : Dune_lang.Action.t option
    ; depends : t list
    ; vendored_depends : (Package.Name.t * Path.Source.t) list
      (* Dependencies on vendored dune packages - name and source path *)
    ; depends_on_dune : bool
      (* whether the package declares a dependency on Dune, even if Dune is stripped from [depends] *)
    ; depexts : Depexts.t list
    ; info : Pkg_info.t
    ; paths : Path.t Paths.t
    ; write_paths : Path.Build.t Paths.t
    ; files_dir : Path.Build.t option
    ; pkg_id : Pkg_id.t
    ; mutable exported_env : string Env_update.t list
    ; all_package_versions : Package_version.t Package.Name.Map.t
      (* All packages in the lock, for looking up versions of non-dependencies *)
    ; build_id : Dune_digest.t
      (* Recursive build-id: hash(opam_content, deps' build_ids) for toolchain cache *)
    ; is_cached_toolchain : bool
      (* Whether this toolchain is being populated from the global cache *)
    ; toolchain_cache_dir : Path.t option
      (* Path to the global toolchain cache directory if this is a cached toolchain *)
    ; context : Context_name.t
    ; is_dev_tool : bool
      (* Whether this package is the main dev tool package (installs to default context) *)
    }

  module Top_closure = Top_closure.Make (Id.Set) (Monad.Id)

  let top_closure depends =
    match
      Top_closure.top_closure depends ~key:(fun t -> t.id) ~deps:(fun t -> t.depends)
    with
    | Ok s -> s
    | Error cycle ->
      User_error.raise
        [ Pp.text "the following packages form a cycle:"
        ; Pp.chain cycle ~f:(fun pkg ->
            Pp.verbatim (Package.Name.to_string pkg.info.name))
        ]
  ;;

  let deps_closure t = top_closure t.depends

  let source_files t ~loc =
    let skip_dir = function
      | ".hg" | ".git" | "_darcs" | "_opam" | "_build" | "_esy" -> true
      | _ -> false
    in
    let skip_file = String.is_prefix ~prefix:".#" in
    let rec loop root acc path =
      let full_path = Path.External.append_local root path in
      Fs_memo.dir_contents (External full_path)
      >>= function
      | Error e ->
        User_error.raise
          ~loc
          [ Pp.textf "Unable to read %s" (Path.External.to_string_maybe_quoted full_path)
          ; Unix_error.Detailed.pp e
          ]
      | Ok contents ->
        let files, dirs =
          let contents = Fs_cache.Dir_contents.to_list contents in
          List.rev_filter_partition_map contents ~f:(fun (name, kind) ->
            (* TODO handle links and cycles correctly *)
            match kind with
            | S_DIR -> if skip_dir name then Skip else Right name
            | _ -> if skip_file name then Skip else Left name)
        in
        let acc =
          Path.Local.Set.of_list_map files ~f:(Path.Local.relative path)
          |> Path.Local.Set.union acc
        in
        let+ dirs =
          Memo.parallel_map dirs ~f:(fun dir ->
            let dir = Path.Local.relative path dir in
            loop root Path.Local.Set.empty dir)
        in
        Path.Local.Set.union_all (acc :: dirs)
    in
    (match t.info.source with
     | None -> Memo.return None
     | Some source ->
       Lock_dir.source_kind source
       >>| (function
        | `Local (`File, _) | `Fetch -> None
        | `Local (`Directory, root) -> Some root))
    >>= function
    | None -> Memo.return Path.Local.Set.empty
    | Some root -> loop root Path.Local.Set.empty Path.Local.root
  ;;

  (* Depend on the installed marker, which indicates files are in shared prefix *)
  let dep t = Dep.file (Paths.installed_marker t.paths.target_dir)

  let package_deps t =
    deps_closure t
    |> List.fold_left ~init:Dep.Set.empty ~f:(fun acc t -> dep t |> Dep.Set.add acc)
  ;;

  (* Dependencies on vendored dune packages - depend on their @install alias
     so they get installed to the shared install directory where opam packages
     can find them via OCAMLFIND_DESTDIR *)
  let vendored_deps t =
    let context = t.context in
    List.map t.vendored_depends ~f:(fun (_pkg_name, vendored_path) ->
      (* Depend on @install to install the package to shared install dir *)
      let alias_name = Alias0.install in
      (* Vendored package directory, converted to build path *)
      let dir = Path.Build.append_source (Context_name.build_dir context) vendored_path in
      let alias = Alias.make alias_name ~dir in
      Dep.alias alias)
    |> Dep.Set.of_list
  ;;

  (* All packages install files to the shared _build/install/<ctx>/ directory.
     The per-package target_dir/cookie is used only for dependency tracking. *)
  let install_roots t =
    Pkg_opam.Pkg_install.roots_for_package ~pkg_name:t.info.name ~context:t.context
  ;;

  (* Given a list of packages, construct an env containing variables
     set by each package. Variables containing delimited lists of
     paths (e.g. PATH) which appear in multiple package's envs are
     concatenated in the reverse order of their associated packages in
     the input list. Environment updates via the `exported_env` field
     (equivalent to opam's `setenv` field) are applied for each
     package in the same order as the argument list. *)
  let build_env_of_deps ts =
    List.fold_left ts ~init:Env.Map.empty ~f:(fun env t ->
      let env =
        let roots = install_roots t in
        let init = Value_list_env.add_path env Env_path.var roots.bin in
        let vars = Install.Roots.to_env_without_path roots ~relative:Path.relative in
        List.fold_left vars ~init ~f:(fun acc (var, path) ->
          Value_list_env.add_path acc var path)
      in
      List.fold_left t.exported_env ~init:env ~f:Env_update.set)
  ;;

  (* [build_env t] returns an env containing paths containing all the
     tools and libraries required to build the package [t] inside the
     faux opam directory contained in the _build dir. *)
  let build_env t = build_env_of_deps @@ deps_closure t

  let base_env t =
    Env.Map.of_list_exn
      [ Opam_switch.opam_switch_prefix_var_name, [ Value.Path t.paths.target_dir ]
      ; "CDPATH", [ Value.String "" ]
      ; "MAKELEVEL", [ Value.String "" ]
      ; "OPAM_PACKAGE_NAME", [ Value.String (Package.Name.to_string t.info.name) ]
      ; ( "OPAM_PACKAGE_VERSION"
        , [ Value.String (Package_version.to_string t.info.version) ] )
      ; "OPAMCLI", [ Value.String "2.0" ]
      ]
  ;;

  (* [exported_value_env t] returns the complete env that will be used
     to build the package [t] *)
  let exported_value_env t =
    let package_env = build_env t |> Env.Map.superpose (base_env t) in
    (* TODO: Run actions in a constrained environment. [Global.env ()] is the
       environment from which dune was executed, and some of the environment
       variables may affect builds in unintended ways and make builds less
       reproducible. However other environment variables must be set in order
       for build actions to run successfully, such as $PATH on systems where the
       shell's default $PATH variable doesn't include the location of standard
       programs or build tools (e.g. NixOS). *)
    Value_list_env.extend_concat_path (Lazy.force Value_list_env.global) package_env
  ;;

  let exported_env t = Value_list_env.to_env @@ exported_value_env t
end

module Pkg_installed = struct
  type t = { cookie : Install_cookie.t Action_builder.t }

  let of_paths (paths : Path.t Paths.t) =
    let cookie =
      let open Action_builder.O in
      (* Cookie is inside target_dir, gets cached with installed files *)
      let path = Paths.install_cookie paths.target_dir in
      let+ () = path |> Dep.file |> Action_builder.dep in
      Install_cookie.load_exn path
    in
    { cookie }
  ;;
end

module Expander0 = struct
  include Expander0

  type t =
    { name : Dune_pkg.Package_name.t
    ; paths : Path.t Paths.t
    ; artifacts : Path.t Filename.Map.t Memo.t
    ; depends :
        (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
          Memo.t
    ; depexts : Depexts.t list
    ; context : Context_name.t
    ; version : Package_version.t
    ; env : Value.t list Env.Map.t
    ; all_package_versions : Package_version.t Package.Name.Map.t Memo.t
      (* All packages in the lock, for looking up versions of non-dependencies *)
    ; self_build_id : Dune_digest.t (* Build ID of this package *)
    ; build_ids : Dune_digest.t Package.Name.Map.t Memo.t
      (* Build IDs of all dependencies for resolving %{pkg:build-id} *)
    }

  let expand_pform_fdecl
    : (t
       -> source:Dune_sexp.Template.Pform.t
       -> Pform.t
       -> (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t)
        Fdecl.t
    =
    Fdecl.create Dyn.opaque
  ;;
end

module Substitute = struct
  include Substs.Make (Memo)
  module Expander = Expander0

  module Spec = struct
    type ('src, 'dst) t =
      { (* XXX it's not good to serialize the substitution map like this. We're
           essentially implementing the same substitution procedure but in two
           different places: action geeneration, and action execution.

           The two implementations are bound to drift. Better would be to
           reconstruct everything that is needed to call our one and only
           substitution function. *)
        expander : Expander.t
      ; depends :
          (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
      ; artifacts : Path.t Filename.Map.t
      ; src : 'src
      ; dst : 'dst
      }

    let name = "substitute"
    let version = 3
    let bimap t f g = { t with src = f t.src; dst = g t.dst }
    let is_useful_to ~memoize = memoize

    let encode { expander; depends; artifacts; src; dst } input output : Sexp.t =
      let e =
        let paths (p : Path.t Paths.t) = p.source_dir, p.target_dir, p.name in
        ( paths expander.paths
        , String.Map.to_list artifacts
        , Package.Name.Map.to_list_map depends ~f:(fun _ (m, p) -> m, paths p)
        , expander.version )
        |> Digest.generic
        |> Digest.to_string_raw
      in
      List [ Atom e; input src; output dst ]
    ;;

    let action { expander; depends = _; artifacts = _; src; dst } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      let env (var : Substs.Variable.t) =
        let open Memo.O in
        ((* TODO loc *)
         let loc = Loc.none in
         let source =
           (* TODO it's rather ugly that we're going through the pform machinery
              to do this *)
           { Dune_sexp.Template.Pform.loc; name = ""; payload = None }
         in
         match
           match var with
           | Package var -> Some (Package_variable.to_pform var)
           | Global n ->
             Package_variable_name.to_string n
             |> Pform.Var.of_opam_global_variable_name
             |> Option.map ~f:(fun v -> Pform.Var v)
         with
         | None -> Memo.return @@ Variable.S ""
         | Some pform ->
           (Fdecl.get Expander.expand_pform_fdecl) expander ~source pform
           >>| (function
            | Error (`Undefined_pkg_var _) ->
              (* these are opam's semantics as far as I understand. *)
              Variable.S ""
            | Ok v ->
              let dir = Path.parent_exn src |> Path.drop_optional_sandbox_root in
              Variable.of_values v ~dir))
        >>| Option.some
      in
      subst env expander.paths.name ~src ~dst |> Memo.run
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action (expander : Expander.t) ~src ~dst =
    let+ depends = expander.depends
    and+ artifacts = expander.artifacts in
    A.action { Spec.expander; depends; artifacts; src; dst }
  ;;
end

module Action_expander = struct
  module Expander = struct
    include Expander0

    let map_exe _ x =
      (* TODO *)
      x
    ;;

    let expand_pkg_macro
          ~context
          ~loc
          ~all_package_versions
          ~self_build_id
          ~build_ids
          (self_paths : _ Paths.t)
          deps
          macro_invocation
      =
      let* deps = deps
      and* all_versions = all_package_versions
      and* build_ids = build_ids in
      let { Package_variable.name = variable_name; scope; default_if_true } =
        match Package_variable.of_macro_invocation ~loc macro_invocation with
        | Ok package_variable -> package_variable
        | Error `Unexpected_macro ->
          Code_error.raise
            "Attempted to treat an unexpected macro invocation as a package variable \
             encoding"
            []
      in
      let package_name =
        match scope with
        | Self -> self_paths.name
        | Package package_name -> package_name
      in
      let variables, dep_paths =
        match Package.Name.Map.find deps package_name with
        | None -> Package_variable_name.Map.empty, None
        | Some (var, paths) -> var, Some paths
      in
      let dep_install_paths =
        Option.map dep_paths ~f:(fun paths -> Lazy.force paths.Paths.install_paths)
      in
      let+ result =
        match Package_variable_name.Map.find variables variable_name with
        | Some v -> Memo.return @@ Ok (Variable.dune_value v)
        | None ->
          let present = Option.is_some dep_paths in
          (match
             Pkg_opam.resolve_builtin_var
               ~context
               ~package_name
               ~all_versions
               ~present
               ~scope
               ~self_build_id
               ~build_ids
               ~dep_install_paths
               variable_name
           with
           | Some result -> result
           | None -> Memo.return (Error (`Undefined_pkg_var variable_name)))
      in
      Pkg_opam.apply_default_if_true default_if_true result
    ;;

    let expand_pform
          { name
          ; env = _
          ; paths
          ; artifacts = _
          ; context
          ; depends
          ; version
          ; depexts = _
          ; all_package_versions
          ; self_build_id
          ; build_ids
          }
          ~source
          (pform : Pform.t)
      : (Value.t list, [ `Undefined_pkg_var of Package_variable_name.t ]) result Memo.t
      =
      let loc = Dune_sexp.Template.Pform.loc source in
      match pform with
      | Var (Pkg Name) ->
        Memo.return (Ok [ Value.String (Dune_pkg.Package_name.to_string name) ])
      | Var (Pkg Version) ->
        Memo.return (Ok [ Value.String (Package_version.to_string version) ])
      | Var (Pkg var) ->
        Pkg_opam.expand_pkg ~context ~source_dir:paths.source_dir ~prefix:paths.prefix var
        >>| Result.ok
      | Var Context_name ->
        Memo.return (Ok [ Value.String (Context_name.to_string context) ])
      | Var Make ->
        let+ make =
          let path = Env_path.path (Global.env ()) in
          Make_prog.which loc context ~path
        in
        Ok [ Value.Path make ]
      | Macro ({ macro = Pkg | Pkg_self; _ } as macro_invocation) ->
        expand_pkg_macro
          ~context
          ~loc
          ~all_package_versions
          ~self_build_id
          ~build_ids
          paths
          depends
          macro_invocation
      | _ -> Expander0.isn't_allowed_in_this_position ~source
    ;;

    let () = Fdecl.set expand_pform_fdecl expand_pform

    let expand_pform_gen t =
      String_expander.Memo.expand ~dir:t.paths.source_dir ~f:(fun ~source pform ->
        expand_pform t ~source pform
        >>| function
        | Ok x -> x
        | Error (`Undefined_pkg_var variable_name) ->
          User_error.raise
            ~loc:(Dune_sexp.Template.Pform.loc source)
            [ Pp.textf
                "Undefined package variable: %s"
                (Package_variable_name.to_string variable_name)
            ])
    ;;

    let slang_expander t sw =
      String_expander.Memo.expand_result_deferred_concat sw ~mode:Many ~f:(expand_pform t)
    ;;

    let eval_blang t blang =
      Slang_expand.eval_blang blang ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let eval_slangs_located t slangs =
      let slangs =
        List.map slangs ~f:(fun slang ->
          Slang.map_loc slang ~f:Dune_pkg.Lock.loc_in_source_tree)
      in
      Slang_expand.eval_multi_located slangs ~dir:t.paths.source_dir ~f:(slang_expander t)
    ;;

    let filtered_depexts t =
      Memo.List.filter_map t.depexts ~f:(fun (depexts : Depexts.t) ->
        let+ enabled =
          match depexts.enabled_if with
          | `Always -> Memo.return true
          | `Conditional condition -> eval_blang t condition
        in
        if enabled then Some depexts.external_package_names else None)
      >>| List.concat
      >>| List.sort_uniq ~compare:String.compare
    ;;

    let expand_exe_value t value ~loc =
      let+ prog =
        match value with
        | Value.Dir p ->
          User_error.raise
            ~loc
            [ Pp.textf
                "%s is a directory and cannot be used as an executable"
                (Path.to_string_maybe_quoted p)
            ]
        | Path p -> Memo.return @@ Ok p
        | String program ->
          (match Filename.analyze_program_name program with
           | Relative_to_current_dir | Absolute ->
             let dir = t.paths.source_dir in
             Memo.return @@ Ok (Path.relative dir program)
           | In_path ->
             (match program with
              | "dune" ->
                let dune = Path.of_string Sys.executable_name in
                Memo.return @@ Ok dune
              | program ->
                let* artifacts = t.artifacts in
                (match Filename.Map.find artifacts program with
                 | Some s -> Memo.return @@ Ok s
                 | None ->
                   (* Use the package's build environment PATH (which includes
                      _build/install/<ctx>/bin/), then fall back to system PATH *)
                   let pkg_path =
                     Value_list_env.get_path t.env
                     |> Option.value ~default:[]
                     |> List.filter_map ~f:(function
                       | Value.Path p | Value.Dir p -> Some p
                       | Value.String s -> Some (Path.of_string s))
                   in
                   let system_path = Global.env () |> Env_path.path in
                   let path = pkg_path @ system_path in
                   Which.which ~path program
                   >>= (function
                    | Some p -> Memo.return (Ok p)
                    | None ->
                      let+ depexts = filtered_depexts t in
                      let hint =
                        Run_with_path.depexts_hint depexts
                        |> Option.map ~f:(fun pp -> Format.asprintf "%a" Pp.to_fmt pp)
                      in
                      Error
                        (Action.Prog.Not_found.create
                           ?hint
                           ~program
                           ~context:t.context
                           ~loc:(Some loc)
                           ())))))
      in
      Result.map prog ~f:(map_exe t)
    ;;
  end

  (* [install_prefix] is where install commands write to. If None, uses the
     shared install directory. For caching, this should be set to target_dir
     so installed files can be cached independently of the shared prefix. *)
  let rec expand ?install_prefix (action : Dune_lang.Action.t) ~(expander : Expander.t) =
    let dir = expander.paths.source_dir in
    (* Determine the actual install prefix and roots to use *)
    let get_install_prefix_and_roots () =
      match install_prefix with
      | Some prefix ->
        let roots = Install.Roots.opam_from_prefix ~relative:Path.Build.relative prefix in
        Path.build prefix, roots
      | None ->
        let shared_roots = Pkg_opam.Pkg_install.roots_build ~context:expander.context in
        let prefix = Pkg_opam.Pkg_install.dir ~context:expander.context |> Path.build in
        prefix, shared_roots
    in
    match action with
    | Run args ->
      Expander.eval_slangs_located expander args
      >>= (function
       | [] ->
         let loc =
           let loc = function
             | Slang.Nil -> None
             | Literal sw -> Some (String_with_vars.loc sw)
             | Form (loc, _) -> Some loc
           in
           let start = List.find_map args ~f:loc in
           let stop =
             List.fold_left args ~init:None ~f:(fun last a ->
               match loc a with
               | None -> last
               | Some _ as s -> s)
           in
           Option.both start stop
           |> Option.map ~f:(fun (start, stop) -> Loc.span start stop)
         in
         User_error.raise
           ?loc
           [ Pp.text "\"run\" action must have at least one argument" ]
       | (prog_loc, prog) :: args ->
         let+ exe =
           let prog = Value.Deferred_concat.force prog ~dir in
           Expander.expand_exe_value expander prog ~loc:prog_loc
         and+ depexts = Expander.filtered_depexts expander in
         let args =
           Array.Immutable.of_list_map args ~f:(fun (_loc, arg) ->
             Value.Deferred_concat.parts arg
             |> Array.Immutable.of_list_map ~f:(fun (arg : Value.t) ->
               match arg with
               | String s -> Run_with_path.Spec.String s
               | Path p | Dir p -> Path p))
         in
         let prefix, roots = get_install_prefix_and_roots () in
         let ocamlfind_destdir = Path.build roots.lib_root in
         Run_with_path.action
           ~depexts
           ~pkg:(expander.name, prog_loc)
           exe
           args
           ~prefix
           ~ocamlfind_destdir)
    | Progn t ->
      let+ args = Memo.parallel_map t ~f:(expand ?install_prefix ~expander) in
      Action.Progn args
    | System arg ->
      let+ cmd =
        Expander.expand_pform_gen ~mode:Single expander arg >>| Value.to_string ~dir
      in
      (* Use Run_with_path.system_action for proper cache key handling.
         BUILD_PATH_PREFIX_MAP is set at execution time, while cache key uses
         canonical placeholders for cross-project cache sharing. *)
      let prefix, roots = get_install_prefix_and_roots () in
      let ocamlfind_destdir = Path.build roots.lib_root in
      Run_with_path.system_action ~cmd ~prefix ~ocamlfind_destdir
    | Patch p ->
      let+ patch =
        Expander.expand_pform_gen ~mode:Single expander p >>| Value.to_path ~dir
      in
      Dune_patch.action ~patch
    | Substitute (src, dst) ->
      let* src =
        Expander.expand_pform_gen ~mode:Single expander src >>| Value.to_path ~dir
      and* dst =
        Expander.expand_pform_gen ~mode:Single expander dst
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir ~what:"substitute" ~loc:(String_with_vars.loc dst)
      in
      Substitute.action expander ~src ~dst
    | Withenv (updates, action) -> expand_withenv ?install_prefix expander updates action
    | When (condition, action) ->
      Expander.eval_blang expander condition
      >>= (function
       | true -> expand ?install_prefix action ~expander
       | false -> Memo.return (Action.progn []))
    | Write_file (path_sw, perm, contents_sw) ->
      let+ path =
        Expander.expand_pform_gen ~mode:Single expander path_sw
        >>| Value.to_path ~dir
        >>| Expander0.as_in_build_dir
              ~what:"write-file"
              ~loc:(String_with_vars.loc path_sw)
      and+ contents =
        Expander.expand_pform_gen ~mode:Single expander contents_sw
        >>| Value.to_string ~dir
      in
      Action.Write_file (path, perm, contents)
    | _ ->
      Code_error.raise
        "Pkg_rules.action_expander.expand: unsupported action"
        [ "action", Dune_lang.Action.to_dyn action ]

  and expand_withenv ?install_prefix (expander : Expander.t) updates action =
    let* env, updates =
      let dir = expander.paths.source_dir in
      Memo.List.fold_left
        ~init:(expander.env, [])
        updates
        ~f:(fun (env, updates) ({ Env_update.op = _; var; value } as update) ->
          let+ value =
            let+ value =
              let expander = { expander with env } in
              Expander.expand_pform_gen expander value ~mode:Single
            in
            Value.to_string ~dir value
          in
          let env = Env_update.set env { update with value } in
          let update =
            let value =
              match Env.Map.find env var with
              | Some v -> Value_list_env.string_of_env_values v
              | None ->
                (* TODO *)
                ""
            in
            var, value
          in
          env, update :: updates)
    in
    let+ action =
      let expander = { expander with env } in
      expand ?install_prefix action ~expander
    in
    List.fold_left updates ~init:action ~f:(fun action (k, v) ->
      Action.Setenv (k, v, action))
  ;;

  module Artifacts_and_deps = struct
    type artifacts_and_deps =
      { binaries : Path.t Filename.Map.t
      ; dep_info :
          (OpamVariable.variable_contents Package_variable_name.Map.t * Path.t Paths.t)
            Package.Name.Map.t
      }

    let empty = { binaries = Filename.Map.empty; dep_info = Package.Name.Map.empty }

    let of_closure closure =
      Memo.parallel_map closure ~f:(fun (pkg : Resolved_pkg.t) ->
        let cookie = (Pkg_installed.of_paths pkg.paths).cookie in
        Action_builder.evaluate_and_collect_facts cookie
        |> Memo.map ~f:(fun ((cookie : Install_cookie.t), _) -> pkg, cookie))
      |> Memo.map ~f:(fun (cookies : (Resolved_pkg.t * Install_cookie.t) list) ->
        List.fold_left
          cookies
          ~init:empty
          ~f:
            (fun
              { binaries; dep_info }
              ((pkg : Resolved_pkg.t), (cookie : Install_cookie.t))
            ->
            let binaries =
              Section.Map.Multi.find cookie.files Bin
              |> List.fold_left ~init:binaries ~f:(fun acc bin ->
                Filename.Map.set acc (Path.basename bin) bin)
            in
            let dep_info =
              let variables =
                Package_variable_name.Map.superpose
                  (Package_variable_name.Map.of_list_exn cookie.variables)
                  (Pkg_info.variables pkg.info)
              in
              Package.Name.Map.add_exn dep_info pkg.info.name (variables, pkg.paths)
            in
            { binaries; dep_info }))
    ;;
  end

  let expander context (pkg : Resolved_pkg.t) =
    let closure =
      Memo.lazy_
        ~human_readable_description:(fun () ->
          Pp.textf
            "Computing closure for package %S"
            (Package.Name.to_string pkg.info.name))
        (fun () -> Resolved_pkg.deps_closure pkg |> Artifacts_and_deps.of_closure)
    in
    let env = Resolved_pkg.exported_value_env pkg in
    let depends =
      Memo.Lazy.map closure ~f:(fun { Artifacts_and_deps.dep_info; _ } ->
        Package.Name.Map.add_exn
          dep_info
          pkg.info.name
          (Pkg_info.variables pkg.info, pkg.paths))
      |> Memo.Lazy.force
    in
    let artifacts =
      let+ { Artifacts_and_deps.binaries; _ } = Memo.Lazy.force closure in
      binaries
    in
    (* Compute build_ids map for all dependencies *)
    let build_ids =
      let deps_closure = Resolved_pkg.deps_closure pkg in
      let dep_build_ids =
        List.map deps_closure ~f:(fun (dep : Resolved_pkg.t) ->
          dep.info.name, dep.build_id)
      in
      (* Include self in the map *)
      Memo.return
        (Package.Name.Map.of_list_exn ((pkg.info.name, pkg.build_id) :: dep_build_ids))
    in
    { Expander.paths = pkg.paths
    ; name = pkg.info.name
    ; artifacts
    ; context
    ; depends
    ; depexts = pkg.depexts
    ; version = pkg.info.version
    ; env
    ; all_package_versions = Memo.return pkg.all_package_versions
    ; self_build_id = pkg.build_id
    ; build_ids
    }
  ;;

  (* Build commands require Copy sandbox mode to get a writable copy of the
     source directory. Symlink/Hardlink modes won't work because source files
     from tarballs are often read-only, and symlinks/hardlinks preserve that.
     Copy mode explicitly adds write permissions (see sandbox.ml chmod_file). *)
  let build_sandbox =
    Sandbox_mode.Set.of_func (function
      | Some Sandbox_mode.Copy -> true
      | Some Symlink | Some Hardlink | None | Some Patch_back_source_tree -> false)
  ;;

  let expand_action
        ?(can_go_in_shared_cache = true)
        ?install_prefix
        ~sandbox
        ?(chdir = true)
        context
        (pkg : Resolved_pkg.t)
        action
    =
    let+ action =
      let expander = expander context pkg in
      (* When install_prefix is provided, update the expander's paths so that
         %{prefix}%, %{lib}%, etc. expand to the install prefix location.
         This ensures install commands write to target_dir for caching. *)
      let expander =
        match install_prefix with
        | None -> expander
        | Some target_dir ->
          let prefix = Path.build target_dir in
          let install_roots =
            Install.Roots.opam_from_prefix ~relative:Path.relative prefix
          in
          let install_paths =
            Install.Paths.make
              ~relative:Path.relative
              ~package:pkg.info.name
              ~roots:install_roots
          in
          { expander with
            paths =
              { expander.paths with
                prefix
              ; install_roots = lazy install_roots
              ; install_paths = lazy install_paths
              }
          }
      in
      let+ action = expand ?install_prefix action ~expander in
      if chdir then Action.chdir pkg.paths.source_dir action else action
    in
    Action.Full.make ~sandbox ~can_go_in_shared_cache action
    |> Action_builder.return
    |> Action_builder.with_no_targets
  ;;

  let dune_exe context =
    Which.which ~path:(Env_path.path Env.initial) "dune"
    >>| function
    | Some s -> Ok s
    | None -> Error (Action.Prog.Not_found.create ~loc:None ~context ~program:"dune" ())
  ;;

  let build_command context (pkg : Resolved_pkg.t) =
    (* Build commands run sandboxed for isolation.
       - WITH install action: PREFIX=target_dir (consistent with install, cacheable)
       - WITHOUT install action: PREFIX=shared (to find dependencies) *)
    let install_prefix =
      match pkg.install_command with
      | Some _ -> Some pkg.write_paths.target_dir
      | None -> None
    in
    Option.map pkg.build_command ~f:(function
      | Action action ->
        expand_action ?install_prefix ~sandbox:build_sandbox context pkg action
      | Dune ->
        (* CR-someday rgrinberg: respect [dune subst] settings. *)
        Command.run_dyn_prog
          (Action_builder.of_memo (dune_exe context))
          ~dir:pkg.paths.source_dir
          [ A "build"; A "-p"; A (Package.Name.to_string pkg.info.name) ]
        |> Memo.return)
  ;;

  (* Install command that writes to target_dir for caching.
     The target_dir becomes the cacheable artifact - installed files are later
     copied to the shared prefix by a separate non-cached rule. *)
  let install_command_to_target_dir context (pkg : Resolved_pkg.t) =
    Option.map pkg.install_command ~f:(fun action ->
      (* Install to target_dir instead of shared prefix for caching *)
      expand_action
        ~can_go_in_shared_cache:true
        ~install_prefix:pkg.write_paths.target_dir
        ~sandbox:build_sandbox
        context
        pkg
        action)
  ;;

  let exported_env (expander : Expander.t) (env : _ Env_update.t) =
    let+ value =
      let+ value = Expander.expand_pform_gen expander env.value ~mode:Single in
      value |> Value.to_string ~dir:expander.paths.source_dir
    in
    { env with value }
  ;;
end

(* System-provided packages that don't need to be built *)
let default_system_provided = Package.Name.Set.singleton Dune_pkg.Dune_dep.name

(** Status of a package with respect to vendoring. *)
module Vendor_status = struct
  type t =
    | Not_vendored
    | Dune_native (* Built as vendored code in main dune context *)
    | Opam_sandboxed (* Built in opam sandbox with local source *)
end

module rec Resolve : sig
  (** Resolve a package from the unified registry. Works for both lock file
      packages and vendor packages. *)
  val resolve_entry
    :  Package_registry.t
    -> Package_registry.entry
    -> package_universe:Package_universe.t
    -> Resolved_pkg.t Memo.t
end = struct
  open Resolve

  let relocate action =
    let string_with_vars = String_with_vars.map_loc ~f:Dune_pkg.Lock.loc_in_source_tree in
    let slang = Slang.map_loc ~f:Dune_pkg.Lock.loc_in_source_tree in
    let blang = Slang.Blang.map_loc ~f:Dune_pkg.Lock.loc_in_source_tree in
    Dune_lang.Action.map action ~string_with_vars ~slang ~blang
  ;;

  let relocate_build b =
    match (b : Build_command.t) with
    | Dune -> Build_command.Dune
    | Action a -> Build_command.Action (relocate a)
  ;;

  (* Resolve files_dir, handling both versioned and unversioned paths.
     Handles the case where lockdirs may or may not be portable. *)
  let resolve_files_dir package_universe info =
    let* lock_dir =
      Package_universe.lock_dir_path package_universe >>| Option.value_exn
    in
    let+ files_dir =
      (* TODO(steve): simplify this once portable lockdirs become the default.
         This logic currently handles both portable lockdirs (version number
         in files dir name) and non-portable lockdirs (no version number). *)
      let path_with_version =
        Pkg.source_files_dir info.Pkg_info.name (Some info.version) ~lock_dir
      in
      let* path_with_version_exists =
        Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir path_with_version)
      in
      match path_with_version_exists with
      | true ->
        Memo.return @@ Some (Pkg.files_dir info.name (Some info.version) ~lock_dir)
      | false ->
        let path_without_version = Pkg.source_files_dir info.name None ~lock_dir in
        let+ path_without_version_exists =
          Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir path_without_version)
        in
        (match path_without_version_exists with
         | true -> Some (Pkg.files_dir info.name None ~lock_dir)
         | false -> None)
    in
    files_dir
    |> Option.map ~f:(fun (p : Path.t) ->
      match p with
      | External e ->
        let source_path = Dune_pkg.Pkg_workspace.dev_tool_path_to_source_dir e in
        (match Path.Source.explode source_path with
         (* Dev tool lock files: _build/tools-{name}/.lock/{files_dir} *)
         | [ "_build"; ctx_name; ".lock"; files_dir ]
           when String.is_prefix ctx_name ~prefix:Dev_tool.context_name_prefix ->
           Path.Build.L.relative Path.Build.root [ ctx_name; ".lock"; files_dir ]
         | components ->
           Code_error.raise
             "Package files directory is external source directory, this is unsupported"
             [ "external", Path.External.to_dyn e
             ; "source", Path.Source.to_dyn source_path
             ; "components", Dyn.(list string) components
             ])
      | In_source_tree s ->
        Code_error.raise "Unexpected files_dir path" [ "dir", Path.Source.to_dyn s ]
      | In_build_dir b -> b)
  ;;

  (* Apply toolchain caching: for cached toolchains, replace build/install
     commands with populate-from-cache action. *)
  let apply_toolchain_caching
        ~info
        ~pkg
        ~build_id
        ~build_command
        ~install_command
        ~write_paths
        ~package_universe
    =
    let is_toolchain =
      Pkg_toolchain.is_compiler_and_toolchains_enabled info.Pkg_info.name
    in
    let is_cached = is_toolchain && Pkg_toolchain.is_installed pkg ~build_id in
    let toolchain_cache_dir =
      if is_cached then Some (Pkg_toolchain.cache_dir pkg ~build_id) else None
    in
    let build_command, install_command =
      if is_cached
      then (
        let cache_dir = Option.value_exn toolchain_cache_dir in
        let context = Package_universe.context_name package_universe in
        let install_dir = Install.Context.dir ~context in
        Log.info
          "Toolchain cache hit"
          [ "name", Dyn.string (Package.Name.to_string info.name)
          ; "version", Dyn.string (Package_version.to_string info.version)
          ; "cache_dir", Dyn.string (Path.to_string cache_dir)
          ; "build_id", Dyn.string (Dune_digest.to_string build_id)
          ; "local_target", Dyn.string (Path.Build.to_string write_paths.Paths.target_dir)
          ; "install_dir", Dyn.string (Path.Build.to_string install_dir)
          ];
        let populate_action =
          Pkg_toolchain.populate_from_cache_action
            pkg
            ~build_id
            ~install_dir
            ~target_dir:write_paths.target_dir
        in
        Some (Build_command.Action populate_action), None)
      else if is_toolchain
      then (
        Log.info
          "Toolchain cache miss"
          [ "name", Dyn.string (Package.Name.to_string info.name)
          ; "version", Dyn.string (Package_version.to_string info.version)
          ; "build_id", Dyn.string (Dune_digest.to_string build_id)
          ];
        build_command, install_command)
      else build_command, install_command
    in
    build_command, install_command, is_cached, toolchain_cache_dir
  ;;

  (* Input for resolve_entry memoization *)
  module Entry_input = struct
    type t =
      { entry : Package_registry.entry
      ; universe : Package_universe.t
      }

    let equal { entry; universe } t =
      Package.Name.equal entry.name t.entry.name
      && Package_version.equal entry.version t.entry.version
      && Package_universe.equal universe t.universe
    ;;

    let hash { entry; universe } =
      Tuple.T3.hash
        Package.Name.hash
        Package_version.hash
        Package_universe.hash
        (entry.name, entry.version, universe)
    ;;

    let to_dyn = Dyn.opaque
  end

  (* Load opam file from a vendor source directory *)
  let load_vendor_opam_file ~source_dir ~pkg_name =
    match Vendor_rules.find_opam_file ~pkg_name ~pkg_dir:source_dir with
    | None ->
      User_error.raise
        [ Pp.textf
            "No opam file found for vendored package %s in %s"
            pkg_name
            (Path.Source.to_string source_dir)
        ]
    | Some opam_path ->
      let contents = Io.read_file ~binary:true (Path.source opam_path) in
      (match OpamFile.OPAM.read_from_string contents with
       | exception exn ->
         User_error.raise
           [ Pp.textf
               "Failed to parse opam file for vendored package %s: %s"
               pkg_name
               (Printexc.to_string exn)
           ]
       | opam -> opam)
  ;;

  (* Resolve dependencies from registry entries *)
  let resolve_entry_deps registry (pkg : Pkg.t) =
    let* platform = Lock_dir.Sys_vars.solver_env in
    let deps =
      Pkg.Conditional_choice.choose_for_platform pkg.depends ~platform
      |> Option.value ~default:[]
    in
    let system_provided = default_system_provided in
    let has_dune_dep, dep_entries =
      List.fold_right
        deps
        ~init:(false, [])
        ~f:(fun { Dependency.name; loc = _ } (has_dune_dep, acc) ->
          if Dune_lang.Package_name.equal name Dune_pkg.Dune_dep.name
          then true, acc
          else if Package.Name.Set.mem system_provided name
          then has_dune_dep, acc
          else (
            match Package_registry.find registry name with
            | None ->
              (* Dependency not in registry - skip (might be optional) *)
              has_dune_dep, acc
            | Some dep_entry -> has_dune_dep, dep_entry :: acc))
    in
    Memo.return (has_dune_dep, dep_entries)
  ;;

  let resolve_entry_impl registry { Entry_input.entry; universe = package_universe } =
    let { Package_registry.name; version; source } = entry in
    let pkg_name = Package.Name.to_string name in
    let pkg_id = Pkg_id.create ~name ~version in
    (* Get the Pkg.t from the source *)
    let* pkg_result =
      match source with
      | Package_registry.Source.From_lock { pkg } -> Memo.return (Ok pkg)
      | Package_registry.Source.From_vendor { source_dir; stanza = _ } ->
        let opam = load_vendor_opam_file ~source_dir ~pkg_name in
        (* Set source to point to the vendor directory so source_rules can copy it *)
        let abs_path =
          Path.source source_dir |> Path.to_absolute_filename |> Path.External.of_string
        in
        let vendor_source = Source.external_copy (Loc.none, abs_path) in
        Dune_pkg.Pkg.of_opam_file ~name ~version ~source:vendor_source ~opam ()
        |> Memo.return
    in
    match pkg_result with
    | Error msg -> User_error.raise (User_message.pp msg |> List.singleton)
    | Ok pkg ->
      let info = pkg.Pkg.info in
      (* Vendor status is determined by the entry source:
         - From_vendor with Dune_native build_method should not reach here
         - From_vendor with Opam_sandboxed or From_lock are both valid *)
      let* platform = Lock_dir.Sys_vars.solver_env in
      let choose_for_current_platform field =
        Pkg.Conditional_choice.choose_for_platform field ~platform
      in
      (* Resolve dependencies *)
      let* has_dune_dep, dep_entries = resolve_entry_deps registry pkg in
      (* Recursively resolve dep entries *)
      let* all_depends =
        Memo.parallel_map dep_entries ~f:(fun dep_entry ->
          let vendor_path = Vendor.package_dir dep_entry.name dep_entry.version in
          let* is_vendored =
            Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir vendor_path)
          in
          match is_vendored with
          | true ->
            (* Check if it's dune-native vendored *)
            (match dep_entry.source with
             | Package_registry.Source.From_vendor { stanza; _ } ->
               (match stanza.build_method with
                | Some Vendor_stanza.Build_method.Dune_native | None ->
                  Memo.return (Either.Left (dep_entry.name, vendor_path))
                | Some Opam_sandboxed ->
                  let+ resolved = resolve_entry registry dep_entry ~package_universe in
                  Either.Right resolved)
             | From_lock _ ->
               let+ resolved = resolve_entry registry dep_entry ~package_universe in
               Either.Right resolved)
          | false ->
            let+ resolved = resolve_entry registry dep_entry ~package_universe in
            Either.Right resolved)
      and+ files_dir = resolve_files_dir package_universe info in
      let vendored_depends, depends = List.partition_map all_depends ~f:Fun.id in
      (* Prepare paths and commands *)
      let id = Resolved_pkg.Id.gen () in
      let write_paths =
        Paths.make pkg_id package_universe ~relative:Path.Build.relative
      in
      let install_command = choose_for_current_platform pkg.install_command in
      let install_command = Option.map install_command ~f:relocate in
      let build_command = choose_for_current_platform pkg.build_command in
      let build_command = Option.map build_command ~f:relocate_build in
      (* Get build_id from lock file if available, otherwise compute it.
         The build_id is a hash(opam_content, platform, deps' build_ids).
         This creates a Merkle tree where any change in the dep graph propagates up. *)
      let build_id =
        match pkg.build_id with
        | Some build_id ->
          (* Use pre-computed build_id from lock file *)
          build_id
        | None ->
          (* Compute build_id for backward compatibility with older lock files *)
          let opam_content_hash =
            Dune_digest.Feed.compute_digest Pkg.digest_feed (Pkg.remove_locs pkg)
            |> Dune_digest.to_string
          in
          let platform_hash = Dune_digest.generic platform |> Dune_digest.to_string in
          let deps_hashes =
            List.map depends ~f:(fun (dep : Resolved_pkg.t) ->
              Dune_digest.to_string dep.build_id)
            |> List.sort ~compare:String.compare
          in
          Dune_digest.generic (opam_content_hash :: platform_hash :: deps_hashes)
      in
      (* Apply toolchain caching if applicable *)
      let build_command, install_command, is_cached, toolchain_cache_dir =
        apply_toolchain_caching
          ~info
          ~pkg
          ~build_id
          ~build_command
          ~install_command
          ~write_paths
          ~package_universe
      in
      (* Build all_package_versions from registry *)
      let all_package_versions =
        Package_registry.to_list registry
        |> List.fold_left ~init:Package.Name.Map.empty ~f:(fun acc reg_entry ->
          Package.Name.Map.set acc reg_entry.Package_registry.name reg_entry.version)
      in
      let context = Package_universe.context_name package_universe in
      (* Build the package record *)
      let paths =
        let base_paths = Paths.map_path write_paths ~f:Path.build in
        (* For toolchain packages (not cached), set prefix to the toolchain cache directory
           so files get installed there. This enables cache sharing across projects. *)
        let is_toolchain =
          Pkg_toolchain.is_compiler_and_toolchains_enabled info.Pkg_info.name
        in
        if is_toolchain && not is_cached
        then (
          let toolchain_prefix =
            Pkg_toolchain.installation_prefix pkg ~build_id |> Path.outside_build_dir
          in
          let install_roots =
            lazy
              (Pkg_toolchain.install_roots
                 ~prefix:(Path.as_outside_build_dir_exn toolchain_prefix))
          in
          let install_paths =
            lazy
              (Install.Paths.make
                 ~relative:Path.relative
                 ~package:info.name
                 ~roots:
                   (Lazy.force install_roots
                    |> Install.Roots.map ~f:Path.outside_build_dir))
          in
          { base_paths with
            prefix = toolchain_prefix
          ; install_roots =
              Lazy.map install_roots ~f:(Install.Roots.map ~f:Path.outside_build_dir)
          ; install_paths
          })
        else (
          (* For regular (non-toolchain) packages, use the shared install directory
             so %{prefix}%, %{lib}%, etc. expand to the correct paths *)
          let shared_prefix = Pkg_opam.Pkg_install.dir ~context |> Path.build in
          let shared_roots = Pkg_opam.Pkg_install.roots ~context in
          let install_paths =
            lazy
              (Install.Paths.make
                 ~relative:Path.relative
                 ~package:info.name
                 ~roots:shared_roots)
          in
          { base_paths with
            prefix = shared_prefix
          ; install_roots = lazy shared_roots
          ; install_paths
          })
      in
      let t =
        { Resolved_pkg.id
        ; build_command
        ; install_command
        ; depends
        ; vendored_depends
        ; depends_on_dune = has_dune_dep
        ; depexts = pkg.depexts
        ; paths
        ; write_paths
        ; info
        ; files_dir
        ; pkg_id
        ; exported_env = []
        ; all_package_versions
        ; build_id
        ; is_cached_toolchain = is_cached
        ; toolchain_cache_dir
        ; context
        ; is_dev_tool =
            (match package_universe with
             | Package_universe.Dev_tool dev_tool ->
               (* Only the main dev tool package (not its deps) installs to default context *)
               Package.Name.equal info.name (Dune_pkg.Dev_tool.package_name dev_tool)
             | Package_universe.Dependencies _ -> false)
        }
      in
      let+ exported_env =
        let expander =
          Action_expander.expander (Package_universe.context_name package_universe) t
        in
        Memo.parallel_map pkg.exported_env ~f:(Action_expander.exported_env expander)
      in
      t.exported_env <- exported_env;
      t
  ;;

  let resolve_entry_memo =
    Memo.create
      "pkg-resolve-entry"
      ~input:(module Entry_input)
      ~human_readable_description:(fun t ->
        Pp.textf "- package %s" (Package.Name.to_string t.entry.name))
      (fun input ->
         let* registry =
           Package_registry.of_ctx (Package_universe.context_name input.universe)
         in
         resolve_entry_impl registry input)
  ;;

  let resolve_entry _registry entry ~package_universe =
    Memo.exec resolve_entry_memo { entry; universe = package_universe }
  ;;
end

module Install_action = struct
  (* The install action does the following:

     1. Runs the install action in the lock file (if exists)
     2. Reads the .install file produced by the build command
     3. Discoves all the files produced by 1.
     4. Combines the set of files in 2. and 3. to produce a "cookie" file
  *)

  let installable_sections =
    Section.(Set.diff all (Set.of_list [ Misc; Libexec; Libexec_root ]))
    |> Section.Set.to_list
  ;;

  module Spec = struct
    type ('path, 'target) t =
      { (* location of the install file we must read (if produced) *)
        install_file : 'path
      ; (* location of the variables we must read (if produced) *)
        config_file : 'path
      ; (* where we are supposed to put the installed artifacts *)
        target_dir : 'target
      ; (* the PREFIX path for relocatability checking *)
        prefix : 'path
      ; (* if the package's installation prefix is outside the build
           dir, it's stored here and will be used instead of [target_dir]
           as the location of insntalled artifacts *)
        prefix_outside_build_dir : Path.Outside_build_dir.t option
      ; (* does the package have its own install command? *)
        install_action : [ `Has_install_action | `No_install_action ]
      ; package : Package.Name.t
      }

    let name = "install-file-run"
    let version = 1

    let bimap
          ({ install_file
           ; config_file
           ; target_dir
           ; prefix
           ; prefix_outside_build_dir = _
           ; install_action = _
           ; package = _
           } as t)
          f
          g
      =
      { t with
        install_file = f install_file
      ; config_file = f config_file
      ; target_dir = g target_dir
      ; prefix = f prefix
      }
    ;;

    let is_useful_to ~memoize = memoize

    let encode
          { install_file
          ; config_file
          ; target_dir
          ; prefix
          ; prefix_outside_build_dir
          ; install_action
          ; package
          }
          path
          target
      : Sexp.t
      =
      List
        [ path install_file
        ; path config_file
        ; target target_dir
        ; path prefix
        ; (match
             Option.map
               prefix_outside_build_dir
               ~f:Path.Outside_build_dir.to_string_maybe_quoted
           with
           | None -> List []
           | Some s -> List [ Atom s ])
        ; Atom (Package.Name.to_string package)
        ; Atom
            (match install_action with
             | `Has_install_action -> "has_install_action"
             | `No_install_action -> "no_install_action")
        ]
    ;;

    let prepare_copy ~install_file ~target_dir entry =
      let dst =
        let paths =
          let package =
            Path.basename install_file
            |> Filename.remove_extension
            |> Package.Name.of_string
          in
          let roots =
            Path.build target_dir
            |> Install.Roots.opam_from_prefix ~relative:Path.relative
          in
          Install.Paths.make ~relative:Path.relative ~package ~roots
        in
        Install.Entry.relative_installed_path entry ~paths
      in
      Path.mkdir_p (Path.parent_exn dst);
      dst
    ;;

    let readdir path =
      match Path.Untracked.readdir_unsorted_with_kinds path with
      | Error _ -> [], []
      | Ok listing ->
        List.partition_map listing ~f:(fun (basename, kind) ->
          let path = Path.relative path basename in
          match kind with
          | S_DIR -> Right path
          | _ -> Left path)
    ;;

    let rec collect paths acc =
      match paths with
      | [] -> acc
      | path :: paths ->
        let files, dirs = readdir path in
        let acc = List.rev_append files acc in
        collect (List.rev_append dirs paths) acc
    ;;

    let skip path skip =
      List.iter skip ~f:(fun s -> assert (Path.equal path (Path.parent_exn s)));
      let files, dirs = readdir path in
      let dirs =
        List.filter_map dirs ~f:(fun path ->
          if List.mem skip path ~equal:Path.equal then None else Some path)
      in
      files, dirs
    ;;

    let maybe_drop_sandbox_dir path =
      match Path.extract_build_context_dir_maybe_sandboxed path with
      | None -> path
      | Some (sandbox, source) ->
        let ctx =
          let name = Path.basename sandbox in
          Path.relative (Path.build Path.Build.root) name
        in
        Path.append_source ctx source
    ;;

    let section_map_of_dir install_paths =
      (* reverse engineer the installed artifacts from running the install
         action by looking at the file system post running the action and
         taking educated guesses about which section each file belongs to *)
      let get = Install.Paths.get install_paths in
      List.concat_map installable_sections ~f:(fun section ->
        let path = get section in
        let acc, dirs =
          match section with
          | Lib_root -> skip path [ get Toplevel; get Stublibs; get Lib ]
          | Share_root -> skip path [ get Share ]
          | _ -> [], [ path ]
        in
        collect dirs acc
        |> List.rev_map ~f:(fun file ->
          let section =
            match
              match section with
              | Lib_root -> Some Section.Libexec_root
              | Lib -> Some Libexec
              | _ -> None
            with
            | None -> section
            | Some section' ->
              let perm = (Unix.stat (Path.to_string file)).st_perm in
              if Path.Permissions.(test execute perm) then section' else section
          in
          section, maybe_drop_sandbox_dir file))
      |> Section.Map.of_list_multi
    ;;

    let maybe_set_executable section dst =
      match Section.should_set_executable_bit section with
      | false -> ()
      | true ->
        let dst = Path.to_string dst in
        let permission =
          let perm = (Unix.stat dst).st_perm in
          Path.Permissions.(add execute) perm
        in
        Unix.chmod dst permission
    ;;

    let read_variables config_file =
      match Path.Untracked.exists config_file with
      | false -> []
      | true ->
        let config =
          let filename = Path.to_string config_file in
          match
            Io.read_file config_file
            |> OpamFile.Dot_config.read_from_string
                 ~filename:(OpamFile.make (OpamFilename.of_string filename))
          with
          | s -> s
          | exception OpamPp.Bad_format (pos, message) ->
            let loc =
              Option.map
                pos
                ~f:(fun { OpamParserTypes.FullPos.filename = _; start; stop } ->
                  let file_contents = Io.read_file config_file in
                  let bols = ref [ 0 ] in
                  String.iteri file_contents ~f:(fun i ch ->
                    if ch = '\n' then bols := (i + 1) :: !bols);
                  let bols = Array.of_list (List.rev !bols) in
                  let make_pos (line, column) =
                    let pos_bol = bols.(line - 1) in
                    { Lexing.pos_fname = filename
                    ; pos_lnum = line
                    ; pos_bol
                    ; pos_cnum = pos_bol + column
                    }
                  in
                  let start = make_pos start in
                  let stop = make_pos stop in
                  Loc.create ~start ~stop)
            in
            let message_with_loc =
              (* The location is inlined b/c the original config file is going
                 to be deleted, so we don't be able to fetch the part of the
                 file that's bad *)
              let open Pp.O in
              let error = Pp.textf "Error parsing %s" (Path.basename config_file) in
              match loc with
              | None -> error
              | Some loc ->
                (Loc.pp loc |> Pp.map_tags ~f:(fun Loc.Loc -> User_message.Style.Loc))
                ++ error
            in
            User_error.raise
              [ message_with_loc; Pp.seq (Pp.text "Reason: ") (Pp.text message) ]
        in
        OpamFile.Dot_config.bindings config
        |> List.map ~f:(fun (name, value) -> Package_variable_name.of_opam name, value)
    ;;

    (* Install a single entry from .install file to target_dir.
       The copy to shared prefix is handled by copy_to_prefix_rule. *)
    let install_entry
          ~src
          ~install_file
          ~target_dir
          (entry : Path.t Install.Entry.Expanded.t)
      =
      match Path.Untracked.exists src, entry.optional with
      | false, true -> None
      | false, false ->
        User_error.raise
          (* TODO loc *)
          [ Pp.textf
              "entry %s in %s does not exist"
              (Path.to_string_maybe_quoted src)
              (Path.to_string install_file)
          ]
      | true, _ ->
        let dst = prepare_copy ~install_file ~target_dir entry in
        let src =
          match Path.to_string src |> Unix.readlink with
          | exception Unix.Unix_error (_, _, _) -> src
          | link ->
            Path.external_
              (let base = Path.parent_exn src in
               Filename.concat (Path.to_absolute_filename base) link
               |> Path.External.of_string)
        in
        Io.portable_hardlink ~src ~dst;
        maybe_set_executable entry.section dst;
        Some (entry.section, dst)
    ;;

    (* Process install action: scan target_dir for installed files and create cookie.
       All files are installed to target_dir only - copying to shared prefix is done
       by a separate rule to enable caching of target_dir contents. *)
    let action
          { package
          ; install_file
          ; config_file
          ; target_dir
          ; prefix
          ; prefix_outside_build_dir
          ; install_action
          }
          ~ectx:_
          ~eenv:_
      =
      let open Fiber.O in
      let* () = Fiber.return () in
      (* For install actions, scan target_dir (where PREFIX pointed during install).
         The install command ran with PREFIX=target_dir so files are there. *)
      let* files =
        let from_install_action =
          match install_action with
          | `No_install_action -> Section.Map.empty
          | `Has_install_action ->
            (* Install action wrote to target_dir, scan it for installed files.
               Use the actual target_dir path, not Paths.of_root which would
               create nested directories. *)
            let roots =
              Install.Roots.opam_from_prefix
                ~relative:Path.relative
                (Path.build target_dir)
            in
            let install_paths =
              Install.Paths.make ~relative:Path.relative ~package ~roots
            in
            section_map_of_dir install_paths
        in
        let+ from_install_file =
          (* Read all the artifacts from the .install file produced by
             the build command. This is the happy path where we don't guess
             anything. *)
          Async.async (fun () -> Path.Untracked.exists install_file)
          >>= function
          | false -> Fiber.return Section.Map.empty
          | true ->
            let* map =
              let install_entries =
                let dir = Path.parent_exn install_file in
                Install.Entry.Expanded.load_install_file install_file (fun local ->
                  Path.append_local dir local)
              in
              let by_src =
                List.rev_map install_entries ~f:(fun (entry : _ Install.Entry.t) ->
                  entry.src, entry)
                |> Path.Map.of_list_multi
              in
              let+ install_entries =
                Path.Map.to_list_map by_src ~f:(fun src entries ->
                  List.map entries ~f:(fun entry -> src, entry))
                |> List.concat
                |> Fiber.parallel_map ~f:(fun (src, entry) ->
                  Async.async (fun () ->
                    install_entry ~src ~install_file ~target_dir entry))
                >>| List.filter_opt
              in
              List.rev_map install_entries ~f:(fun (section, file) ->
                let file = maybe_drop_sandbox_dir file in
                section, file)
              |> Section.Map.of_list_multi
            in
            let+ () =
              Async.async (fun () -> Fpath.unlink_exn (Path.to_string install_file))
            in
            map
        in
        (* Combine the artifacts declared in the .install, and the ones we discovered
           by running the install action *)
        (* TODO we should make sure that overwrites aren't allowed *)
        Section.Map.union from_install_action from_install_file ~f:(fun _ x y ->
          Some (x @ y))
        |> Section.Map.map ~f:(List.sort ~compare:Path.compare)
      in
      let* cookies =
        let+ variables = Async.async (fun () -> read_variables config_file) in
        { Install_cookie.Gen.files; variables }
      in
      (* Produce the cookie file at root level (sibling of target/) for split rules *)
      let cookie_file = Path.build @@ Paths.install_cookie_build target_dir in
      let* () =
        Async.async (fun () ->
          cookie_file |> Path.parent_exn |> Path.mkdir_p;
          Install_cookie.dump cookie_file cookies)
      in
      (* For toolchain packages, also write cookie to cache location for persistence *)
      let* () =
        match prefix_outside_build_dir with
        | None -> Fiber.return ()
        | Some prefix_dir ->
          let cache_cookie =
            Path.outside_build_dir (Path.Outside_build_dir.relative prefix_dir "cookie")
          in
          Async.async (fun () ->
            cache_cookie |> Path.parent_exn |> Path.mkdir_p;
            Install_cookie.dump cache_cookie cookies)
      in
      (* Check that installed files don't contain absolute paths (relocatability check) *)
      let+ () =
        Async.async (fun () ->
          let install_dir = Path.build target_dir in
          Relocatable_check.check ~pkg_name:package ~prefix ~install_dir)
      in
      Dune_engine.Progress.finish_target ~name:(Package.Name.to_string package)
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action (p : Path.Build.t Paths.t) install_action ~prefix ~prefix_outside_build_dir =
    A.action
      { Spec.install_file = Path.build @@ Paths.install_file p
      ; config_file = Path.build @@ Paths.config_file p
      ; target_dir = p.target_dir
      ; prefix
      ; prefix_outside_build_dir
      ; install_action
      ; package = p.name
      }
  ;;
end

let add_env env action =
  Action_builder.With_targets.map action ~f:(Action.Full.add_env env)
;;

let rule ?loc { Action_builder.With_targets.build; targets } =
  (* TODO this ignores the workspace file *)
  Rule.make ~info:(Rule.Info.of_loc_opt loc) ~targets build |> Rules.Produce.rule
;;

let source_rules (pkg : Resolved_pkg.t) =
  let+ source_deps, copy_rules =
    match pkg.info.source with
    | None -> Memo.return (Dep.Set.empty, [])
    | Some source ->
      let loc = fst source.url in
      Lock_dir.source_kind source
      >>= (function
       | `Local (`File, _) | `Fetch ->
         let fetch =
           Fetch_rules.fetch ~target:pkg.write_paths.source_dir `Directory source
           |> With_targets.map
                ~f:
                  (Action.Full.map ~f:(fun action ->
                     let progress =
                       Pkg_build_progress.progress_action
                         pkg.info.name
                         pkg.info.version
                         `Downloading
                     in
                     Action.progn [ progress; action ]))
         in
         Memo.return (Dep.Set.of_files [ pkg.paths.source_dir ], [ loc, fetch ])
       | `Local (`Directory, source_root) ->
         let+ source_files, rules =
           let source_root = Path.external_ source_root in
           Resolved_pkg.source_files pkg ~loc
           >>| Path.Local.Set.fold ~init:([], []) ~f:(fun file (source_files, rules) ->
             let src = Path.append_local source_root file in
             if Path.is_broken_symlink src
             then
               (* Don't copy broken symlinks into the build directory. Note
                  that this only works for packages sourced from local
                  directories. Packages whose source is extracted from an
                  archive (possibly fetched over the web) have broken symlinks
                  explicitly deleted immediately after the archive is
                  extracted. This logic is implemented in the "source-fetch"
                  action spec in [Fetch_rules]. *)
               source_files, rules
             else (
               let dst = Path.Build.append_local pkg.write_paths.source_dir file in
               let copy = loc, Action_builder.copy ~src ~dst in
               Path.build dst :: source_files, copy :: rules))
         in
         Dep.Set.of_files source_files, rules)
  in
  let extra_source_deps, extra_copy_rules =
    List.map pkg.info.extra_sources ~f:(fun (local, (fetch : Source.t)) ->
      let extra_source = Paths.extra_source_build pkg.write_paths local in
      let rule =
        let loc = fst fetch.url in
        (* We assume that [fetch] is always a file. Would be good
           to give a decent error message if it's not *)
        match Source.kind fetch with
        | `Directory_or_archive src ->
          loc, Action_builder.copy ~src:(Path.external_ src) ~dst:extra_source
        | `Fetch ->
          let rule = Fetch_rules.fetch ~target:extra_source `File fetch in
          loc, rule
      in
      Path.build extra_source, rule)
    |> List.unzip
  in
  let copy_rules = copy_rules @ extra_copy_rules in
  let source_deps = Dep.Set.union source_deps (Dep.Set.of_files extra_source_deps) in
  source_deps, Memo.parallel_iter copy_rules ~f:(fun (loc, copy) -> rule ~loc copy)
;;

let rec scan_contents p =
  let module P = Path.Build in
  let dir_contents =
    match Readdir.read_directory_with_kinds (P.to_string p) with
    | Ok dir_contents -> dir_contents
    | Error e ->
      Code_error.raise
        "Failure to enumerate files"
        [ "error", Unix_error.Detailed.to_dyn e ]
  in
  List.fold_left
    dir_contents
    ~init:(P.Set.empty, P.Set.empty)
    ~f:(fun (files, empty_directories) (file_name, file_kind) ->
      let p = P.relative p file_name in
      match (file_kind : Unix.file_kind) with
      | S_REG -> P.Set.add files p, empty_directories
      | S_DIR ->
        let recursive_files, recursive_empty_dir = scan_contents p in
        (match P.Set.is_empty recursive_files, P.Set.is_empty recursive_empty_dir with
         | true, true ->
           recursive_files, P.Set.union empty_directories recursive_empty_dir
         | true, false -> files, P.Set.union empty_directories recursive_empty_dir
         | false, _ -> P.Set.union files recursive_files, empty_directories)
      | otherwise ->
        Code_error.raise
          "Unsupported directory content"
          [ "path", P.to_dyn p; "file_kind", File_kind.to_dyn otherwise ])
;;

let files path =
  let files, empty_directories = scan_contents path in
  let to_path_set set =
    Path.Build.Set.fold
      set
      ~f:(fun e acc -> Path.Set.add acc (Path.build e))
      ~init:Path.Set.empty
  in
  let files = to_path_set files in
  let empty_directories = to_path_set empty_directories in
  Dep.Set.of_source_files ~files ~empty_directories, files
;;

let dune_dep =
  lazy (Sys.executable_name |> Path.External.of_string |> Path.external_ |> Dep.file)
;;

(* Build sandbox mode - restricts writes to declared targets *)
let build_sandbox = Sandbox_config.needs_sandboxing

(* Cookie file path inside target_dir *)
let install_cookie_path (pkg : Resolved_pkg.t) =
  Paths.install_cookie_build pkg.write_paths.target_dir
;;

(* Rule 1: Build + Install to target_dir (CACHEABLE)
   - Copies sources
   - Runs build command
   - Runs install command with PREFIX=target_dir
   - Processes .install file, scans target_dir for installed files
   - Produces target_dir (directory target) + cookie file

   This rule's output (target_dir) can be cached and restored on cache hit. *)
let build_and_install_rule context_name ~source_deps (pkg : Resolved_pkg.t) =
  let+ build_and_install_actions =
    let+ copy_action =
      let+ copy_action =
        let+ () = Memo.return () in
        let open Action_builder.O in
        [ Action_builder.with_no_targets
          @@ ((match pkg.files_dir with
               | Some files_dir -> Action_builder.path (Path.build files_dir)
               | None -> Action_builder.return ())
              >>> Action_builder.of_memo
                    (Memo.of_thunk (fun () ->
                       match pkg.files_dir with
                       | None -> Memo.return (Path.Set.empty, Dep.Set.empty)
                       | Some files_dir ->
                         let deps, source_deps = files files_dir in
                         Memo.return (source_deps, deps)))
              |> Action_builder.dyn_deps
              >>= fun source_deps ->
              Path.Set.to_list_map source_deps ~f:(fun src ->
                let dst =
                  let prefix = pkg.files_dir |> Option.value_exn |> Path.build in
                  let local_path = Path.drop_prefix_exn src ~prefix in
                  Path.Build.append_local pkg.write_paths.source_dir local_path
                in
                Action.progn
                  [ Action.mkdir (Path.Build.parent_exn dst); Action.copy src dst ])
              |> Action.concurrent
              |> Action.Full.make ~sandbox:build_sandbox
              |> Action_builder.return)
        ]
      in
      copy_action
      @ List.map pkg.info.extra_sources ~f:(fun (local, _) ->
        let src = Paths.extra_source pkg.paths local in
        let dst = Path.Build.append_local pkg.write_paths.source_dir local in
        Action.progn
          [ Action.mkdir pkg.write_paths.source_dir
          ; Action.remove_tree dst
          ; Action.copy src dst
          ]
        |> Action.Full.make ~sandbox:build_sandbox
        |> Action_builder.With_targets.return)
    and+ build_action =
      match Action_expander.build_command context_name pkg with
      | None -> Memo.return []
      | Some build_command -> build_command >>| List.singleton
    and+ install_action =
      (* Install command writes to target_dir for caching *)
      match Action_expander.install_command_to_target_dir context_name pkg with
      | None -> Memo.return []
      | Some install_command -> install_command >>| List.singleton
    in
    copy_action, build_action, install_action
  in
  let copy_action, build_action, install_action = build_and_install_actions in
  (* Action to print a progress message for the package *)
  let progress_building =
    let status = if pkg.is_cached_toolchain then `Cached else `Building in
    Pkg_build_progress.progress_action pkg.info.name pkg.info.version status
    |> Action.Full.make ~sandbox:build_sandbox
    |> Action_builder.return
    |> Action_builder.with_no_targets
  in
  (* Create source and target directories *)
  let mkdir_pkg_dirs =
    Action.progn
      [ Action.mkdir pkg.write_paths.source_dir; Action.mkdir pkg.write_paths.target_dir ]
    |> Action.Full.make ~sandbox:build_sandbox
    |> Action_builder.With_targets.return
  in
  (* Create install directories in target_dir before build/install.
     Install paths now point to target_dir since that's our install prefix. *)
  let mkdir_install_dirs =
    let target_install_paths =
      let roots =
        Install.Roots.opam_from_prefix
          ~relative:Path.Build.relative
          pkg.write_paths.target_dir
      in
      Install.Paths.make ~relative:Path.Build.relative ~package:pkg.info.name ~roots
    in
    Install_action.installable_sections
    |> List.rev_map ~f:(fun section ->
      Install.Paths.get target_install_paths section |> Action.mkdir)
    |> Action.progn
    |> Action.Full.make ~sandbox:build_sandbox
    |> Action_builder.With_targets.return
  in
  (* Process .install file and scan for installed files, create cookie.
     All files are written to target_dir - copy_to_prefix_rule handles copying to shared prefix. *)
  let has_install_action =
    Option.is_some (Action_expander.install_command_to_target_dir context_name pkg)
  in
  let install_file_action =
    let prefix_outside_build_dir = Path.as_outside_build_dir pkg.paths.prefix in
    Install_action.action
      pkg.write_paths
      (if has_install_action then `Has_install_action else `No_install_action)
      ~prefix:(Path.build pkg.write_paths.target_dir)
      ~prefix_outside_build_dir
    |> Action.Full.make ~sandbox:build_sandbox
    |> Action_builder.return
    (* Cookie is inside target_dir, so it's implicitly a target via the directory target *)
    |> Action_builder.with_no_targets
  in
  let actions =
    [ copy_action
    ; [ progress_building; mkdir_pkg_dirs; mkdir_install_dirs ]
    ; build_action
    ; install_action
    ; [ install_file_action ]
    ]
    |> List.concat
    |> Action_builder.progn
    (* Declare target_dir as a directory target for caching *)
    |> Action_builder.With_targets.add_directories
         ~directory_targets:[ pkg.write_paths.target_dir ]
  in
  let open Action_builder.With_targets.O in
  (let deps =
     let deps = Dep.Set.union source_deps (Resolved_pkg.package_deps pkg) in
     let deps = Dep.Set.union deps (Resolved_pkg.vendored_deps pkg) in
     let deps =
       match pkg.depends_on_dune with
       | false -> deps
       | true -> Dep.Set.add deps (Lazy.force dune_dep)
     in
     match pkg.toolchain_cache_dir with
     | None -> deps
     | Some cache_dir ->
       let ocaml_binary = Path.relative cache_dir "target/bin/ocaml" in
       if Path.Untracked.exists ocaml_binary
       then Dep.Set.add deps (Dep.file ocaml_binary)
       else deps
   in
   Action_builder.deps deps |> Action_builder.with_no_targets)
  >>> add_env (Resolved_pkg.exported_env pkg) actions
;;

(* Action spec for copying files from target_dir to shared prefix *)
module Copy_to_prefix_action = struct
  module Spec = struct
    type ('path, 'target) t =
      { cookie_file : 'path
      ; target_dir : 'path
      ; prefix : 'target (* shared install prefix *)
      ; package : Package.Name.t
      }

    let name = "copy-to-prefix"
    let version = 1

    let bimap { cookie_file; target_dir; prefix; package } f g =
      { cookie_file = f cookie_file
      ; target_dir = f target_dir
      ; prefix = g prefix
      ; package
      }
    ;;

    let is_useful_to ~memoize:_ = false (* Don't cache this action *)

    let encode { cookie_file; target_dir; prefix; package } path target : Sexp.t =
      List
        [ path cookie_file
        ; path target_dir
        ; target prefix
        ; Atom (Package.Name.to_string package)
        ]
    ;;

    (* Copy a single file from src to dst, using hardlink with fallback to copy
       for cross-filesystem scenarios (EXDEV error). *)
    let copy_file ~src ~dst =
      Path.mkdir_p (Path.parent_exn dst);
      try Io.portable_hardlink ~src ~dst with
      | Unix.Unix_error (Unix.EXDEV, _, _) ->
        (* Cross-device link not permitted - fall back to copy *)
        Io.copy_file ~src ~dst ()
    ;;

    let action { cookie_file; target_dir; prefix; package } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let* () = Fiber.return () in
      (* Read the cookie to get the list of files.
         Provide a clear error message including the package name if loading fails. *)
      let* cookie =
        Async.async (fun () ->
          match Install_cookie.Persistent.load cookie_file with
          | Some cookie ->
            { Install_cookie.Gen.files = Section.Map.of_list_exn cookie.files
            ; variables = cookie.variables
            }
          | None ->
            User_error.raise
              [ Pp.textf
                  "Failed to load install cookie for package %s"
                  (Package.Name.to_string package)
              ; Pp.textf "Cookie file: %s" (Path.to_string cookie_file)
              ])
      in
      (* Copy each file from target_dir to prefix *)
      let prefix_path = Path.build prefix in
      let+ () =
        Section.Map.to_list cookie.files
        |> List.concat_map ~f:(fun (_section, files) -> files)
        |> Fiber.parallel_iter ~f:(fun src_in_target ->
          Async.async (fun () ->
            (* src_in_target is the file path in target_dir.
               Compute the relative path and apply it to prefix. *)
            let target_dir_path = target_dir in
            match Path.drop_prefix src_in_target ~prefix:target_dir_path with
            | None ->
              (* File is not under target_dir - might be from external source *)
              ()
            | Some relative ->
              let dst = Path.append_local prefix_path relative in
              if Path.Untracked.exists src_in_target
              then copy_file ~src:src_in_target ~dst))
      in
      ()
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action ~cookie_file ~target_dir ~prefix ~package =
    A.action { Spec.cookie_file; target_dir; prefix; package }
  ;;
end

(* Installed marker path - produced by copy_to_prefix rule *)
let installed_marker_path (pkg : Resolved_pkg.t) =
  Paths.installed_marker_build pkg.write_paths.target_dir
;;

(* Rule 2: Copy from target_dir to shared prefix (NOT CACHED)
   - Depends on cookie file (which means build+install completed)
   - Reads cookie to get list of installed files
   - Copies each file from target_dir to shared prefix
   - Produces installed marker file

   This rule modifies the shared install directory and should not be cached. *)
let copy_to_prefix_rule (pkg : Resolved_pkg.t) =
  let cookie_path = install_cookie_path pkg in
  (* Dev tools install to the default context so their binaries are available in PATH *)
  let shared_prefix =
    let context = if pkg.is_dev_tool then Context_name.default else pkg.context in
    Pkg_opam.Pkg_install.dir ~context
  in
  (* The copy action *)
  let copy_action =
    Copy_to_prefix_action.action
      ~cookie_file:(Path.build cookie_path)
      ~target_dir:(Path.build pkg.write_paths.target_dir)
      ~prefix:shared_prefix
      ~package:pkg.info.name
    |> Action.Full.make
         ~sandbox:Sandbox_config.no_special_requirements
         ~can_go_in_shared_cache:false
    |> Action_builder.return
    |> Action_builder.with_no_targets
  in
  let progress_action =
    Pkg_build_progress.progress_action pkg.info.name pkg.info.version `Installing
    |> Action.Full.make
         ~sandbox:Sandbox_config.no_special_requirements
         ~can_go_in_shared_cache:false
    |> Action_builder.return
    |> Action_builder.with_no_targets
  in
  (* Create installed marker file with metadata for debugging *)
  let marker_action =
    let marker_contents =
      sprintf
        "package: %s\nversion: %s\nprefix: %s\n"
        (Package.Name.to_string pkg.info.name)
        (Package_version.to_string pkg.info.version)
        (Path.Build.to_string shared_prefix)
    in
    Action.write_file (installed_marker_path pkg) marker_contents
    |> Action.Full.make
         ~sandbox:Sandbox_config.no_special_requirements
         ~can_go_in_shared_cache:false
    |> Action_builder.return
    |> Action_builder.with_file_targets ~file_targets:[ installed_marker_path pkg ]
  in
  let open Action_builder.With_targets.O in
  (* Depend on cookie file *)
  Action_builder.path (Path.build cookie_path)
  |> Action_builder.with_no_targets
  >>> Action_builder.progn [ progress_action; copy_action; marker_action ]
;;

let gen_rules context_name (pkg : Resolved_pkg.t) =
  let* source_deps, copy_rules = source_rules pkg in
  let* () = copy_rules
  and* build_install_rule = build_and_install_rule context_name pkg ~source_deps
  and* copy_rule = Memo.return (copy_to_prefix_rule pkg) in
  let* () = rule ~loc:Loc.none build_install_rule in
  rule ~loc:Loc.none copy_rule
;;

module Gen_rules = Build_config.Gen_rules

let pkg_alias_disabled =
  Action_builder.fail
    { fail =
        (fun () ->
          let error =
            [ Pp.text "The @pkg-install alias cannot be used without a lock dir" ]
          in
          let hints =
            [ Pp.concat
                ~sep:Pp.space
                [ Pp.text "You might want to create the lock dir with"
                ; User_message.command "dune pkg lock"
                ]
            ]
          in
          User_error.raise ~hints error)
    }
;;

(* Vendor build helpers - extracted from setup_pkg_install_alias for clarity *)
module Vendor_build = struct
  (* Classify a registry entry's vendor status.
     - From_vendor entries have vendor stanzas - check build method
     - From_lock entries need pkg rules (will be vendored by build rules) *)
  let classify_entry (entry : Package_registry.entry) =
    match entry.source with
    | Package_registry.Source.From_lock { pkg = _ } ->
      (* Lock packages need pkg rules to fetch/build - they'll be vendored by the rules *)
      Memo.return Vendor_status.Not_vendored
    | Package_registry.Source.From_vendor { source_dir; stanza } ->
      (* Vendor packages - determine build method from stanza first *)
      (match stanza.Vendor_stanza.build_method with
       | Some Vendor_stanza.Build_method.Opam_sandboxed ->
         Memo.return Vendor_status.Opam_sandboxed
       | Some Vendor_stanza.Build_method.Dune_native ->
         Memo.return Vendor_status.Dune_native
       | None ->
         (* No explicit build method - check opam file if it exists *)
         let pkg_name = Package.Name.to_string entry.name in
         (match Vendor_rules.find_opam_file ~pkg_name ~pkg_dir:source_dir with
          | None ->
            (* No opam file and no explicit stanza - default to Dune_native
              since dune packages don't need opam files *)
            Memo.return Vendor_status.Dune_native
          | Some opam_path ->
            let contents = Io.read_file ~binary:true (Path.source opam_path) in
            (match OpamFile.OPAM.read_from_string contents with
             | exception _ -> Memo.return Vendor_status.Opam_sandboxed
             | opam ->
               let abs_path =
                 Path.source source_dir
                 |> Path.to_absolute_filename
                 |> Path.External.of_string
               in
               let source = Source.external_copy (Loc.none, abs_path) in
               (match
                  Pkg.of_opam_file
                    ~name:entry.name
                    ~version:entry.version
                    ~source
                    ~opam
                    ()
                with
                | Error _ -> Memo.return Vendor_status.Opam_sandboxed
                | Ok pkg ->
                  let method_ = Vendor.classify_build_method pkg in
                  (match method_ with
                   | Vendor.Dune_native -> Memo.return Vendor_status.Dune_native
                   | Vendor.Opam_sandboxed -> Memo.return Vendor_status.Opam_sandboxed)))))
  ;;

  let build_packages_of_context ctx_name =
    let open Action_builder.O in
    let* pkg_ids =
      Action_builder.of_memo
        (let open Memo.O in
         let* registry = Package_registry.of_ctx ctx_name in
         let entries = Package_registry.to_list registry in
         let+ filtered =
           Memo.parallel_map entries ~f:(fun entry ->
             let+ status = classify_entry entry in
             match status with
             | Vendor_status.Dune_native -> None
             | Vendor_status.Not_vendored | Vendor_status.Opam_sandboxed ->
               let pkg_id =
                 Pkg_id.create ~name:entry.Package_registry.name ~version:entry.version
               in
               Some pkg_id)
         in
         let pkg_ids = List.filter_map filtered ~f:Fun.id in
         let num_pkgs = List.length pkg_ids in
         Pkg_build_progress.Progress.set_total num_pkgs;
         Dune_engine.Progress.set_total num_pkgs;
         pkg_ids)
    in
    List.map pkg_ids ~f:(fun pkg_id ->
      let paths =
        Paths.make ~relative:Path.Build.relative pkg_id (Dependencies ctx_name)
      in
      (* Depend on the installed marker which is produced by the copy_to_prefix rule *)
      Paths.installed_marker_build paths.target_dir |> Path.build)
    |> Action_builder.paths
  ;;

  (* Get the marker path for a vendor package without building rules.
     The actual rules are generated from the pkg context via setup_pkg_context_rules. *)
  let marker_path_for_vendor ~ctx_name ~subdir ~opam_path =
    let opam_contents = Io.read_file ~binary:true (Path.source opam_path) in
    let opam_file =
      Dune_pkg.Opam_file.read_from_string_exn
        ~contents:opam_contents
        (Path.source opam_path)
    in
    (* Get package name from opam file, fallback to parsing directory name *)
    let pkg_name =
      match OpamFile.OPAM.name_opt opam_file with
      | Some n -> Package.Name.of_string (OpamPackage.Name.to_string n)
      | None -> Pkg_id.name_of_string subdir
    in
    let pkg_version =
      match OpamFile.OPAM.version_opt opam_file with
      | Some v -> Package_version.of_string (OpamPackage.Version.to_string v)
      | None -> Package_version.of_string "dev"
    in
    (* Marker is at _build/.pkgs/<ctx>/<name>.<version>/installed (root level, sibling of target/) *)
    Vendor_rules.marker_for_package ~context:ctx_name pkg_name
    >>| function
    | Some marker -> marker
    | None ->
      (* Fallback: compute the path directly if not in vendored map yet *)
      let pkg_dir =
        sprintf
          "%s.%s"
          (Package.Name.to_string pkg_name)
          (Package_version.to_string pkg_version)
      in
      let root = Path.Build.relative (build_dir ctx_name) pkg_dir in
      (* Installed marker at root level, sibling of target/ *)
      Path.Build.relative root "installed"
  ;;

  let classify_vendor_stanzas ctx_name =
    let open Memo.O in
    Source_tree.all_vendor_stanzas ()
    >>= Memo.parallel_map ~f:(fun (subdir_path, stanza) ->
      let subdir = Path.Source.basename subdir_path in
      let build_dir =
        Path.Build.append_source (Context_name.build_dir ctx_name) subdir_path
      in
      match stanza.Vendor_stanza.build_method with
      | Some Vendor_stanza.Build_method.Opam_sandboxed ->
        let dune_project_path = Path.Source.relative subdir_path "dune-project" in
        let* has_dune_project =
          Fs_memo.file_exists (Path.Outside_build_dir.In_source_dir dune_project_path)
        in
        if has_dune_project
        then Memo.return (`Dune_package build_dir)
        else (
          (* Look for opam files. Priority: opam, then <name>.opam *)
          let dir_name = Path.Source.basename subdir_path in
          let pkg_name = Vendor_rules.parse_pkg_name_from_dir dir_name in
          let candidates =
            [ Path.Source.relative subdir_path "opam"
            ; Path.Source.relative subdir_path (pkg_name ^ ".opam")
            ]
          in
          let* opam_file_opt =
            Memo.List.find_map candidates ~f:(fun opam_path ->
              Fs_memo.file_exists (Path.Outside_build_dir.In_source_dir opam_path)
              >>| function
              | true -> Some opam_path
              | false -> None)
          in
          match opam_file_opt with
          | None ->
            User_error.raise
              [ Pp.textf
                  "Vendor directory %s has (mode opam) but no opam file found. Try \
                   running 'dune pkg fetch' to generate opam files."
                  (Path.Source.to_string subdir_path)
              ]
          | Some opam_path -> Memo.return (`Opam_sandbox (subdir, subdir_path, opam_path)))
      | _ -> Memo.return (`Dune_package build_dir))
  ;;

  (* Get marker paths for vendor packages. Rules are NOT registered here -
     they are generated from the pkg context via setup_pkg_context_rules. *)
  let get_opam_sandbox_markers ctx_name opam_sandbox_pkgs =
    Memo.parallel_map opam_sandbox_pkgs ~f:(fun (subdir, _vendor_dir, opam_path) ->
      marker_path_for_vendor ~ctx_name ~subdir ~opam_path)
  ;;

  let build_vendor_deps dune_dirs opam_markers =
    let open Action_builder.O in
    let* () = Action_builder.paths_existing (List.map dune_dirs ~f:Path.build) in
    Action_builder.paths_existing (List.map opam_markers ~f:Path.build)
  ;;

  let has_vendor_sandbox_packages () =
    let open Memo.O in
    Source_tree.all_vendor_stanzas ()
    >>| List.exists ~f:(fun (_subdir_path, stanza) ->
      match stanza.Vendor_stanza.build_method with
      | Some Vendor_stanza.Build_method.Opam_sandboxed -> true
      | _ -> false)
  ;;
end

let setup_pkg_install_alias ~dir ctx_name =
  let rule =
    match
      let build_dir = Context_name.build_dir ctx_name in
      Path.Build.equal dir build_dir
    with
    | false -> Memo.return Rules.empty
    | true ->
      let* active = Lock_dir.lock_dir_active ctx_name in
      let alias = Alias.make ~dir Alias0.pkg_install in
      Rules.collect_unit (fun () ->
        let* vendor_info = Vendor_build.classify_vendor_stanzas ctx_name in
        let dune_dirs, opam_sandbox_pkgs =
          List.partition_map vendor_info ~f:(function
            | `Dune_package dir -> Left dir
            | `Opam_sandbox info -> Right info)
        in
        let* opam_markers =
          Vendor_build.get_opam_sandbox_markers ctx_name opam_sandbox_pkgs
        in
        let* deps =
          match active with
          | true ->
            Memo.return
              (let open Action_builder.O in
               let* () = Vendor_build.build_packages_of_context ctx_name in
               Vendor_build.build_vendor_deps dune_dirs opam_markers)
          | false ->
            let+ has_vendor = Vendor_build.has_vendor_sandbox_packages () in
            if has_vendor
            then Vendor_build.build_vendor_deps dune_dirs opam_markers
            else pkg_alias_disabled
        in
        Rules.Produce.Alias.add_deps alias deps)
  in
  Gen_rules.rules_for ~dir ~allowed_subdirs:Filename.Set.empty rule
  |> Gen_rules.rules_here
;;

(* Setup package rules from a registry entry using the unified resolve path *)
let setup_package_rules_from_entry
      (registry : Package_registry.t)
      (entry : Package_registry.entry)
      ~package_universe
      ~dir
  : Gen_rules.result Memo.t
  =
  (* Check vendor status based on the entry source *)
  let* vendor_status =
    match entry.source with
    | Package_registry.Source.From_vendor { stanza; _ } ->
      (match stanza.build_method with
       | Some Vendor_stanza.Build_method.Dune_native | None ->
         Memo.return Vendor_status.Dune_native
       | Some Opam_sandboxed -> Memo.return Vendor_status.Opam_sandboxed)
    | Package_registry.Source.From_lock { pkg = _ } ->
      (* Lock packages are not vendored (vendor takes precedence in registry) *)
      Memo.return Vendor_status.Not_vendored
  in
  match vendor_status with
  | Vendor_status.Dune_native ->
    (* Vendored dune packages are built as vendored code in the main context.
       They have NO pkg rules - the libraries/binaries come from normal dune build. *)
    Memo.return @@ Gen_rules.rules_here Gen_rules.Rules.empty
  | Vendor_status.Not_vendored | Vendor_status.Opam_sandboxed ->
    let* pkg = Resolve.resolve_entry registry entry ~package_universe in
    let paths = Paths.make pkg.pkg_id package_universe ~relative:Path.Build.relative in
    let+ directory_targets =
      (* source_dir is a directory target for packages with fetched sources (produced by source_rules).
         target_dir is a directory target containing installed files (produced by build_and_install_rule). *)
      match pkg.info.source with
      | None ->
        (* No source - only target_dir is a directory target (source_dir is just mkdir'd) *)
        Memo.return (Path.Build.Map.singleton paths.target_dir Loc.none)
      | Some source ->
        Lock_dir.source_kind source
        >>| (function
         | `Local (`Directory, _) ->
           (* Local directory source - only target_dir is a directory target *)
           Path.Build.Map.singleton paths.target_dir Loc.none
         | `Local (`File, _) | `Fetch ->
           (* Fetched source - both source_dir (from fetch) and target_dir (from build+install) *)
           Path.Build.Map.of_list_exn
             [ paths.source_dir, fst source.url; paths.target_dir, Loc.none ])
    in
    let build_dir_only_sub_dirs =
      Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
    in
    let context_name = Package_universe.context_name package_universe in
    let rules = Rules.collect_unit (fun () -> gen_rules context_name pkg) in
    Gen_rules.make ~directory_targets ~build_dir_only_sub_dirs rules
;;

(* Set up package rules for a dev tool context.
   Dev tool contexts are not workspace contexts, so we load their lock
   directories directly instead of using Package_registry. *)
let setup_dev_tool_pkg_rules dev_tool ~dir pkg_id =
  let* lock_dir_opt = Dev_tool.load_lock_dir_if_exists dev_tool in
  match lock_dir_opt with
  | None -> Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
  | Some lock_dir ->
    let* platform = Lock_dir.Sys_vars.solver_env in
    let pkgs = Dune_pkg.Lock.packages_on_platform lock_dir ~platform in
    (match Package.Name.Map.find pkgs pkg_id.Pkg_id.name with
     | None -> Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
     | Some pkg
       when not (Package_version.equal pkg.Dune_pkg.Pkg.info.version pkg_id.version) ->
       (* Version mismatch *)
       Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
     | Some _pkg ->
       (* Create a registry from all lock packages for dependency resolution *)
       let registry = Package_registry.of_lock_packages pkgs in
       (match
          Package_registry.find_by_name_version
            registry
            ~name:pkg_id.name
            ~version:pkg_id.version
        with
        | None -> Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
        | Some entry ->
          setup_package_rules_from_entry
            registry
            entry
            ~package_universe:(Dev_tool dev_tool)
            ~dir))
;;

let setup_pkg_context_rules ctx ~dir ~components =
  match components with
  | [] ->
    (* _build/.pkgs/<ctx>/ - list all package digests *)
    Gen_rules.make
      ~build_dir_only_sub_dirs:
        (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
      (Memo.return Rules.empty)
    |> Memo.return
  | [ pkg_dir_string ] ->
    (* _build/.pkgs/<ctx>/<pkg_dir>/ - set up package build rules. *)
    let pkg_id = Pkg_id.of_string pkg_dir_string in
    (* Check if this is a dev tool context *)
    (match Dev_tool.of_context_name ctx with
     | Some dev_tool ->
       (* Dev tool context - load lock directory directly *)
       setup_dev_tool_pkg_rules dev_tool ~dir pkg_id
     | None ->
       (* Regular context - use Package_registry *)
       let* registry = Package_registry.of_ctx ctx in
       (match
          Package_registry.find_by_name_version
            registry
            ~name:pkg_id.name
            ~version:pkg_id.version
        with
        | None ->
          (* Package not found in registry - no rules *)
          Memo.return @@ Gen_rules.make (Memo.return Rules.empty)
        | Some entry ->
          setup_package_rules_from_entry
            registry
            entry
            ~package_universe:(Dependencies ctx)
            ~dir))
  | _ :: _ ->
    (* Subdirectories within a package build - redirect to parent *)
    Memo.return @@ Gen_rules.redirect_to_parent Gen_rules.Rules.empty
;;

let resolve_pkg_dep context (loc, package_name) =
  let* registry = Package_registry.of_ctx context in
  match Package_registry.find registry package_name with
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf "Package %S not found" (Package.Name.to_string package_name) ]
  | Some entry ->
    Resolve.resolve_entry registry entry ~package_universe:(Dependencies context)
;;

let ocaml_toolchain context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf
      "Loading OCaml toolchain from Lock directory for context %S"
      (Context_name.to_string context))
  @@ fun () ->
  let* lock_dir = Lock_dir.get_exn context in
  match lock_dir.ocaml with
  | None -> Memo.return None
  | Some ocaml ->
    let+ pkg = resolve_pkg_dep context ocaml in
    let toolchain =
      let open Action_builder.O in
      let transitive_deps = pkg :: Resolved_pkg.deps_closure pkg in
      let* env, binaries =
        Action_builder.List.fold_left
          ~init:(Global.env (), Path.Set.empty)
          ~f:(fun (env, binaries) pkg ->
            let env = Env.extend_env env (Resolved_pkg.exported_env pkg) in
            let+ cookie = (Pkg_installed.of_paths pkg.paths).cookie in
            let binaries =
              Section.Map.find cookie.files Bin
              |> Option.value ~default:[]
              |> Path.Set.of_list
              |> Path.Set.union binaries
            in
            env, binaries)
          transitive_deps
      in
      let path = Env_path.path (Global.env ()) in
      Action_builder.of_memo @@ Ocaml_toolchain.of_binaries ~path context env binaries
    in
    Some (Action_builder.memoize "ocaml_toolchain" toolchain)
;;

let all_deps universe =
  let ctx = Package_universe.context_name universe in
  let* registry = Package_registry.of_ctx ctx in
  Package_registry.to_list registry
  |> Memo.parallel_map ~f:(fun entry ->
    (* Filter out vendored dune packages - they're built as vendored code *)
    match entry.Package_registry.source with
    | Package_registry.Source.From_vendor { stanza; _ } ->
      (match stanza.build_method with
       | Some Vendor_stanza.Build_method.Dune_native | None -> Memo.return None
       | Some Opam_sandboxed ->
         let+ pkg = Resolve.resolve_entry registry entry ~package_universe:universe in
         Some pkg)
    | Package_registry.Source.From_lock _ ->
      let+ pkg = Resolve.resolve_entry registry entry ~package_universe:universe in
      Some pkg)
  >>| List.filter_map ~f:Fun.id
  >>| Resolved_pkg.top_closure
;;

let all_project_deps context = all_deps (Dependencies context)

let which context =
  let artifacts_and_deps =
    Memo.lazy_
      ~human_readable_description:(fun () ->
        Pp.textf
          "Loading all binaries in the lock directory for %S"
          (Context_name.to_string context))
      (fun () ->
         let+ { binaries; dep_info = _ } =
           all_project_deps context >>= Action_expander.Artifacts_and_deps.of_closure
         in
         binaries)
  in
  Staged.stage (fun program ->
    let+ artifacts = Memo.Lazy.force artifacts_and_deps in
    Filename.Map.find artifacts program)
;;

let ocamlpath universe =
  let+ all_project_deps = all_deps universe in
  let env = Resolved_pkg.build_env_of_deps all_project_deps in
  Env.Map.find env Dune_findlib.Config.ocamlpath_var
  |> Option.value ~default:[]
  |> List.map ~f:(function
    | Value.Dir p | Path p -> p
    | String s -> Path.of_filename_relative_to_initial_cwd s)
;;

let project_ocamlpath context = ocamlpath (Dependencies context)
let dev_tool_ocamlpath dev_tool = ocamlpath (Dev_tool dev_tool)
let lock_dir_active = Lock_dir.lock_dir_active
let lock_dir_path = Lock_dir.get_path

let dev_tool_env tool =
  let package_name = Dune_pkg.Dev_tool.package_name tool in
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf
      "lock directory environment for dev tools %S"
      (Package.Name.to_string package_name))
  @@ fun () ->
  let ctx = Dev_tool.context_name tool in
  let* registry = Package_registry.of_ctx ctx in
  match Package_registry.find registry package_name with
  | None ->
    User_error.raise
      [ Pp.textf "Dev tool package %S not found" (Package.Name.to_string package_name) ]
  | Some entry ->
    let+ pkg = Resolve.resolve_entry registry entry ~package_universe:(Dev_tool tool) in
    Resolved_pkg.exported_env pkg
;;

let exported_env context =
  Memo.push_stack_frame ~human_readable_description:(fun () ->
    Pp.textf "lock directory environment for context %S" (Context_name.to_string context))
  @@ fun () ->
  let+ all_project_deps = all_project_deps context in
  let env = Resolved_pkg.build_env_of_deps all_project_deps in
  let vars = Env.Map.map env ~f:Value_list_env.string_of_env_values in
  Env.extend Env.empty ~vars
;;

let find_package ctx pkg =
  lock_dir_active ctx
  >>= function
  | false -> Memo.return None
  | true ->
    let+ pkg = resolve_pkg_dep ctx (Loc.none, pkg) in
    Some
      (let open Action_builder.O in
       let+ _cookie = (Pkg_installed.of_paths pkg.paths).cookie in
       ())
;;

let all_filtered_depexts context =
  let* all_project_deps = all_project_deps context in
  Memo.List.map all_project_deps ~f:(fun (pkg : Resolved_pkg.t) ->
    let expander = Action_expander.expander context pkg in
    Action_expander.Expander.filtered_depexts expander)
  >>| List.concat
  >>| List.sort_uniq ~compare:String.compare
;;

(* Returns depexts with their source package info *)
let all_filtered_depexts_with_origins context =
  let* all_project_deps = all_project_deps context in
  Memo.List.map all_project_deps ~f:(fun (pkg : Resolved_pkg.t) ->
    let expander = Action_expander.expander context pkg in
    let+ depexts = Action_expander.Expander.filtered_depexts expander in
    let pkg_name = pkg.info.name in
    let pkg_version = pkg.info.version in
    List.map depexts ~f:(fun depext -> depext, pkg_name, pkg_version))
  >>| List.concat
  >>| List.sort ~compare:(fun (d1, _, _) (d2, _, _) -> String.compare d1 d2)
;;

let pkg_id_of_project_dependency ctx package_name =
  let+ registry = Package_registry.of_ctx ctx in
  match Package_registry.find registry package_name with
  | None -> None
  | Some entry ->
    Some (Pkg_id.create ~name:entry.Package_registry.name ~version:entry.version)
;;
