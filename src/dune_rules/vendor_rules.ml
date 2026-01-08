open Import
module Vendor = Dune_pkg.Vendor
module Vendor_stanza = Dune_lang.Vendor_stanza

(* Extract all package names from an opam dependency formula.
   This ignores version constraints and filters - just collects the names. *)
let extract_dep_names (formula : OpamTypes.filtered_formula) =
  let names = ref [] in
  OpamFormula.iter (fun (name, _condition) -> names := name :: !names) formula;
  !names
;;

(* Path to a package's build directory: _build/.pkgs/<ctx>/<name>.<version>/ *)
let pkg_build_root ~context ~pkg_name ~pkg_version =
  let ctx_dir =
    Path.Build.relative Dpath.Build.pkgs_dir (Context_name.to_string context)
  in
  let pkg_dir =
    sprintf
      "%s.%s"
      (Package.Name.to_string pkg_name)
      (Package_version.to_string pkg_version)
  in
  Path.Build.relative ctx_dir pkg_dir
;;

let extract_public_name_from_sexp sexp =
  let open Dune_sexp.Ast in
  let atom_to_string = function
    | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
    | _ -> None
  in
  match sexp with
  | List (_, Atom (_, lib_atom) :: fields)
    when String.equal (Dune_sexp.Atom.to_string lib_atom) "library" ->
    List.find_map fields ~f:(fun field ->
      match field with
      | List (_, [ Atom (_, key); value ])
        when String.equal (Dune_sexp.Atom.to_string key) "public_name" ->
        atom_to_string value
      | _ -> None)
  | _ -> None
;;

let scan_dune_file path =
  let full_path = Path.source path in
  if Path.Untracked.exists full_path
  then (
    let contents = Io.read_file ~binary:true full_path in
    match
      Dune_sexp.Parser.parse_string
        ~fname:(Path.Source.to_string path)
        ~mode:Many
        contents
    with
    | exception _ -> []
    | sexps -> List.filter_map sexps ~f:extract_public_name_from_sexp)
  else []
;;

let rec scan_dir_for_dune_files dir =
  let full_path = Path.source dir in
  if not (Path.Untracked.exists full_path)
  then []
  else (
    let dune_file = Path.Source.relative dir "dune" in
    let libs = scan_dune_file dune_file in
    match Path.Untracked.readdir_unsorted_with_kinds full_path with
    | Error _ -> libs
    | Ok entries ->
      let subdirs =
        List.filter_map entries ~f:(fun (entry, kind) ->
          match kind with
          | Unix.S_DIR -> Some (Path.Source.relative dir entry)
          | _ -> None)
      in
      let subdir_libs = List.concat_map subdirs ~f:scan_dir_for_dune_files in
      libs @ subdir_libs)
;;

let scan_public_libraries dir = scan_dir_for_dune_files dir

let scan_meta_libraries dir ~pkg_name =
  let meta_file_in_pkg = Path.Source.relative dir (pkg_name ^ "/META") in
  let meta_file = Path.Source.relative dir "META" in
  if
    Path.Untracked.exists (Path.source meta_file_in_pkg)
    || Path.Untracked.exists (Path.source meta_file)
  then [ pkg_name ]
  else []
;;

let scan_opam_libraries dir =
  match Path.Untracked.readdir_unsorted (Path.source dir) with
  | Error _ -> []
  | Ok entries ->
    List.filter_map entries ~f:(fun entry ->
      if String.is_suffix entry ~suffix:".opam"
      then Some (Filename.remove_extension entry)
      else None)
;;

(* Find opam file in a directory. Checks both "opam" and "<name>.opam". *)
let find_opam_file ~pkg_name ~pkg_dir =
  let candidates =
    [ Path.Source.relative pkg_dir "opam"
    ; Path.Source.relative pkg_dir (pkg_name ^ ".opam")
    ]
  in
  List.find candidates ~f:(fun p -> Path.Untracked.exists (Path.source p))
;;

let scan_libraries dir ~pkg_name =
  match scan_public_libraries dir with
  | [] ->
    (match scan_meta_libraries dir ~pkg_name with
     | [] -> scan_opam_libraries dir
     | libs -> libs)
  | libs -> libs
;;

let read_project_name dir =
  let dune_project = Path.Source.relative dir "dune-project" in
  let full_path = Path.source dune_project in
  if Path.Untracked.exists full_path
  then (
    let contents = Io.read_file ~binary:true full_path in
    match
      Dune_sexp.Parser.parse_string
        ~fname:(Path.Source.to_string dune_project)
        ~mode:Many
        contents
    with
    | exception _ -> None
    | sexps ->
      List.find_map sexps ~f:(fun sexp ->
        let open Dune_sexp.Ast in
        match sexp with
        | List (_, Atom (_, name_atom) :: Atom (_, value) :: _)
          when String.equal (Dune_sexp.Atom.to_string name_atom) "name" ->
          Some (Dune_sexp.Atom.to_string value)
        | _ -> None))
  else None
;;

(* Lib-cache for looking up library→directory mappings during dune pkg fetch.
   The cache maps library names to directory names in duniverse/. *)
let lib_cache_file =
  lazy
    (let pkg_dir = Path.build Dpath.Build.pkgs_dir in
     Path.relative pkg_dir "lib-cache")
;;

let lib_cache : string String.Table.t option ref = ref None

let load_lib_cache () =
  let cache_file = Lazy.force lib_cache_file in
  if Path.Untracked.exists cache_file
  then (
    let lines = Io.lines_of_file cache_file in
    let tbl = String.Table.create 256 in
    List.iter lines ~f:(fun line ->
      match String.lsplit2 line ~on:':' with
      | Some (lib, dir) -> String.Table.set tbl lib dir
      | None -> ());
    Some tbl)
  else None
;;

let find_dir_for_library lib_name =
  let cache =
    match !lib_cache with
    | Some c -> Some c
    | None ->
      let c = load_lib_cache () in
      lib_cache := c;
      c
  in
  match cache with
  | None -> None
  | Some tbl -> String.Table.find tbl lib_name
;;

let invalidate_lib_cache () = lib_cache := None

module Vendored_map = struct
  type package_info =
    { version : Package_version.t
    ; source_dir : Path.Source.t
    ; libraries : string list
    ; build_method : Dune_lang.Vendor_stanza.Build_method.t option
      (* None or Some Dune_native = built as workspace code, no marker needed
         Some Opam_sandboxed = built in sandbox, needs marker file *)
    }

  type t =
    { packages : package_info Package.Name.Map.t
    ; lib_to_package : Package.Name.t String.Map.t
    }

  let empty = { packages = Package.Name.Map.empty; lib_to_package = String.Map.empty }

  let add t ~name ~version ~source_dir ~libraries ~build_method =
    let info = { version; source_dir; libraries; build_method } in
    let packages = Package.Name.Map.set t.packages name info in
    let lib_to_package =
      List.fold_left libraries ~init:t.lib_to_package ~f:(fun acc lib ->
        String.Map.set acc lib name)
    in
    { packages; lib_to_package }
  ;;

  let find t name = Package.Name.Map.find t.packages name
  let is_installed t name = Package.Name.Map.mem t.packages name

  let version t name =
    match Package.Name.Map.find t.packages name with
    | Some info -> Some info.version
    | None -> None
  ;;

  let source_dir t name =
    match Package.Name.Map.find t.packages name with
    | Some info -> Some info.source_dir
    | None -> None
  ;;

  let package_for_library t lib_name = String.Map.find t.lib_to_package lib_name
  let all_packages t = Package.Name.Map.keys t.packages

  let needs_marker t name =
    match Package.Name.Map.find t.packages name with
    | None -> false
    | Some info ->
      (match info.build_method with
       | Some Vendor_stanza.Build_method.Opam_sandboxed -> true
       | Some Dune_native | None -> false)
  ;;
end

(* Scan a vendor package directory and extract package info *)
let scan_vendor_package ~pkg_name ~pkg_dir =
  let version =
    match find_opam_file ~pkg_name ~pkg_dir with
    | None -> Package_version.of_string "dev"
    | Some opam_file ->
      let contents = Io.read_file ~binary:true (Path.source opam_file) in
      (match OpamFile.OPAM.read_from_string contents with
       | exception _ -> Package_version.of_string "dev"
       | opam ->
         (match OpamFile.OPAM.version_opt opam with
          | Some v -> Package_version.of_string (OpamPackage.Version.to_string v)
          | None -> Package_version.of_string "dev"))
  in
  let libraries = scan_libraries pkg_dir ~pkg_name in
  version, libraries
;;

(* Scan duniverse directory and build vendored package map *)
let scan_vendor_dir vendor_dir =
  let full_path = Path.source vendor_dir in
  if not (Path.Untracked.exists full_path)
  then Vendored_map.empty
  else (
    match Path.Untracked.readdir_unsorted_with_kinds full_path with
    | Error _ -> Vendored_map.empty
    | Ok entries ->
      List.fold_left entries ~init:Vendored_map.empty ~f:(fun map (entry, kind) ->
        match kind with
        | Unix.S_DIR ->
          let pkg_name = entry in
          let pkg_dir = Path.Source.relative vendor_dir entry in
          let version, libraries = scan_vendor_package ~pkg_name ~pkg_dir in
          Vendored_map.add
            map
            ~name:(Package.Name.of_string pkg_name)
            ~version
            ~source_dir:pkg_dir
            ~libraries
            ~build_method:None
        | _ -> map))
;;

(* Parse package name from directory format "<name>.<version>" using OpamPackage *)
let parse_pkg_name_from_dir dir_name =
  match OpamPackage.of_string_opt dir_name with
  | Some pkg -> OpamPackage.Name.to_string (OpamPackage.name pkg)
  | None -> dir_name (* fallback to full name if parsing fails *)
;;

(* Try to read package name from opam file in directory *)
let read_pkg_name_from_opam pkg_dir =
  let full_path = Path.source pkg_dir in
  match Path.Untracked.readdir_unsorted full_path with
  | Error _ -> None
  | Ok entries ->
    List.find_map entries ~f:(fun entry ->
      if String.equal entry "opam" || String.is_suffix entry ~suffix:".opam"
      then (
        let opam_path = Path.relative full_path entry in
        if Path.Untracked.exists opam_path
        then (
          let contents = Io.read_file ~binary:true opam_path in
          match OpamFile.OPAM.read_from_string contents with
          | exception _ -> None
          | opam ->
            (match OpamFile.OPAM.name_opt opam with
             | Some n -> Some (OpamPackage.Name.to_string n)
             | None -> None))
        else None)
      else None)
;;

let get_vendored_map =
  let impl () =
    let open Memo.O in
    let+ stanzas = Source_tree.all_vendor_stanzas () in
    List.fold_left stanzas ~init:Vendored_map.empty ~f:(fun map (pkg_dir, stanza) ->
      let subdir = Path.Source.basename pkg_dir in
      (* Try to get package name from opam file, fallback to directory parsing *)
      let pkg_name =
        match read_pkg_name_from_opam pkg_dir with
        | Some name -> name
        | None -> parse_pkg_name_from_dir subdir
      in
      let version, libraries = scan_vendor_package ~pkg_name ~pkg_dir in
      Vendored_map.add
        map
        ~name:(Package.Name.of_string pkg_name)
        ~version
        ~source_dir:pkg_dir
        ~libraries
        ~build_method:stanza.Vendor_stanza.build_method)
  in
  Memo.lazy_ ~name:"vendored-map" impl
;;

let get_vendored_map () = Memo.Lazy.force get_vendored_map

module Paths = struct
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  let map_path t ~f =
    { t with
      source_dir = f t.source_dir
    ; target_dir = f t.target_dir
    ; extra_sources = f t.extra_sources
    ; install_roots = Lazy.map ~f:(Install.Roots.map ~f) t.install_roots
    ; install_paths = Lazy.map ~f:(Install.Paths.map ~f) t.install_paths
    ; prefix = f t.prefix
    }
  ;;

  let install_roots ~target_dir ~relative =
    Install.Roots.opam_from_prefix ~relative target_dir
  ;;

  let install_paths roots package ~relative = Install.Paths.make ~relative ~package ~roots

  let of_root name ~root ~relative =
    let source_dir = relative root "source" in
    let target_dir = relative root "target" in
    let extra_sources = relative root "extra_source" in
    let install_roots = lazy (install_roots ~target_dir ~relative) in
    let install_paths = lazy (install_paths (Lazy.force install_roots) name ~relative) in
    { source_dir
    ; target_dir
    ; extra_sources
    ; name
    ; install_paths
    ; install_roots
    ; prefix = target_dir
    }
  ;;

  let make_install_cookie target_dir ~relative = relative target_dir "cookie"

  let install_cookie' target_dir =
    make_install_cookie target_dir ~relative:Path.Build.relative
  ;;

  let install_cookie t = make_install_cookie t.target_dir ~relative:Path.relative
  let target_dir t = t.target_dir
  let source_dir t = t.source_dir
  let install_paths t = Lazy.force t.install_paths
end

let marker_for_package ~context pkg_name =
  let open Memo.O in
  let+ map = get_vendored_map () in
  if Vendored_map.needs_marker map pkg_name
  then (
    let version =
      match Vendored_map.find map pkg_name with
      | None -> Package_version.of_string "dev"
      | Some info -> info.version
    in
    (* _build/.pkgs/<ctx>/<name>.<version>/target/cookie *)
    let root = pkg_build_root ~context ~pkg_name ~pkg_version:version in
    let paths = Paths.of_root pkg_name ~root ~relative:Path.Build.relative in
    Some (Paths.install_cookie' (Paths.target_dir paths)))
  else None
;;

(** Given a library name, find the vendor package that provides it and return
    the marker file needed to trigger that package's build.
    Returns None if the library is not from a vendor package. *)
let marker_for_library ~context lib_name =
  let open Memo.O in
  let* map = get_vendored_map () in
  match Vendored_map.package_for_library map lib_name with
  | None -> Memo.return None
  | Some pkg_name -> marker_for_package ~context pkg_name
;;

(* Build an opam package using the shared Pkg_opam expansion logic *)
let build_opam_package ~context ~pkg_name ~pkg_version ~source_dir ~opam_file =
  let open Memo.O in
  let* vendored_map = get_vendored_map () in
  let build_cmds = OpamFile.OPAM.build opam_file in
  let install_cmds = OpamFile.OPAM.install opam_file in
  (* Extract dependencies from the opam file and filter to vendored packages *)
  let opam_depends = OpamFile.OPAM.depends opam_file in
  let dep_names = extract_dep_names opam_depends in
  let vendored_deps =
    List.filter_map dep_names ~f:(fun opam_name ->
      let name = Package.Name.of_string (OpamPackage.Name.to_string opam_name) in
      match Vendored_map.find vendored_map name with
      | None -> None
      | Some info -> Some (name, info))
  in
  (* Convert vendored_map to all_packages for Pkg_opam *)
  let all_packages =
    List.fold_left
      (Vendored_map.all_packages vendored_map)
      ~init:Package.Name.Map.empty
      ~f:(fun acc pkg ->
        match Vendored_map.version vendored_map pkg with
        | Some v -> Package.Name.Map.set acc pkg v
        | None -> acc)
  in
  (* Get install paths - use absolute paths since vendor builds chdir
     to the package directory *)
  let install_dir = Pkg_opam.Pkg_install.dir ~context in
  let prefix = Path.of_string (Path.to_absolute_filename (Path.build install_dir)) in
  let roots = Pkg_opam.Pkg_install.roots ~context in
  let ocamlfind_destdir = Path.of_string (Path.to_absolute_filename roots.lib_root) in
  (* For vendor builds, use the build directory path *)
  let build_dir = Path.Build.append_source (Context_name.build_dir context) source_dir in
  let build_path = Path.build build_dir in
  let system_path = Global.env () |> Env_path.path in
  let expand_vars s =
    Pkg_opam.expand_string
      ~context
      ~pkg_name
      ~pkg_version
      ~all_packages
      ~prefix
      ~ocamlfind_destdir
      s
  in
  let expand_ident var =
    Pkg_opam.expand_ident ~context ~pkg_name ~pkg_version ~prefix ~ocamlfind_destdir var
  in
  let cmd_to_action (args, _filter) =
    let* args =
      Memo.List.filter_map args ~f:(fun (arg, _filter) ->
        match (arg : OpamTypes.simple_arg) with
        | CString s ->
          let+ expanded = expand_vars s in
          Some expanded
        | CIdent i -> Memo.return (expand_ident i))
    in
    match args with
    | [] -> Memo.return None
    | prog_str :: cmd_args ->
      let prog_path =
        match Filename.analyze_program_name prog_str with
        | Absolute -> Ok (Path.of_string prog_str)
        | Relative_to_current_dir -> Ok (Path.relative build_path prog_str)
        | In_path ->
          (match Bin.which ~path:system_path prog_str with
           | Some p -> Ok p
           | None ->
             Error (Action.Prog.Not_found.create ~program:prog_str ~context ~loc:None ()))
      in
      let args_arr =
        Array.Immutable.of_list_map cmd_args ~f:(fun arg ->
          Array.Immutable.of_list [ Run_with_path.Spec.String arg ])
      in
      Memo.return
        (Some
           (Run_with_path.action
              ~pkg:(pkg_name, Loc.none)
              ~depexts:[]
              prog_path
              args_arr
              ~prefix
              ~ocamlfind_destdir))
  in
  let* build_actions = Memo.List.filter_map build_cmds ~f:cmd_to_action in
  let* install_actions = Memo.List.filter_map install_cmds ~f:cmd_to_action in
  let progress_building =
    Pkg_build_progress.progress_action pkg_name pkg_version `Building
  in
  let progress_installing =
    Pkg_build_progress.progress_action pkg_name pkg_version `Installing
  in
  (* _build/.pkgs/<ctx>/<name>.<version>/target/cookie *)
  let root = pkg_build_root ~context ~pkg_name ~pkg_version in
  let paths = Paths.of_root pkg_name ~root ~relative:Path.Build.relative in
  let target_dir = Paths.target_dir paths in
  let marker_file = Paths.install_cookie' target_dir in
  let mkdir_action = Action.mkdir target_dir in
  let marker_action = Action.write_file marker_file "" in
  let all_actions =
    [ progress_building ]
    @ build_actions
    @ [ progress_installing ]
    @ install_actions
    @ [ mkdir_action; marker_action ]
  in
  let action =
    Action.chdir build_path (Action.progn all_actions)
    |> Action.Full.make ~env:Env.initial
  in
  (* Depend on all files in the build dir to trigger vendored_dirs mirroring. *)
  let file_selector =
    File_selector.of_predicate_lang ~dir:build_path Predicate_lang.true_
  in
  let source_deps = Action_builder.paths_matching_unit ~loc:Loc.none file_selector in
  (* Build dependencies on other vendored packages *)
  let pkg_deps =
    List.map vendored_deps ~f:(fun (dep_name, dep_info) ->
      match dep_info.Vendored_map.build_method with
      | Some Vendor_stanza.Build_method.Opam_sandboxed ->
        (* Opam-sandboxed packages: depend on their marker file *)
        let dep_root =
          pkg_build_root ~context ~pkg_name:dep_name ~pkg_version:dep_info.version
        in
        let dep_paths =
          Paths.of_root dep_name ~root:dep_root ~relative:Path.Build.relative
        in
        let dep_target_dir = Paths.target_dir dep_paths in
        let dep_cookie = Paths.install_cookie' dep_target_dir in
        Dep.file (Path.build dep_cookie)
      | Some Dune_native | None ->
        (* Native dune packages: depend on @install alias to populate install dir *)
        let dir =
          Path.Build.append_source (Context_name.build_dir context) dep_info.source_dir
        in
        Dep.alias (Alias.make Alias0.install ~dir))
    |> Dep.Set.of_list
  in
  let deps =
    Action_builder.O.(
      source_deps >>> Action_builder.deps pkg_deps |> Action_builder.map ~f:(fun _ -> ()))
  in
  let with_targets =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps
    >>> (Action_builder.return action
         |> Action_builder.with_no_targets
         |> Action_builder.With_targets.add_directories ~directory_targets:[ target_dir ]
        )
  in
  Memo.return (marker_file, with_targets)
;;

let setup_vendor_package_rules ~context ~pkg_dir =
  let open Memo.O in
  (* Parse pkg_dir which is like "foo.1.0.0" into name using OpamPackage *)
  let pkg_name =
    match OpamPackage.of_string_opt pkg_dir with
    | Some pkg ->
      Package.Name.of_string (OpamPackage.Name.to_string (OpamPackage.name pkg))
    | None -> Package.Name.of_string pkg_dir
  in
  let* map = get_vendored_map () in
  match Vendored_map.find map pkg_name with
  | None -> Memo.return None
  | Some info ->
    (* Only handle opam-sandboxed packages here *)
    (match info.build_method with
     | Some Vendor_stanza.Build_method.Opam_sandboxed ->
       let pkg_name_str = Package.Name.to_string pkg_name in
       (match find_opam_file ~pkg_name:pkg_name_str ~pkg_dir:info.source_dir with
        | None -> Memo.return None
        | Some opam_file_path ->
          let contents = Io.read_file ~binary:true (Path.source opam_file_path) in
          (match OpamFile.OPAM.read_from_string contents with
           | exception _ -> Memo.return None
           | opam_file ->
             let+ _marker, action =
               build_opam_package
                 ~context
                 ~pkg_name
                 ~pkg_version:info.version
                 ~source_dir:info.source_dir
                 ~opam_file
             in
             Some action))
     | Some Dune_native | None ->
       (* Native dune packages don't need special rules here *)
       Memo.return None)
;;
