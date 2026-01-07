open Import
module Vendor = Dune_pkg.Vendor
module Vendor_stanza = Dune_lang.Vendor_stanza

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

let lib_cache_file =
  lazy
    (let pkg_dir = Path.build (Path.Build.relative Path.Build.root "_build/.pkg") in
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
    let opam_file = Path.Source.relative pkg_dir (pkg_name ^ ".opam") in
    if Path.Untracked.exists (Path.source opam_file)
    then (
      let contents = Io.read_file ~binary:true (Path.source opam_file) in
      match OpamFile.OPAM.read_from_string contents with
      | exception _ -> Package_version.of_string "dev"
      | opam ->
        (match OpamFile.OPAM.version_opt opam with
         | Some v -> Package_version.of_string (OpamPackage.Version.to_string v)
         | None -> Package_version.of_string "dev"))
    else Package_version.of_string "dev"
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

let get_vendored_map =
  let impl () =
    let open Memo.O in
    let vendor_dir = Vendor.default_dir in
    let+ stanzas = Source_tree.vendor_stanzas vendor_dir in
    List.fold_left stanzas ~init:Vendored_map.empty ~f:(fun map (subdir, stanza) ->
      let pkg_dir = Path.Source.relative vendor_dir subdir in
      let pkg_name =
        match String.lsplit2 subdir ~on:'.' with
        | Some (name, _version) -> name
        | None -> subdir
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

let marker_for_package ~context pkg_name =
  let open Memo.O in
  let+ map = get_vendored_map () in
  if Vendored_map.needs_marker map pkg_name
  then
    Some
      (Path.Build.relative
         (Context_name.build_dir context)
         (sprintf ".pkg/vendor-%s.marker" (Package.Name.to_string pkg_name)))
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

(* Opam variable expansion (see https://opam.ocaml.org/doc/Manual.html#Variables):
   Global: %{make}%, %{jobs}%, %{arch}%, %{os}%, %{os-family}%, %{os-distribution}%,
           %{os-version}%
   Switch: %{prefix}%, %{lib}%, %{bin}%, %{sbin}%, %{share}%, %{doc}%, %{etc}%,
           %{man}%, %{toplevel}%, %{stublibs}%
   Package: %{name}%, %{version}%, %{pkg:installed}%, %{pkg:enable}%,
            %{pkg:lib}%, %{pkg:share}%, %{pkg:etc}%, %{pkg:doc}%
   Build: %{_:name}%, %{_:lib}%, %{_:share}%, %{_:etc}% for current package *)
let build_opam_package ~context ~pkg_name ~pkg_version ~source_dir ~opam_file =
  let open Memo.O in
  let* vendored_map = get_vendored_map () in
  let build_cmds = OpamFile.OPAM.build opam_file in
  let install_cmds = OpamFile.OPAM.install opam_file in
  (* Get install paths for variable expansion *)
  let install_dir = Install.Context.dir ~context in
  let prefix = Path.build install_dir in
  let roots = Install.Roots.opam_from_prefix ~relative:Path.Build.relative install_dir in
  let ocamlfind_destdir = Path.build roots.lib_root in
  (* For vendor builds, use the build directory path.
     The vendored_dirs stanza in duniverse/dune will mirror source files there. *)
  let build_dir = Path.Build.append_source (Context_name.build_dir context) source_dir in
  let build_path = Path.build build_dir in
  let system_path = Global.env () |> Env_path.path in
  (* Simple string replacement helper *)
  let replace_all s ~pattern ~with_ =
    Re.replace_string (Re.compile (Re.str pattern)) ~by:with_ s
  in
  (* Make command - gmake on BSD, make elsewhere *)
  let make_cmd =
    match Bin.which ~path:system_path "gmake" with
    | Some _ -> "gmake"
    | None -> "make"
  in
  (* Expand package variable like %{foo:installed}% *)
  let expand_pkg_var pkg_str var =
    let pkg = Package.Name.of_string pkg_str in
    match var with
    | "installed" -> Some (Bool.to_string (Vendored_map.is_installed vendored_map pkg))
    | "enable" ->
      Some (if Vendored_map.is_installed vendored_map pkg then "enable" else "disable")
    | "version" ->
      (match Vendored_map.version vendored_map pkg with
       | Some v -> Some (Package_version.to_string v)
       | None -> Some "")
    | _ -> None
  in
  (* Regex for %{pkg:var}% pattern *)
  let pkg_var_re = Re.compile (Re.Perl.re {|%\{([^:}]+):([^}]+)\}%|}) in
  (* Expand opam variables in a string *)
  let expand_vars s =
    (* First expand simple variables *)
    let s = replace_all s ~pattern:"%{prefix}%" ~with_:(Path.to_string prefix) in
    let s = replace_all s ~pattern:"%{lib}%" ~with_:(Path.to_string ocamlfind_destdir) in
    let s = replace_all s ~pattern:"%{name}%" ~with_:(Package.Name.to_string pkg_name) in
    let s =
      replace_all s ~pattern:"%{jobs}%" ~with_:(Int.to_string !Clflags.concurrency)
    in
    let s = replace_all s ~pattern:"%{make}%" ~with_:make_cmd in
    (* Then expand %{pkg:var}% patterns *)
    Re.replace pkg_var_re s ~f:(fun group ->
      let pkg_str = Re.Group.get group 1 in
      let var = Re.Group.get group 2 in
      match expand_pkg_var pkg_str var with
      | Some value -> value
      | None -> Re.Group.get group 0 (* keep original if unknown *))
  in
  (* Convert opam command to action *)
  let cmd_to_action (args, _filter) =
    let args =
      List.filter_map args ~f:(fun (arg, _filter) ->
        match (arg : OpamTypes.simple_arg) with
        | CString s -> Some (expand_vars s)
        | CIdent i ->
          (* Handle opam idents like %{make}% *)
          (match i with
           | "make" ->
             Some
               (match Bin.which ~path:system_path "gmake" with
                | Some _ -> "gmake"
                | None -> "make")
           | "jobs" -> Some (Int.to_string !Clflags.concurrency)
           | "name" -> Some (Package.Name.to_string pkg_name)
           | "prefix" -> Some (Path.to_string prefix)
           | "lib" -> Some (Path.to_string ocamlfind_destdir)
           | _ -> None))
    in
    match args with
    | [] -> None
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
      Some
        (Run_with_path.action
           ~pkg:(pkg_name, Loc.none)
           ~depexts:[]
           prog_path
           args_arr
           ~prefix
           ~ocamlfind_destdir)
  in
  let build_actions = List.filter_map build_cmds ~f:cmd_to_action in
  let install_actions = List.filter_map install_cmds ~f:cmd_to_action in
  let progress_building =
    Pkg_build_progress.progress_action pkg_name pkg_version `Building
  in
  let progress_installing =
    Pkg_build_progress.progress_action pkg_name pkg_version `Installing
  in
  let marker_file =
    Path.Build.relative
      (Context_name.build_dir context)
      (sprintf ".pkg/vendor-%s.marker" (Package.Name.to_string pkg_name))
  in
  (* Create marker file to indicate successful build/install *)
  let marker_action = Action.write_file marker_file "" in
  let all_actions =
    [ progress_building ]
    @ build_actions
    @ [ progress_installing ]
    @ install_actions
    @ [ marker_action ]
  in
  let action =
    Action.chdir build_path (Action.progn all_actions)
    |> Action.Full.make ~env:Env.initial
  in
  (* Depend on all files in the build dir to trigger vendored_dirs mirroring. *)
  let file_selector =
    File_selector.of_predicate_lang ~dir:build_path Predicate_lang.true_
  in
  let deps = Action_builder.paths_matching_unit ~loc:Loc.none file_selector in
  let with_targets =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps
    >>> (Action_builder.return action
         |> Action_builder.with_no_targets
         |> Action_builder.With_targets.add ~file_targets:[ marker_file ])
  in
  Memo.return (marker_file, with_targets)
;;
