open Import
module Vendor = Dune_pkg.Vendor
module Vendor_stanza = Dune_lang.Vendor_stanza

(* Extract (public_name, package) from a library stanza.
   Returns (public_name, Some pkg) if package is specified, (public_name, None) otherwise. *)
let extract_public_name_and_package_from_sexp sexp =
  let open Dune_sexp.Ast in
  let atom_to_string = function
    | Atom (_, a) -> Some (Dune_sexp.Atom.to_string a)
    | _ -> None
  in
  match sexp with
  | List (_, Atom (_, lib_atom) :: fields)
    when String.equal (Dune_sexp.Atom.to_string lib_atom) "library" ->
    let public_name = ref None in
    let package = ref None in
    List.iter fields ~f:(fun field ->
      match field with
      | List (_, [ Atom (_, key); value ]) ->
        let key_str = Dune_sexp.Atom.to_string key in
        if String.equal key_str "public_name"
        then public_name := atom_to_string value
        else if String.equal key_str "package"
        then package := atom_to_string value
      | _ -> ());
    (match !public_name with
     | Some name -> Some (name, !package)
     | None -> None)
  | _ -> None
;;

(* Determine if a library belongs to a package.
   Only libraries with explicit (package X) field matching pkg_name are included. *)
let library_belongs_to_package ~pkg_name (_public_name, explicit_package) =
  match explicit_package with
  | Some pkg -> String.equal pkg pkg_name
  | None -> false
;;

(* Path.t versions - work with any path (source or build) *)

let scan_dune_file' ~pkg_name (path : Path.t) =
  if Path.Untracked.exists path && not (Path.Untracked.is_directory path)
  then (
    let contents = Io.read_file ~binary:true path in
    match
      Dune_sexp.Parser.parse_string ~fname:(Path.to_string path) ~mode:Many contents
    with
    | exception _ -> []
    | sexps ->
      List.filter_map sexps ~f:(fun sexp ->
        match extract_public_name_and_package_from_sexp sexp with
        | Some (public_name, explicit_package) ->
          if library_belongs_to_package ~pkg_name (public_name, explicit_package)
          then Some public_name
          else None
        | None -> None))
  else []
;;

let rec scan_dir_for_dune_files' ~pkg_name (dir : Path.t) =
  if not (Path.Untracked.exists dir)
  then []
  else (
    let dune_file = Path.relative dir "dune" in
    let libs = scan_dune_file' ~pkg_name dune_file in
    match Path.Untracked.readdir_unsorted_with_kinds dir with
    | Error _ -> libs
    | Ok entries ->
      let subdirs =
        List.filter_map entries ~f:(fun (entry, kind) ->
          match kind with
          | Unix.S_DIR -> Some (Path.relative dir entry)
          | _ -> None)
      in
      let subdir_libs = List.concat_map subdirs ~f:(scan_dir_for_dune_files' ~pkg_name) in
      libs @ subdir_libs)
;;

let scan_meta_libraries' (dir : Path.t) ~pkg_name =
  let meta_file_in_pkg = Path.relative dir (pkg_name ^ "/META") in
  let meta_file = Path.relative dir "META" in
  if Path.Untracked.exists meta_file_in_pkg || Path.Untracked.exists meta_file
  then [ pkg_name ]
  else []
;;

let scan_opam_libraries' (dir : Path.t) =
  match Path.Untracked.readdir_unsorted dir with
  | Error _ -> []
  | Ok entries ->
    List.filter_map entries ~f:(fun entry ->
      if String.is_suffix entry ~suffix:".opam"
      then Some (Filename.remove_extension entry)
      else None)
;;

let scan_libraries' (dir : Path.t) ~pkg_name =
  match scan_dir_for_dune_files' ~pkg_name dir with
  | [] ->
    (match scan_meta_libraries' dir ~pkg_name with
     | [] -> scan_opam_libraries' dir
     | libs -> libs)
  | libs -> libs
;;

(* Path.Source.t versions - wrappers for backward compatibility *)

let scan_public_libraries ~pkg_name dir =
  scan_dir_for_dune_files' ~pkg_name (Path.source dir)
;;

let scan_meta_libraries dir ~pkg_name = scan_meta_libraries' (Path.source dir) ~pkg_name
let scan_opam_libraries dir = scan_opam_libraries' (Path.source dir)

(* Find opam file in a directory. Checks both "opam" and "<name>.opam".
   We check that it exists and is not a directory to avoid matching opam/ directories. *)
let find_opam_file ~pkg_name ~pkg_dir =
  let candidates =
    [ Path.Source.relative pkg_dir "opam"
    ; Path.Source.relative pkg_dir (pkg_name ^ ".opam")
    ]
  in
  List.find candidates ~f:(fun p ->
    let full_path = Path.source p in
    Path.Untracked.exists full_path && not (Path.Untracked.is_directory full_path))
;;

let scan_libraries dir ~pkg_name = scan_libraries' (Path.source dir) ~pkg_name

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

(* Parse "name.version" strings into components. These are the canonical helpers
   for parsing opam-style package directory names. *)
let parse_name_version dir_name =
  match OpamPackage.of_string_opt dir_name with
  | Some pkg ->
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let version = OpamPackage.Version.to_string (OpamPackage.version pkg) in
    Some (name, version)
  | None -> None
;;

let parse_pkg_name_from_dir dir_name =
  match parse_name_version dir_name with
  | Some (name, _) -> name
  | None -> dir_name
;;

(* Lib-cache for looking up library→package→directory mappings.
   Format: lib_name:pkg_name:dirname
   - lib_name: the library name (e.g., "cmdliner")
   - pkg_name: the package name (e.g., "cmdliner")
   - dirname: the directory name in duniverse (e.g., "cmdliner.1.2.0") *)

type lib_cache_entry =
  { lib_name : string
  ; pkg_name : string
  ; dirname : string
  }

let lib_cache_file =
  lazy
    (let pkg_dir = Path.build Dpath.Build.pkgs_dir in
     Path.relative pkg_dir "lib-cache")
;;

(* Internal cache indexed by lib_name for fast lookup *)
let lib_cache : lib_cache_entry String.Table.t option ref = ref None

let load_lib_cache () =
  let cache_file = Lazy.force lib_cache_file in
  if Path.Untracked.exists cache_file
  then (
    let lines = Io.lines_of_file cache_file in
    let tbl = String.Table.create 256 in
    List.iter lines ~f:(fun line ->
      match String.lsplit2 line ~on:':' with
      | Some (lib_name, rest) ->
        (match String.lsplit2 rest ~on:':' with
         | Some (pkg_name, dirname) ->
           String.Table.set tbl lib_name { lib_name; pkg_name; dirname }
         | None -> ())
      | None -> ());
    Some tbl)
  else None
;;

(* Find the package that provides a given library.
   Uses the lib-cache file written by dune pkg fetch. *)
let find_pkg_for_library lib_name =
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
  | Some tbl ->
    (match String.Table.find tbl lib_name with
     | Some entry -> Some entry.pkg_name
     | None -> None)
;;

(* Find the directory that contains a given library.
   Uses the lib-cache file written by dune pkg fetch. *)
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
  | Some tbl ->
    (match String.Table.find tbl lib_name with
     | Some entry -> Some entry.dirname
     | None -> None)
;;

let invalidate_lib_cache () = lib_cache := None

(* Get all entries from lib-cache as (lib_name, pkg_name) pairs *)
let lib_cache_entries () =
  let cache =
    match !lib_cache with
    | Some c -> Some c
    | None ->
      let c = load_lib_cache () in
      lib_cache := c;
      c
  in
  match cache with
  | None -> []
  | Some tbl ->
    String.Table.to_list tbl |> List.map ~f:(fun (lib, entry) -> lib, entry.pkg_name)
;;

(* Return the path to the lib-cache file as a build path *)
let lib_cache_path () = Path.Build.relative Dpath.Build.pkgs_dir "lib-cache"

(* Write lib-cache entries to file. *)
let write_lib_cache entries =
  let cache_path = Path.build (lib_cache_path ()) in
  let parent = Path.parent_exn cache_path in
  Path.mkdir_p parent;
  let lines =
    List.map entries ~f:(fun { lib_name; pkg_name; dirname } ->
      sprintf "%s:%s:%s" lib_name pkg_name dirname)
  in
  Io.write_lines cache_path lines;
  invalidate_lib_cache ()
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

  (* Legacy cookie path inside target_dir - kept for backward compatibility *)
  let make_install_cookie target_dir ~relative = relative target_dir "cookie"

  let install_cookie' target_dir =
    make_install_cookie target_dir ~relative:Path.Build.relative
  ;;

  let install_cookie t = make_install_cookie t.target_dir ~relative:Path.relative
  let target_dir t = t.target_dir
  let source_dir t = t.source_dir
  let install_paths t = Lazy.force t.install_paths
end

(* Path to a package's build directory: _build/.pkgs/<ctx>/<name>/
   Note: Path uses only package name (no version) to enable cleanup on upgrade.
   Exposed for use by callers that need to compute marker paths. *)
let pkg_build_dir ~context ~pkg_name =
  let ctx_dir =
    Path.Build.relative Dpath.Build.pkgs_dir (Context_name.to_string context)
  in
  Path.Build.relative ctx_dir (Package.Name.to_string pkg_name)
;;
